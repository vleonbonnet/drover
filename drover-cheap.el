;;; drover-cheap.el --- The cheap model of this host's OpenCode catalog  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; Author: Valentin Leon <valentin@leon.click>
;; Keywords: tools, llm, convenience

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

;; `drover-model-cheap' tells the model small requests of this host
;; should run, such as the summaries of `drover-summary', and the
;; models to try after it: the cheap models the providers of this
;; host's OpenCode service offer, as the user prefers them, chosen from
;; the service's catalogs alone.  `drover-cheap-generate' has them
;; write what a prompt asks, trying the next when one is unavailable.
;;
;; Every model of a tier of `drover-cheap-families' a usable provider
;; offers is ranked.  A model is current when its version, read from
;; its identifier (`drover-cheap-versions'), is the newest the usable
;; providers offer of its family, outdated otherwise: outdated models
;; come after every current one.  Then come providers in the order of
;; the classes this kind of host uses (`drover-cheap-host-kind',
;; `drover-cheap-provider-orders'), a provider belonging to the first
;; class of that order it matches (`drover-cheap-provider-classes'):
;; by its identifier, or by how the service is signed in to it, a
;; subscription, a key, or a plugin's own credentials.  Then the tier,
;; then, within a tier, the newer version and the model released last,
;; the edition not premium and the cheaper.  The chain tried is the
;; best model of each provider, in that order, since what makes a model
;; unavailable, a quota, a login, an outage, is mostly its provider's.
;;
;; A provider the service is not signed in to, one excluded
;; (`drover-cheap-excluded-providers'), a provider disabled, a model
;; excluded (`drover-cheap-excluded-models', spot capacity by default)
;; and a model disabled or deprecated are never chosen.
;;
;; The resolution is kept until a catalog changes; how the service is
;; signed in to each provider comes from its integrations
;; (/api/integration), read again as they change.  Each host resolves
;; its own: its catalogs, and its kind, which its configuration sets.
;;
;; `drover-cheap-title-mode' has the local OpenCode service title its
;; sessions with the cheap model, naming it for the title agent in a
;; configuration file of the service's own (`drover-cheap-title-file').

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'drover-core)

;; Reloaded into an Emacs running an older drover, load its core anew.
(unless (and (boundp 'drover-core-generation) (>= drover-core-generation 18))
  (load "drover-core" nil t))

(declare-function opencode-call-with-service "opencode-common" (service function &rest arguments))
(declare-function opencode-service-remote-p "opencode-common" (&optional service))
(declare-function opencode-catalog "opencode-session" (kind &optional directory))
(declare-function opencode-catalog-cached-p "opencode-session" (kind &optional directory))
(declare-function opencode-catalog-fetch "opencode-session" (kind callback &optional directory else))
(declare-function opencode-events-register "opencode-events" (type function))
(declare-function opencode-events-unregister "opencode-events" (type function))
(declare-function opencode-request "opencode-http" (method path &rest arguments))
(declare-function opencode-request-data "opencode-http" (response))
(declare-function opencode-error-data "opencode-http" (error))
(declare-function opencode-error-describe "opencode-http" (error))
(declare-function opencode-error-status "opencode-http" (error))
(declare-function opencode-error-tag "opencode-http" (error))
(declare-function opencode-service-read-registration "opencode-service" ())
(defvar opencode-catalog-generation)

(defgroup drover-cheap nil
  "The cheap model of this host, for small requests such as summaries."
  :group 'drover
  :prefix "drover-cheap-")

;;;; Options

(defcustom drover-cheap-host-kind 'personal
  "The kind of this host, choosing the providers it may use and their order.
A key of `drover-cheap-provider-orders': `mongodb' for a host of
MongoDB's, which uses MongoDB's AI providers alone, `personal' for one
of the user's own, which prefers what costs least.  Set it per host,
in its configuration: it is not guessed."
  :type '(choice (const :tag "A MongoDB host: MongoDB's AI providers alone" mongodb)
                 (const :tag "A personal host: subscriptions, MongoDB's AI, then APIs" personal)
                 (symbol :tag "Another key of `drover-cheap-provider-orders'"))
  :group 'drover-cheap)

(defcustom drover-cheap-provider-classes
  '((github-copilot :id "\\`github-copilot\\'")
    (grove-bedrock :id "\\`ai-gateway-\\(?:anthropic\\|openai\\|misc\\)\\'")
    (fireworks :id "\\`fireworks-ai\\'")
    (prodsec :id "\\`prodsec-gateway\\'")
    (mongodb-ai :id "\\`\\(?:ai-gateway-\\(?:anthropic\\|openai\\|misc\\)\\|prodsec-gateway\\)\\'")
    (subscription :auth (subscription))
    (api :auth (api plugin)))
  "Classes of providers, which `drover-cheap-provider-orders' orders.
Each entry is (CLASS . CONDITIONS), CONDITIONS a plist of:

 :id    A regexp the provider's identifier matches.
 :auth  The ways the service may be signed in to it, a list of
        `subscription', a login to a subscription such as ChatGPT's,
        Claude's or Copilot's, `api', a key the service holds, and
        `plugin', a provider a plugin or the configuration defines,
        without an integration of the service, its credentials its own.

A provider is of CLASS when it meets every condition given.  The
defaults name MongoDB's providers, as the Grove plugin and the
MongoDB relay name them: the Grove gateway's providers by wire format
\(`grove-bedrock'), Fireworks, the ProdSec gateway, and the Grove
gateway's and the ProdSec gateway's as the relay serves them to
another host (`mongodb-ai')."
  :type '(alist :key-type (symbol :tag "Class")
                :value-type (plist :options ((:id regexp)
                                             (:auth (repeat (choice (const subscription) (const api)
                                                                    (const plugin)))))))
  :group 'drover-cheap)

(defcustom drover-cheap-provider-orders
  '((mongodb github-copilot grove-bedrock fireworks prodsec)
    (personal subscription mongodb-ai api))
  "The classes of providers each kind of host uses, in its order of preference.
Each entry is (KIND . CLASSES), KIND a value of
`drover-cheap-host-kind', CLASSES keys of `drover-cheap-provider-classes'.
A provider belongs to the first of CLASSES it is of; one of none is
not used.  Among current models (see `drover-cheap-versions'), those
of the first class come first, the best its providers offer (see
`drover-cheap-families'), then those of the next."
  :type '(alist :key-type (symbol :tag "Kind of host") :value-type (repeat (symbol :tag "Class")))
  :group 'drover-cheap)

(defcustom drover-cheap-excluded-providers '("\\`secateur-gateway-")
  "Regexps of the identifiers of providers never chosen.
The defaults exclude the copies of the Grove gateway's providers billed
to Secateur, which a session chooses explicitly."
  :type '(repeat regexp)
  :group 'drover-cheap)

(defcustom drover-cheap-families
  '((haiku :family "\\`claude-haiku\\'" :id "claude-haiku")
    (luna :family "\\`gpt-luna\\'" :id "\\`gpt-[0-9.]+-luna\\(?:-\\|\\'\\)")
    (open-weight :family "\\`\\(?:deepseek\\|glm\\)-flash\\'" :id "\\(?:deepseek\\|glm\\)[-0-9a-z.]*-flash"))
  "Tiers of cheap models, best first.
Each entry is (TIER :family REGEXP :id REGEXP): a model is of TIER when
the family the catalog gives it matches :family, or, for a model of no
family, when its identifier matches :id, both downcased.  A tier may
take several families, as `open-weight' takes DeepSeek's and GLM's
flash models.  Within a tier, the newer version of a family comes
first, then the model released last.  The defaults are Anthropic's
Haiku, OpenAI's Luna, then the flash models of DeepSeek and GLM.  A
tier ranks models of the same currency and class of provider: see
`drover-cheap-versions' and `drover-cheap-provider-orders'."
  :type '(alist :key-type (symbol :tag "Tier")
                :value-type (plist :options ((:family regexp) (:id regexp))))
  :group 'drover-cheap)

(defcustom drover-cheap-versions
  '(("claude-haiku" . "claude-haiku-\\([0-9]+\\)\\(?:[.-]\\([0-9]\\{1,2\\}\\)\\)?\\(?:[^0-9]\\|\\'\\)\\|claude-\\([0-9]+\\)\\(?:[.-]\\([0-9]\\{1,2\\}\\)\\)?-haiku")
    ("gpt-luna" . "gpt-\\([0-9]+\\)\\(?:\\.\\([0-9]+\\)\\)?-luna")
    ("deepseek-flash" . "deepseek-v\\([0-9]+\\)\\(?:[.p]\\([0-9]+\\)\\)?\\(?:[^0-9]\\|\\'\\)")
    ("glm-flash" . "glm-\\([0-9]+\\)\\(?:[.p]\\([0-9]+\\)\\)?\\(?:[^0-9]\\|\\'\\)"))
  "How the identifier of a model of each family tells its version.
Each entry is (FAMILY . REGEXP), FAMILY a family as catalogs name them,
REGEXP matched against the last part of a model's identifier, after
its last slash, downcased: the numbers its groups match, in order, are
the version, compared as `version-list-<' does, such as 5.5 for
claude-haiku-5-5 and claude-haiku-5.5-20261007, 4.1 for
deepseek-v4p1-flash-1017.  A model of no family is of the first FAMILY
whose REGEXP its identifier matches.

The current version of a family is the newest the usable providers of
this host offer, whatever the dates catalogs tell, which differ from
one provider to another, or are missing: its models are current, those
of an older version outdated, and so is one whose identifier tells no
version, such as a router to the latest, when another tells one.
Outdated models come after every current one, whatever their provider."
  :type '(alist :key-type (string :tag "Family") :value-type regexp)
  :group 'drover-cheap)

(defcustom drover-cheap-premium-regexp "-\\(?:fast\\|ultrafast\\|priority\\)\\'"
  "Regexp of the identifiers of a model's premium editions, or nil.
Such as gpt-6-luna-fast beside gpt-6-luna: of two models released the
same day, the premium edition comes after."
  :type '(choice (const :tag "None" nil) regexp)
  :group 'drover-cheap)

(defcustom drover-cheap-excluded-models '("-spot\\(?:-\\|\\'\\)")
  "Regexps of the identifiers of models never chosen, downcased.
The defaults exclude the editions run on spot capacity, such as
Fireworks' deepseek-v4p1-flash-1017-spot, which the provider may take
back at any time."
  :type '(repeat regexp)
  :group 'drover-cheap)

(defcustom drover-cheap-attempts 4
  "How many models `drover-cheap-generate' asks at most, one after another.
The next is asked only when one was unavailable (see
`drover-cheap-fallback-p')."
  :type 'natnum
  :group 'drover-cheap)

(defcustom drover-cheap-variant "low"
  "Variant `drover-cheap-generate' runs a model at, when it offers it, or nil.
Such as low, the least effort.  A model without it runs its default."
  :type '(choice (const :tag "The model's default" nil) string)
  :group 'drover-cheap)

(defcustom drover-cheap-request-timeout 60
  "Seconds `drover-cheap-generate' waits for a model's answer."
  :type 'number
  :group 'drover-cheap)

(defcustom drover-cheap-timeout 10
  "Seconds `drover-model-cheap' waits for the catalogs, called interactively."
  :type 'number
  :group 'drover-cheap)

(defcustom drover-cheap-title-file nil
  "File where `drover-cheap-title-mode' names the model of OpenCode's titles.
Nil for opencode.json in the global configuration directory of this
host's OpenCode service, as the service finds it: $OPENCODE_CONFIG_DIR,
else opencode under $XDG_CONFIG_HOME or ~/.config.  The service reads
that file with opencode.jsonc, whose settings win, and reloads it as it
changes.  The mode writes the file, and deletes it, only while it holds
nothing but what the mode writes."
  :type '(choice (const :tag "The service's global opencode.json" nil) file)
  :group 'drover-cheap)

;;;; How the service is signed in to providers

(defun drover-cheap-integration-auths (integrations)
  "Return how the service is signed in to each of INTEGRATIONS, an alist.
INTEGRATIONS are the data of /api/integration; the alist maps the
identifier of each integration connected to `subscription', for a
login (OAuth), or `api', for a key or the environment."
  (let ((auths nil))
    (dolist (integration (and (listp integrations) integrations))
      (let ((id (and (consp integration) (alist-get 'id integration)))
            (connections (and (consp integration) (alist-get 'connections integration))))
        (when (and (stringp id) (consp connections))
          (push (cons id (if (seq-some (lambda (connection)
                                         (and (consp connection)
                                              (equal (alist-get 'method connection) "oauth")))
                                       connections)
                             'subscription
                           'api))
                auths))))
    (nreverse auths)))

(defun drover-cheap-provider-auth (provider auths)
  "Return how the service is signed in to PROVIDER, of its catalog, or nil.
AUTHS is the alist of `drover-cheap-integration-auths', or `unknown'
for a service telling no integrations.  It is `subscription' or `api'
as the provider's integration is connected; `api' for a key its
settings hold; `plugin' for a provider of no integration, which a
plugin or the configuration defines; nil for an integration not
connected, which the service cannot use.  With AUTHS `unknown', a
provider of an integration counts as `api'."
  (let* ((integration (alist-get 'integrationID provider))
         (auth (and (stringp integration) (listp auths)
                    (alist-get integration auths nil nil #'equal))))
    (cond
     (auth auth)
     ((alist-get 'apiKey (alist-get 'settings provider)) 'api)
     ((not (stringp integration)) 'plugin)
     ((eq auths 'unknown) 'api))))

(defun drover-cheap--excluded-p (provider-id)
  "Return non-nil when PROVIDER-ID is never chosen."
  (seq-some (lambda (regexp) (string-match-p regexp provider-id)) drover-cheap-excluded-providers))

(defun drover-cheap--class-p (conditions provider-id auth)
  "Return non-nil when PROVIDER-ID, signed in as AUTH, meets CONDITIONS."
  (let ((id (plist-get conditions :id))
        (auths (plist-get conditions :auth)))
    (and (or id auths)
         (or (null id) (string-match-p id provider-id))
         (or (null auths) (memq auth auths)))))

(defun drover-cheap--order (&optional kind)
  "Return the classes of providers hosts of KIND use, by default this one's."
  (cdr (assq (or kind drover-cheap-host-kind) drover-cheap-provider-orders)))

(defun drover-cheap-provider-class (provider-id auth &optional kind)
  "Return the class of the provider PROVIDER-ID, signed in as AUTH, or nil.
It is the first class of the order of hosts of KIND, by default this
one's, that the provider is of (see `drover-cheap-provider-orders');
nil for a provider excluded, not signed in, or of no class of that
order, which is not used."
  (and (stringp provider-id) auth
       (not (drover-cheap--excluded-p provider-id))
       (seq-find (lambda (class)
                   (drover-cheap--class-p (cdr (assq class drover-cheap-provider-classes)) provider-id auth))
                 (drover-cheap--order kind))))

;;;; Choosing the model

(defun drover-cheap--tier (model)
  "Return the index of the tier of MODEL, of the model catalog, or nil."
  (let ((family (alist-get 'family model))
        (id (alist-get 'id model))
        (case-fold-search t))
    (cl-position-if (lambda (tier)
                      (let ((family-regexp (plist-get (cdr tier) :family))
                            (id-regexp (plist-get (cdr tier) :id)))
                        (if (and (stringp family) (not (string-empty-p family)))
                            (and family-regexp (string-match-p family-regexp (downcase family)))
                          (and id-regexp (stringp id) (string-match-p id-regexp (downcase id))))))
                    drover-cheap-families)))

(defun drover-cheap--excluded-model-p (id)
  "Return non-nil when the model ID is never chosen.
See `drover-cheap-excluded-models'."
  (let ((id (downcase id)))
    (seq-some (lambda (regexp) (string-match-p regexp id)) drover-cheap-excluded-models)))

(defun drover-cheap--usable-p (model)
  "Return non-nil when MODEL, an entry of the model catalog, may be chosen.
Neither disabled, deprecated nor excluded."
  (and (stringp (alist-get 'id model))
       (stringp (alist-get 'providerID model))
       (not (memq (alist-get 'enabled model t) '(nil :false)))
       (not (equal (alist-get 'status model) "deprecated"))
       (not (drover-cheap--excluded-model-p (alist-get 'id model)))))

(defun drover-cheap--released (model)
  "Return when MODEL was released, in milliseconds, 0 when unknown."
  (let ((released (alist-get 'released (alist-get 'time model))))
    (if (numberp released) released 0)))

(defun drover-cheap--cost (model)
  "Return what a million tokens MODEL reads cost, 0 when unknown.
A subscription's models tell no cost."
  (let* ((cost (alist-get 'cost model))
         (first (if (and (consp cost) (consp (car cost)) (consp (caar cost))) (car cost) cost))
         (input (and (consp first) (alist-get 'input first))))
    (if (numberp input) input 0)))

(defun drover-cheap--premium-p (model)
  "Return non-nil when MODEL is a premium edition.
See `drover-cheap-premium-regexp'."
  (and drover-cheap-premium-regexp
       (string-match-p drover-cheap-premium-regexp (alist-get 'id model))))

(defun drover-cheap--base-name (model)
  "Return the identifier of MODEL after its last slash, downcased."
  (car (last (split-string (downcase (alist-get 'id model)) "/"))))

(defun drover-cheap--family (model)
  "Return the family of MODEL, downcased, or nil.
The family the catalog gives it, else the first of
`drover-cheap-versions' its identifier matches."
  (let ((family (alist-get 'family model)))
    (if (and (stringp family) (not (string-empty-p family)))
        (downcase family)
      (let ((name (drover-cheap--base-name model)))
        (car (seq-find (lambda (entry) (string-match-p (cdr entry) name)) drover-cheap-versions))))))

(defun drover-cheap--version (model family)
  "Return the version of MODEL, of FAMILY, a list of numbers, or nil.
Its identifier tells it, as `drover-cheap-versions' reads it."
  (when-let* ((regexp (cdr (assoc family drover-cheap-versions)))
              (name (drover-cheap--base-name model))
              ((string-match regexp name)))
    (let ((version nil))
      (dotimes (group (/ (length (match-data)) 2))
        (when-let* (((> group 0))
                     (number (match-string group name)))
          (push (string-to-number number) version)))
      (nreverse version))))

(defun drover-cheap--newer-p (a b)
  "Return non-nil when version A is newer than version B, either nil."
  (and a (or (null b) (version-list-< b a))))

(defun drover-cheap--better-p (a b)
  "Return non-nil when candidate A comes before candidate B.
Each is a plist of `drover-cheap--candidates'.  The current before the
outdated, then the provider of the class first in the host's order,
then the better tier, then the newer version of a family, then the
model released last, then the edition not premium, then the cheaper,
then the provider listed first, then the shorter identifier, then the
first in order."
  (let ((ma (plist-get a :model)) (mb (plist-get b :model)))
    (cl-flet ((differ (key) (not (equal (plist-get a key) (plist-get b key)))))
      (cond
       ((differ :outdated) (not (plist-get a :outdated)))
       ((differ :rank) (< (plist-get a :rank) (plist-get b :rank)))
       ((differ :tier) (< (plist-get a :tier) (plist-get b :tier)))
       ((and (equal (plist-get a :family) (plist-get b :family))
             (not (version-list-= (or (plist-get a :version) '(-1)) (or (plist-get b :version) '(-1)))))
        (drover-cheap--newer-p (plist-get a :version) (plist-get b :version)))
       ((differ :released) (> (plist-get a :released) (plist-get b :released)))
       ((not (eq (not (drover-cheap--premium-p ma)) (not (drover-cheap--premium-p mb))))
        (not (drover-cheap--premium-p ma)))
       ((/= (drover-cheap--cost ma) (drover-cheap--cost mb))
        (< (drover-cheap--cost ma) (drover-cheap--cost mb)))
       ((differ :position) (< (plist-get a :position) (plist-get b :position)))
       ((/= (length (alist-get 'id ma)) (length (alist-get 'id mb)))
        (< (length (alist-get 'id ma)) (length (alist-get 'id mb))))
       (t (string< (alist-get 'id ma) (alist-get 'id mb)))))))

(defun drover-cheap--release-dates (models)
  "Return when each model of MODELS was released, by name, a table.
A provider may list a model without its date, which another lists
with it: the table maps the downcased name of every model dated to its
latest date, in milliseconds."
  (let ((dates (make-hash-table :test #'equal)))
    (dolist (model models)
      (when-let* (((consp model))
                   (name (alist-get 'name model))
                   ((stringp name))
                   (released (drover-cheap--released model))
                   ((> released 0)))
        (puthash (downcase name) (max released (gethash (downcase name) dates 0)) dates)))
    dates))

(defun drover-cheap--released-of (model dates)
  "Return when MODEL was released, its own date, else that DATES give its name.
0 when neither tells.  DATES is a table of `drover-cheap--release-dates'."
  (let ((released (drover-cheap--released model))
        (name (alist-get 'name model)))
    (if (or (> released 0) (not (stringp name)))
        released
      (gethash (downcase name) dates 0))))

(defun drover-cheap--candidates (models providers auths kind)
  "Return the models of MODELS a host of KIND may choose, as plists.
PROVIDERS and AUTHS are as `drover-cheap-rank' takes them.  Each is
\(:model MODEL :provider PROVIDER :class CLASS :rank RANK :auth AUTH
:tier TIER :position POSITION :released RELEASED :family FAMILY
:version VERSION :outdated OUTDATED): the catalog's entries of the
model and of its provider, the class of the provider and its index in
KIND's order, how the service is signed in to it, the index of the
model's tier, the provider's position among those usable, when the
model was released, in milliseconds, 0 when unknown, its family, its
version, a list of numbers or nil, and whether it is outdated (see
`drover-cheap-versions')."
  (let ((entries (make-hash-table :test #'equal))
        (order (drover-cheap--order kind))
        (position 0)
        (dates (drover-cheap--release-dates models))
        (newest (make-hash-table :test #'equal))
        (candidates nil))
    (dolist (provider providers)
      (let ((id (and (consp provider) (alist-get 'id provider))))
        (when (and (stringp id) (not (equal (alist-get 'activation provider) "disabled"))
                   (not (gethash id entries)))
          (let* ((auth (drover-cheap-provider-auth provider auths))
                 (class (drover-cheap-provider-class id auth kind)))
            (when class
              (puthash id (list class auth provider (cl-incf position)) entries))))))
    (dolist (model models)
      (when-let* (((consp model))
                  ((drover-cheap--usable-p model))
                  (entry (gethash (alist-get 'providerID model) entries))
                  (tier (drover-cheap--tier model)))
        (let* ((family (drover-cheap--family model))
               (version (drover-cheap--version model family)))
          (when (and family (drover-cheap--newer-p version (gethash family newest)))
            (puthash family version newest))
          (push (list :model model :provider (nth 2 entry) :class (car entry)
                      :rank (cl-position (car entry) order) :auth (nth 1 entry)
                      :tier tier :position (nth 3 entry)
                      :released (drover-cheap--released-of model dates)
                      :family family :version version)
                candidates))))
    (dolist (candidate candidates)
      (let ((newest (gethash (plist-get candidate :family) newest)))
        (nconc candidate (list :outdated (and newest (drover-cheap--newer-p newest (plist-get candidate :version))
                                              t)))))
    (nreverse candidates)))

(defun drover-cheap--describe (candidate)
  "Return the plist describing CANDIDATE, of `drover-cheap--candidates'."
  (let* ((model (plist-get candidate :model))
         (provider-id (alist-get 'providerID model))
         (id (alist-get 'id model))
         (released (plist-get candidate :released))
         (version (plist-get candidate :version)))
    (list :model (concat provider-id "/" id)
          :provider-id provider-id
          :provider-name (alist-get 'name (plist-get candidate :provider))
          :id id
          :name (alist-get 'name model)
          :family (alist-get 'family model)
          :tier (car (nth (plist-get candidate :tier) drover-cheap-families))
          :class (plist-get candidate :class)
          :auth (plist-get candidate :auth)
          :released (and (> released 0) released)
          :version (and version (mapconcat #'number-to-string version "."))
          :outdated (plist-get candidate :outdated)
          :variants (delq nil (mapcar (lambda (variant)
                                        (let ((name (and (consp variant) (alist-get 'id variant))))
                                          (and (stringp name) name)))
                                      (alist-get 'variants model))))))

(defun drover-cheap-rank (models providers auths &optional kind)
  "Return every cheap model of MODELS a host of KIND may choose, best first.
MODELS and PROVIDERS are the service's model and provider catalogs,
AUTHS how it is signed in to its integrations (see
`drover-cheap-provider-auth'); KIND defaults to
`drover-cheap-host-kind'.  A model may be chosen when its provider is
of a class of KIND's order (see `drover-cheap-provider-orders') and
it is of a tier of `drover-cheap-families'.  The current come first
\(see `drover-cheap-versions'), then the provider of the class first in
KIND's order, then the tier; see `drover-cheap--better-p'.  A model
listed without its date released takes the date the catalog gives its
name elsewhere.  Each model is a plist: :model, \"PROVIDER/ID\";
:provider-id, :provider-name, :id, :name, :family, :tier, :class,
:auth, :released, in milliseconds or nil, :version, the version its
identifier tells, as \"5.5\", or nil, :outdated, non-nil when a newer
version of its family is offered, and :variants, the names of its
variants."
  (mapcar #'drover-cheap--describe
          (sort (drover-cheap--candidates models providers auths (or kind drover-cheap-host-kind))
                :lessp #'drover-cheap--better-p)))

(defun drover-cheap-chain (models providers auths &optional kind)
  "Return the models a host of KIND tries in turn, the best of each provider.
The models of MODELS as `drover-cheap-rank' ranks them, from PROVIDERS
and AUTHS, as it takes them, a provider's best alone: what makes a
model unavailable, a quota, a login, an outage, is mostly its
provider's, so that its other models would fail as well."
  (let ((seen nil))
    (seq-filter (lambda (model)
                  (unless (member (plist-get model :provider-id) seen)
                    (push (plist-get model :provider-id) seen)))
                (drover-cheap-rank models providers auths kind))))

(defun drover-cheap-resolve (models providers auths &optional kind)
  "Return the cheap model of MODELS a host of KIND would choose, or nil.
The first of `drover-cheap-chain' of MODELS, PROVIDERS and AUTHS,
with :fallbacks, the models of the chain after it, each described as
the first is (see `drover-cheap-rank')."
  (when-let* ((chain (drover-cheap-chain models providers auths kind)))
    (append (car chain) (list :fallbacks (cdr chain)))))

(defun drover-cheap-none-reason (&optional kind)
  "Return why no cheap model resolves for a host of KIND, a line."
  (let ((kind (or kind drover-cheap-host-kind)))
    (if (drover-cheap--order kind)
        (format "no provider of %s (%s host) offers a model of %s"
                (mapconcat #'symbol-name (drover-cheap--order kind) ", ")
                kind
                (mapconcat (lambda (tier) (symbol-name (car tier))) drover-cheap-families ", "))
      (format "`drover-cheap-provider-orders' has no order for a %s host" kind))))

;;;; This host's

(defvar drover-cheap--auths nil
  "How the local service is signed in to its integrations, as last read, or nil.
\(EPOCH . AUTHS): AUTHS as `drover-cheap-provider-auth' takes them, read
as of EPOCH, a value of `drover-cheap--epoch'.")

(defvar drover-cheap--epoch 0
  "Count of the changes the local service told of its integrations.")

(defvar drover-cheap--waiters nil
  "Callbacks waiting for the integrations being read, newest first.")

(defvar drover-cheap--resolved nil
  "The cheap model last resolved, (KEY . ANSWER), or nil.
KEY is what it was resolved from (see `drover-cheap--key'), ANSWER the
plist of `drover-cheap-resolve', or (:none REASON).")

(defvar drover-cheap--following nil
  "Non-nil once the changes of the local service's integrations are followed.")

(defun drover-cheap--directory ()
  "Return the directory whose catalogs are read: the user's home."
  (file-name-as-directory (expand-file-name "~")))

(defun drover-cheap--local (function &rest arguments)
  "Call FUNCTION with ARGUMENTS for the local OpenCode service."
  (if (fboundp 'opencode-call-with-service)
      (apply #'opencode-call-with-service nil function arguments)
    (apply function arguments)))

(defun drover-cheap--remote-p ()
  "Return non-nil when the running code works for another OpenCode service."
  (and (fboundp 'opencode-service-remote-p) (opencode-service-remote-p)))

(defun drover-cheap-service-running-p ()
  "Return non-nil when this host's OpenCode service runs, as registered.
Nothing here ever starts it."
  (require 'opencode-service)
  (when-let* ((registration (opencode-service-read-registration))
              (pid (plist-get registration :pid)))
    (and (natnump pid) (process-attributes pid) t)))

(defun drover-cheap--integrations-changed (&rest _)
  "Note that the local service's integrations may have changed.
For opencode.el's events and `opencode-catalog-changed-functions'."
  (drover-with-reporting "cheap model"
    (unless (drover-cheap--remote-p)
      (cl-incf drover-cheap--epoch)
      (when (bound-and-true-p drover-cheap-title-mode)
        (drover-cheap--title-schedule)))))

(defun drover-cheap--follow ()
  "Follow the changes of the local service's integrations, once."
  (unless drover-cheap--following
    (setq drover-cheap--following t)
    (require 'opencode-events)
    (opencode-events-register "integration.updated" #'drover-cheap--integrations-changed)
    (opencode-events-register "server.connected" #'drover-cheap--integrations-changed)
    (add-hook 'opencode-catalog-changed-functions #'drover-cheap--integrations-changed)))

(defun drover-cheap--settle-auths (failure)
  "Call the callbacks waiting for the integrations, with FAILURE or nil."
  (let ((waiters (nreverse drover-cheap--waiters)))
    (setq drover-cheap--waiters nil)
    (dolist (waiter waiters)
      (drover-with-reporting "cheap model"
        (funcall waiter failure)))))

(defun drover-cheap--fetch-auths (then)
  "Call THEN once the local service's integrations are read.
THEN gets nil, or a failure, a line.  They are read again when the
service told of a change since."
  (if (and drover-cheap--auths (eql (car drover-cheap--auths) drover-cheap--epoch))
      (funcall then nil)
    (push then drover-cheap--waiters)
    (unless (cdr drover-cheap--waiters)
      (drover-cheap--follow)
      (require 'opencode-http)
      (let ((epoch drover-cheap--epoch))
        (condition-case err
            (opencode-request
             'get "/api/integration" :directory nil :buffer nil
             :then (lambda (response)
                     (drover-with-reporting "cheap model"
                       (setq drover-cheap--auths
                             (cons epoch (drover-cheap-integration-auths (opencode-request-data response))))
                       (drover-cheap--settle-auths nil)))
             :else (lambda (error)
                     (drover-with-reporting "cheap model"
                       ;; A service telling no integrations: a route missing, not an error.
                       (if (and (eql (opencode-error-status error) 404) (not (opencode-error-tag error)))
                           (progn (setq drover-cheap--auths (cons epoch 'unknown))
                                  (drover-cheap--settle-auths nil))
                         (drover-cheap--settle-auths (opencode-error-describe error))))))
          (error (drover-cheap--settle-auths (drover-error-message err))))))))

(defun drover-cheap--key ()
  "Return what this host's cheap model is resolved from, compared with `equal'."
  (list (bound-and-true-p opencode-catalog-generation) (car drover-cheap--auths)
        drover-cheap-host-kind drover-cheap-provider-classes drover-cheap-provider-orders
        drover-cheap-excluded-providers drover-cheap-excluded-models drover-cheap-families
        drover-cheap-versions drover-cheap-premium-regexp))

(defun drover-cheap--ready-p ()
  "Return non-nil when the local service's catalogs and integrations are read."
  (let ((directory (drover-cheap--directory)))
    (and drover-cheap--auths
         (fboundp 'opencode-catalog-cached-p)
         (drover-cheap--local #'opencode-catalog-cached-p 'model directory)
         (drover-cheap--local #'opencode-catalog-cached-p 'provider directory))))

(defun drover-cheap--answer ()
  "Return this host's cheap model, from what was read, or (:none REASON).
It is resolved again only once a catalog, the integrations or the
options changed."
  (let ((key (drover-cheap--key)))
    (unless (equal (car drover-cheap--resolved) key)
      (let* ((directory (drover-cheap--directory))
             (models (drover-cheap--local #'opencode-catalog 'model directory))
             (providers (drover-cheap--local #'opencode-catalog 'provider directory)))
        (setq drover-cheap--resolved
              (cons key (or (drover-cheap-resolve models providers (cdr drover-cheap--auths))
                            (list :none (drover-cheap-none-reason)))))))
    (cdr drover-cheap--resolved)))

(defun drover-model-cheap-fetch (callback)
  "Call CALLBACK once with this host's cheap model, reading what it needs.
CALLBACK gets the plist of `drover-cheap-resolve', (:none REASON) when
no model resolves, or (:failure MESSAGE) when the local service does
not run or its catalogs could not be read.  The catalogs read are the
local service's, through opencode.el's cache; it is never started.
CALLBACK may be called before this returns."
  (if (not (drover-cheap-service-running-p))
      (funcall callback (list :failure "OpenCode's service does not run"))
    (require 'opencode-session)
    (let ((pending 3)
          (failure nil)
          (directory (drover-cheap--directory)))
      (cl-flet ((done (&optional message)
                  (when (and message (not failure)) (setq failure message))
                  (when (zerop (cl-decf pending))
                    (drover-with-reporting "cheap model"
                      (funcall callback (if failure
                                            (list :failure failure)
                                          (drover-cheap--answer)))))))
        (drover-cheap--local
         (lambda ()
           (dolist (kind '(model provider))
             (condition-case err
                 (opencode-catalog-fetch kind (lambda (_) (done)) directory
                                         (lambda (error) (done (opencode-error-describe error))))
               (error (done (drover-error-message err)))))
           (drover-cheap--fetch-auths #'done)))))))

;;;###autoload
(defvar drover-cheap--signed-in-waiters nil
  "Callbacks of `drover-cheap-signed-in' waiting for a read, newest first.
t alone while a read is under way that none waits for.")

(defun drover-cheap-signed-in (provider-id &optional then)
  "Return how this host's OpenCode service is signed in to PROVIDER-ID.
`subscription' for a login, such as ChatGPT's or Claude's, `api' for a
key, `plugin' for a provider of no integration, as
`drover-cheap-provider-auth' tells it, nil for a provider the service
does not know or cannot use.  `unknown' while the provider catalog and
the integrations are not read, as of their last change: they are read
in the background, the local service running, and THEN, if any, is
called without argument once they are, so that what depended on the
answer is made anew.  A read under way is not started again."
  (if (and (drover-cheap--ready-p) (eql (car drover-cheap--auths) drover-cheap--epoch))
      (when-let* ((provider (seq-find (lambda (provider)
                                        (and (consp provider) (equal (alist-get 'id provider) provider-id)))
                                      (drover-cheap--local #'opencode-catalog 'provider
                                                           (drover-cheap--directory)))))
        (drover-cheap-provider-auth provider (cdr drover-cheap--auths)))
    (if drover-cheap--signed-in-waiters
        (when then
          (setq drover-cheap--signed-in-waiters
                (cons then (and (listp drover-cheap--signed-in-waiters) drover-cheap--signed-in-waiters))))
      (when (drover-cheap-service-running-p)
        (setq drover-cheap--signed-in-waiters (if then (list then) t))
        (condition-case err
            (drover-model-cheap-fetch
             (lambda (result)
               (let ((waiters (and (listp drover-cheap--signed-in-waiters)
                                   (reverse drover-cheap--signed-in-waiters))))
                 (setq drover-cheap--signed-in-waiters nil)
                 (unless (plist-get result :failure)
                   (dolist (waiter waiters)
                     (drover-with-reporting "OpenCode's logins"
                       (funcall waiter)))))))
          (error (setq drover-cheap--signed-in-waiters nil)
                 (signal (car err) (cdr err))))))
    'unknown))

;;;###autoload
(defun drover-model-cheap (&optional interactive)
  "Return the cheap model of this host, or nil when none resolves or is known.
The model is the plist of `drover-cheap-resolve', as the local OpenCode
service's catalogs and the providers it is signed in to give it, for a
host of `drover-cheap-host-kind'; resolved again once they change.
While what it needs is not read, nil, and it is read in the background,
the local service running.  Interactively, or with INTERACTIVE
non-nil, wait for it, `drover-cheap-timeout' seconds at most, and tell
the model, or why there is none."
  (interactive "p")
  (if (not interactive)
      (progn
        (unless (and (drover-cheap--ready-p) (eql (car drover-cheap--auths) drover-cheap--epoch))
          (when (drover-cheap-service-running-p)
            (drover-model-cheap-fetch #'ignore)))
        (when (drover-cheap--ready-p)
          (let ((answer (drover-cheap--answer)))
            (unless (plist-get answer :none) answer))))
    (let ((answer :pending)
          (deadline (+ (float-time) drover-cheap-timeout)))
      (drover-model-cheap-fetch (lambda (result) (setq answer result)))
      (while (and (eq answer :pending) (< (float-time) deadline))
        (accept-process-output nil 0.05))
      (cond
       ((eq answer :pending)
        (drover-message "The catalogs of OpenCode's service did not come in %s s" drover-cheap-timeout)
        nil)
       ((plist-get answer :failure)
        (drover-message "No cheap model known: %s" (plist-get answer :failure))
        nil)
       ((plist-get answer :none)
        (drover-message "No cheap model on this %s host: %s" drover-cheap-host-kind (plist-get answer :none))
        nil)
       (t
        (drover-message "Cheap model of this %s host: %s (%s), %s, %s%s%s"
                        drover-cheap-host-kind
                        (plist-get answer :model) (or (plist-get answer :name) (plist-get answer :id))
                        (or (plist-get answer :provider-name) (plist-get answer :provider-id))
                        (plist-get answer :class)
                        (if-let* ((released (plist-get answer :released)))
                            (format-time-string ", released %F" (/ released 1000.0))
                          "")
                        (if-let* ((fallbacks (plist-get answer :fallbacks)))
                            (format "; then %s"
                                    (mapconcat (lambda (model) (plist-get model :model))
                                               (seq-take fallbacks (max 0 (1- drover-cheap-attempts)))
                                               ", "))
                          ""))
        answer)))))

;;;; Asking it

(defvar drover-cheap--former-path nil
  "Non-nil once the service knew stateless generation by its former path alone.
Services before 2.2 answer POST /api/experimental/generate, which later
ones keep as an alias of /api/experimental/generate/text.")

(defun drover-cheap-fold-system (prompt system)
  "Return PROMPT under the instructions SYSTEM, as one prompt.
OpenCode's stateless generation takes a prompt alone, no system
prompt: SYSTEM, when neither nil nor blank, comes first, then a blank
line and PROMPT."
  (if (and (stringp system) (not (string-blank-p system)))
      (concat (string-trim system) "\n\n" prompt)
    prompt))

(defun drover-cheap--named (model)
  "Return MODEL, \"PROVIDER/ID\", as a plist of `drover-cheap-rank', or nil.
Its variants are not known: a variant asked is sent as it is."
  (let ((slash (and (stringp model) (string-search "/" model))))
    (and slash (> slash 0) (< (1+ slash) (length model))
         (list :model model :provider-id (substring model 0 slash) :id (substring model (1+ slash))
               :variants t))))

(defun drover-cheap--ref (model variant)
  "Return MODEL, a plist of `drover-cheap-rank', as OpenCode's model reference.
It names VARIANT when MODEL offers it."
  `((providerID . ,(plist-get model :provider-id))
    (id . ,(plist-get model :id))
    ,@(and variant (or (eq (plist-get model :variants) t) (member variant (plist-get model :variants)))
           `((variant . ,variant)))))

(defun drover-cheap-fallback-p (error)
  "Return non-nil when ERROR, of a generation, leaves another model to ask.
ERROR is an `opencode-error'.  A model or a provider unavailable, out
of quota or not signed in, a model or provider the service does not
know or cannot run, the service unable to reach the provider, no
answer in time: another model may answer.  Content a provider's filter
refused, a request a provider or the service found invalid, the
service refusing the request: any model would fail alike.  A service
before 2.2 tells a model it cannot select as an invalid request (see
`drover-cheap--former-path'), and an opencode.el not keeping an
error's object tells no provider error's reason, taken then for an
unavailable provider."
  (let* ((data (and (fboundp 'opencode-error-data) (opencode-error-data error)))
         (data (and (consp data) data))
         (status (opencode-error-status error)))
    (pcase (opencode-error-tag error)
      ("ProviderRequestError"
       (not (member (alist-get 'reason data) '("content_filter" "invalid_request"))))
      ((or "ModelNotFoundError" "ProviderNotFoundError" "UnsupportedModelError" "ServiceUnavailableError") t)
      ("InvalidRequestError"
       (let ((field (alist-get 'field data)))
         (if (stringp field) (string-prefix-p "model" field) drover-cheap--former-path)))
      ((pred stringp) nil)
      (_ (or (null status) (>= status 500))))))

;;;###autoload
(cl-defun drover-cheap-generate (prompt system callback
                                        &key model (variant drover-cheap-variant)
                                        (timeout drover-cheap-request-timeout)
                                        (attempts drover-cheap-attempts))
  "Have this host's cheap model write what PROMPT asks, then call CALLBACK.
SYSTEM, a string or nil, gives the instructions, folded into the
prompt (see `drover-cheap-fold-system').  It is a stateless generation
of the local OpenCode service (POST /api/experimental/generate/text, or
its former path for a service without it): it makes no session, which
neither lists nor usage totals of sessions show.

The models asked are this host's cheap model, then the others of its
chain (see `drover-cheap-chain'), ATTEMPTS at most, by default
`drover-cheap-attempts': the next only when one was unavailable (see
`drover-cheap-fallback-p').  MODEL, \"PROVIDER/ID\", names the one model
to ask instead.  Each runs at VARIANT, by default
`drover-cheap-variant', when it offers it, and is waited for TIMEOUT
seconds, by default `drover-cheap-request-timeout'.

CALLBACK is called once, later or at once, with (:text TEXT :model
MODEL :name NAME), what the model MODEL, \"PROVIDER/ID\", named NAME,
wrote, or with (:failure MESSAGE), adding :none t when no cheap model
resolves on this host.  Return a function cancelling the request:
CALLBACK is then never called, and no other model asked.  Nothing here
waits, nor starts the service."
  (require 'opencode-http)
  (let ((done nil)
        (failures nil)
        (text (drover-cheap-fold-system prompt system)))
    (cl-labels
        ((finish (result)
           (unless done
             (setq done t)
             (funcall callback result)))
         (give-up ()
           (finish (list :failure (string-join (reverse failures) "; "))))
         (note (model message)
           (push (drover-bound-line (format "%s: %s" (plist-get model :model) message)) failures))
         (answered (model response)
           (let ((answer (alist-get 'text (opencode-request-data response))))
             (if (and (stringp answer) (not (string-blank-p answer)))
                 (finish (list :text (drover-bound-text answer 400 40000)
                               :model (plist-get model :model) :name (plist-get model :name)))
               (note model "OpenCode answered no text")
               (give-up))))
         (failed (chain error)
           (if (and (not drover-cheap--former-path)
                    (eql (opencode-error-status error) 404)
                    (not (opencode-error-tag error)))
               ;; No such route, rather than no such model.
               (progn (setq drover-cheap--former-path t)
                      (ask chain))
             (note (car chain) (opencode-error-describe error))
             (if (and (cdr chain) (drover-cheap-fallback-p error))
                 (ask (cdr chain))
               (give-up))))
         (ask (chain)
           (unless done
             (let ((model (car chain)))
               (condition-case err
                   (opencode-call-with-service
                    nil #'opencode-request 'post
                    (if drover-cheap--former-path "/api/experimental/generate" "/api/experimental/generate/text")
                    :body `((prompt . ,text) (model . ,(drover-cheap--ref model variant)))
                    :directory nil :buffer nil :timeout timeout
                    :then (lambda (response)
                            (drover-with-reporting "cheap model"
                              (unless done (answered model response))))
                    :else (lambda (error)
                            (drover-with-reporting "cheap model"
                              (unless done (failed chain error)))))
                 (error (note model (drover-error-message err))
                        (give-up)))))))
      (cond
       ((not (drover-cheap-service-running-p))
        (finish (list :failure "OpenCode's service does not run")))
       (model
        (if-let* ((named (drover-cheap--named model)))
            (ask (list named))
          (finish (list :failure (format "Not a model, PROVIDER/ID: %s" (drover-bound-line (format "%s" model)))))))
       (t
        (drover-model-cheap-fetch
         (lambda (answer)
           (cond
            (done)
            ((plist-get answer :failure) (finish (list :failure (plist-get answer :failure))))
            ((plist-get answer :none)
             (finish (list :failure (concat "no cheap model, " (plist-get answer :none)) :none t)))
            (t (ask (seq-take (cons answer (plist-get answer :fallbacks)) (max 1 attempts)))))))))
      (lambda () (setq done t)))))

;;;; OpenCode's titles

(defconst drover-cheap--title-schema "https://opencode.ai/config.json"
  "The schema the file of `drover-cheap-title-mode' names.")

(defvar drover-cheap--title-timer nil
  "Timer of the next update of the model of OpenCode's titles, or nil.")

(defvar drover-cheap--title-foreign nil
  "The file of `drover-cheap-title-mode' found holding more, once told.")

(defun drover-cheap--env (name)
  "Return the environment variable NAME, nil when unset or empty."
  (let ((value (getenv name)))
    (and value (not (string-empty-p value)) value)))

(defun drover-cheap-title-file ()
  "Return the file naming the model of OpenCode's titles.
See the option `drover-cheap-title-file'."
  (expand-file-name
   (or drover-cheap-title-file
       (expand-file-name "opencode.json"
                         (or (drover-cheap--env "OPENCODE_CONFIG_DIR")
                             (expand-file-name "opencode" (or (drover-cheap--env "XDG_CONFIG_HOME")
                                                              "~/.config")))))))

(defun drover-cheap-title-config (model)
  "Return the configuration having OpenCode title sessions with MODEL.
MODEL is a plist of `drover-cheap-resolve', run at `drover-cheap-variant'
when it offers it.  The configuration, an alist for `json-serialize',
sets the model of the title agent alone, by provider and identifier,
since an identifier may hold a slash."
  (let ((ref (drover-cheap--ref model drover-cheap-variant)))
    `((,(intern "$schema") . ,drover-cheap--title-schema)
      (agents (title (model (providerID . ,(alist-get 'providerID ref))
                            (model . ,(alist-get 'id ref))
                            ,@(and (alist-get 'variant ref)
                                   `((variant . ,(alist-get 'variant ref))))))))))

(defun drover-cheap--title-own-p (text)
  "Return non-nil when TEXT holds only what `drover-cheap-title-mode' writes.
That is the schema and the model of the title agent, or nothing."
  (or (string-blank-p text)
      (condition-case nil
          (let ((data (json-parse-string text :object-type 'alist)))
            (and (consp data)
                 (null (seq-difference (mapcar #'car data) (list (intern "$schema") 'agents)))
                 (let ((agents (alist-get 'agents data)))
                   (and (consp agents)
                        (equal (mapcar #'car agents) '(title))
                        (let ((title (alist-get 'title agents)))
                          (and (consp title) (equal (mapcar #'car title) '(model))))))))
        (json-parse-error nil))))

(defun drover-cheap--title-write (file text)
  "Replace FILE with TEXT at once, OpenCode never reading half of it."
  (make-directory (file-name-directory file) t)
  (let* ((coding-system-for-write 'utf-8-unix)
         ;; Not opencode.json: OpenCode reads the name alone.
         (temporary (make-temp-file (expand-file-name ".drover-title-" (file-name-directory file))
                                    nil ".tmp" text)))
    (condition-case err
        (progn
          ;; As a file written in place would be, not private.
          (set-file-modes temporary (logand #o666 (default-file-modes)))
          (rename-file temporary file t))
      (error (delete-file temporary)
             (signal (car err) (cdr err))))))

(defun drover-cheap-title-apply (answer)
  "Have OpenCode title sessions with ANSWER, of `drover-model-cheap-fetch'.
A model is written to the file of `drover-cheap-title-file', when it
changed; no model deletes the file, OpenCode then titling sessions as
it does by default.  A failure to read the catalogs leaves the file as
it is, as does a file holding more than what is written here, which is
said once."
  (unless (plist-get answer :failure)
    (let* ((file (drover-cheap-title-file))
           (text (and (not (plist-get answer :none))
                      (concat (json-serialize (drover-cheap-title-config answer)) "\n")))
           (current (and (file-readable-p file)
                         (with-temp-buffer
                           (let ((coding-system-for-read 'utf-8))
                             (insert-file-contents file))
                           (buffer-string)))))
      (cond
       ((equal text current))
       ((and current (not (drover-cheap--title-own-p current)))
        (unless (equal drover-cheap--title-foreign file)
          (setq drover-cheap--title-foreign file)
          (drover-warn "%s holds more than the model of OpenCode's titles: left as it is"
                       (abbreviate-file-name file))))
       (text (drover-cheap--title-write file text))
       (current (delete-file file))))))

(defun drover-cheap--title-update ()
  "Name this host's cheap model for OpenCode's titles, reading what it needs."
  (setq drover-cheap--title-timer nil)
  (drover-with-reporting "OpenCode's title model"
    (when (bound-and-true-p drover-cheap-title-mode)
      (drover-model-cheap-fetch
       (lambda (answer)
         (drover-with-reporting "OpenCode's title model"
           (when drover-cheap-title-mode
             (drover-cheap-title-apply answer))))))))

(defun drover-cheap--title-schedule ()
  "Update the model of OpenCode's titles soon, once for changes coming together."
  (when (timerp drover-cheap--title-timer)
    (cancel-timer drover-cheap--title-timer))
  (setq drover-cheap--title-timer (run-with-timer 1 nil #'drover-cheap--title-update)))

;;;###autoload
(define-minor-mode drover-cheap-title-mode
  "Have OpenCode title sessions with this host's cheap model.
OpenCode names a session with its title agent, from its first prompt
and as asked later from the conversation, on a small model of the
session's provider it picks, or the free one GitHub Copilot offers for
it.  This mode names the model of the title agent instead, in the file
of `drover-cheap-title-file': this host's cheap model (see
`drover-model-cheap'), at `drover-cheap-variant', named again as the
service's catalogs and integrations change it.  When it fails,
OpenCode asks the session's own model.  Turning the mode off deletes
the file, OpenCode then titling sessions as it does by default.

Only the local service reads the file; each host names its own."
  :global t
  :group 'drover-cheap
  (when (timerp drover-cheap--title-timer)
    (cancel-timer drover-cheap--title-timer)
    (setq drover-cheap--title-timer nil))
  (if drover-cheap-title-mode
      (progn
        (drover-cheap--follow)
        (drover-cheap--title-schedule))
    (drover-cheap-title-apply (list :none "off"))))

(defun drover-cheap-unload-function ()
  "Stop following the integrations of the local service."
  (when (timerp drover-cheap--title-timer)
    (cancel-timer drover-cheap--title-timer))
  (when drover-cheap--following
    (opencode-events-unregister "integration.updated" #'drover-cheap--integrations-changed)
    (opencode-events-unregister "server.connected" #'drover-cheap--integrations-changed)
    (remove-hook 'opencode-catalog-changed-functions #'drover-cheap--integrations-changed))
  nil)

(provide 'drover-cheap)
;;; drover-cheap.el ends here
