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
;; should run, such as the summaries of `drover-summary': the best
;; cheap model the providers of this host's OpenCode service offer, as
;; the user prefers them, chosen from the service's catalogs alone.
;;
;; Providers are taken in the order of the classes this kind of host
;; uses (`drover-cheap-host-kind', `drover-cheap-provider-orders'), a
;; provider belonging to the first class of that order it matches
;; (`drover-cheap-provider-classes'): by its identifier, or by how the
;; service is signed in to it, a subscription, a key, or a plugin's own
;; credentials.  The first class with a provider offering a cheap model
;; gives the model: the first tier of `drover-cheap-families' its
;; providers offer, and in that tier the model released last.  A
;; provider the service is not signed in to, one excluded
;; (`drover-cheap-excluded-providers'), a provider disabled and a model
;; disabled or deprecated are never chosen.
;;
;; The resolution is kept until a catalog changes; how the service is
;; signed in to each provider comes from its integrations
;; (/api/integration), read again as they change.  Each host resolves
;; its own: its catalogs, and its kind, which its configuration sets.

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
    (grove-azure :id "\\`grove-")
    (fireworks :id "\\`fireworks-ai\\'")
    (prodsec :id "\\`prodsec-gateway\\'")
    (mongodb-ai :id "\\`\\(?:ai-gateway-\\(?:anthropic\\|openai\\|misc\\)\\|grove-.+\\|prodsec-gateway\\)\\'")
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
\(`grove-bedrock'), those of the legacy Grove gateway on Azure, if any
\(`grove-azure'), Fireworks, the ProdSec gateway, and all of those
but Copilot and Fireworks as the relay serves them to another host
\(`mongodb-ai')."
  :type '(alist :key-type (symbol :tag "Class")
                :value-type (plist :options ((:id regexp)
                                             (:auth (repeat (choice (const subscription) (const api)
                                                                    (const plugin)))))))
  :group 'drover-cheap)

(defcustom drover-cheap-provider-orders
  '((mongodb github-copilot grove-bedrock grove-azure fireworks prodsec)
    (personal subscription mongodb-ai api))
  "The classes of providers each kind of host uses, in its order of preference.
Each entry is (KIND . CLASSES), KIND a value of
`drover-cheap-host-kind', CLASSES keys of `drover-cheap-provider-classes'.
A provider belongs to the first of CLASSES it is of; one of none is
not used.  The first class with a provider offering a cheap model gives
the model, the best its providers offer (see `drover-cheap-families')."
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
flash models, the one released last winning.  The defaults are
Anthropic's Haiku, OpenAI's Luna, then the flash models of DeepSeek
and GLM."
  :type '(alist :key-type (symbol :tag "Tier")
                :value-type (plist :options ((:family regexp) (:id regexp))))
  :group 'drover-cheap)

(defcustom drover-cheap-premium-regexp "-\\(?:fast\\|ultrafast\\|priority\\)\\'"
  "Regexp of the identifiers of a model's premium editions, or nil.
Such as gpt-6-luna-fast beside gpt-6-luna: of two models released the
same day, the premium edition comes after."
  :type '(choice (const :tag "None" nil) regexp)
  :group 'drover-cheap)

(defcustom drover-cheap-timeout 10
  "Seconds `drover-model-cheap' waits for the catalogs, called interactively."
  :type 'number
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

(defun drover-cheap--usable-p (model)
  "Return non-nil when MODEL, an entry of the model catalog, may be chosen.
Neither disabled nor deprecated."
  (and (stringp (alist-get 'id model))
       (stringp (alist-get 'providerID model))
       (not (memq (alist-get 'enabled model t) '(nil :false)))
       (not (equal (alist-get 'status model) "deprecated"))))

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

(defun drover-cheap--better-p (a b)
  "Return non-nil when candidate A comes before candidate B.
Each is (TIER POSITION RELEASED . MODEL).  The better tier first, then
the model released last, then the edition not premium, then the
cheaper, then the provider listed first, then the shorter identifier,
then the first in order."
  (let ((ma (cdddr a)) (mb (cdddr b)))
    (cl-flet ((differ (x y) (not (equal x y))))
      (cond
       ((differ (car a) (car b)) (< (car a) (car b)))
       ((differ (nth 2 a) (nth 2 b)) (> (nth 2 a) (nth 2 b)))
       ((differ (drover-cheap--premium-p ma) (drover-cheap--premium-p mb))
        (not (drover-cheap--premium-p ma)))
       ((differ (drover-cheap--cost ma) (drover-cheap--cost mb))
        (< (drover-cheap--cost ma) (drover-cheap--cost mb)))
       ((differ (cadr a) (cadr b)) (< (cadr a) (cadr b)))
       ((differ (length (alist-get 'id ma)) (length (alist-get 'id mb)))
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

(defun drover-cheap--describe (model provider tier class auth released)
  "Return the plist describing MODEL of PROVIDER, of TIER and CLASS.
The service is signed in to PROVIDER as AUTH; MODEL was RELEASED, in
milliseconds, 0 when unknown."
  (let ((provider-id (alist-get 'providerID model))
        (id (alist-get 'id model)))
    (list :model (concat provider-id "/" id)
          :provider-id provider-id
          :provider-name (alist-get 'name provider)
          :id id
          :name (alist-get 'name model)
          :family (alist-get 'family model)
          :tier tier
          :class class
          :auth auth
          :released (and (> released 0) released)
          :variants (delq nil (mapcar (lambda (variant)
                                        (let ((name (and (consp variant) (alist-get 'id variant))))
                                          (and (stringp name) name)))
                                      (alist-get 'variants model))))))

(defun drover-cheap-resolve (models providers auths &optional kind)
  "Return the cheap model of MODELS a host of KIND would choose, or nil.
MODELS and PROVIDERS are the service's model and provider catalogs,
AUTHS how it is signed in to its integrations (see
`drover-cheap-provider-auth'); KIND defaults to
`drover-cheap-host-kind'.  The classes of KIND's order are taken in
turn; the first with a provider offering a model of a tier of
`drover-cheap-families' gives the best of those (see
`drover-cheap--better-p'), a model listed without its date released
taking the date the catalog gives its name elsewhere.  The model is a
plist: :model, \"PROVIDER/ID\"; :provider-id, :provider-name, :id,
:name, :family, :tier, :class, :auth, :released, in milliseconds or
nil, and :variants, the names of its variants."
  (let ((classes (make-hash-table :test #'equal))
        (positions (make-hash-table :test #'equal))
        (position 0))
    (dolist (provider providers)
      (let ((id (and (consp provider) (alist-get 'id provider))))
        (when (and (stringp id) (not (equal (alist-get 'activation provider) "disabled")))
          (let* ((auth (drover-cheap-provider-auth provider auths))
                 (class (drover-cheap-provider-class id auth kind)))
            (when class
              (puthash id (list class auth provider) classes)
              (puthash id (cl-incf position) positions))))))
    (let ((candidates nil)
          (dates (drover-cheap--release-dates models)))
      (dolist (model models)
        (when-let* (((consp model))
                    ((drover-cheap--usable-p model))
                    (entry (gethash (alist-get 'providerID model) classes))
                    (tier (drover-cheap--tier model)))
          (push (cons (car entry) (cl-list* tier (gethash (alist-get 'providerID model) positions)
                                            (drover-cheap--released-of model dates) model))
                candidates)))
      (catch 'chosen
        (dolist (class (drover-cheap--order kind))
          (when-let* ((offered (mapcar #'cdr (seq-filter (lambda (candidate) (eq (car candidate) class))
                                                          candidates))))
            (let* ((best (car (sort offered :lessp #'drover-cheap--better-p)))
                   (model (cdddr best))
                   (entry (gethash (alist-get 'providerID model) classes)))
              (throw 'chosen (drover-cheap--describe model (nth 2 entry)
                                                     (car (nth (car best) drover-cheap-families))
                                                     class (nth 1 entry) (nth 2 best))))))))))

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
      (cl-incf drover-cheap--epoch))))

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
        drover-cheap-excluded-providers drover-cheap-families drover-cheap-premium-regexp))

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
        (drover-message "Cheap model of this %s host: %s (%s), %s, %s%s"
                 drover-cheap-host-kind
                 (plist-get answer :model) (or (plist-get answer :name) (plist-get answer :id))
                 (or (plist-get answer :provider-name) (plist-get answer :provider-id))
                 (plist-get answer :class)
                 (if-let* ((released (plist-get answer :released)))
                     (format-time-string ", released %F" (/ released 1000.0))
                   ""))
        answer)))))

(defun drover-cheap-unload-function ()
  "Stop following the integrations of the local service."
  (when drover-cheap--following
    (opencode-events-unregister "integration.updated" #'drover-cheap--integrations-changed)
    (opencode-events-unregister "server.connected" #'drover-cheap--integrations-changed)
    (remove-hook 'opencode-catalog-changed-functions #'drover-cheap--integrations-changed))
  nil)

(provide 'drover-cheap)
;;; drover-cheap.el ends here
