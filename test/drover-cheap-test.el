;;; drover-cheap-test.el --- Tests of the cheap model of a host  -*- lexical-binding: t; -*-

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

;; Catalogs as OpenCode's service tells them, trimmed to what the
;; choice reads: those of a MongoDB host, Reaver, and of a personal
;; one, Core, which reaches MongoDB's providers through a relay.  The
;; service is stubbed: no request leaves the batch Emacs.

;;; Code:

(require 'drover-test-helpers)
(require 'drover-cheap)
(require 'opencode-http)
(require 'opencode-session)

(defun drover-cheap-test-date (date)
  "Return DATE, YYYY-MM-DD, in milliseconds, as catalogs tell releases."
  (* 1000 (truncate (float-time (date-to-time (concat date "T00:00:00Z"))))))

(defun drover-cheap-test-model (provider id family date &rest properties)
  "Return the catalog entry of the model ID of PROVIDER, of FAMILY, released on DATE.
DATE nil leaves it undated, as the ProdSec gateway lists its models.
PROPERTIES are :name, :cost, :variants, :status and :enabled."
  `((id . ,id) (providerID . ,provider) (family . ,family)
    (name . ,(or (plist-get properties :name) id))
    (variants . ,(mapcar (lambda (variant) `((id . ,variant)))
                         (or (plist-get properties :variants) '("low" "high"))))
    (time . ((released . ,(if date (drover-cheap-test-date date) 0))))
    ,@(and (plist-member properties :cost)
           `((cost . ,(and (plist-get properties :cost) `(((input . ,(plist-get properties :cost))))))))
    (status . ,(or (plist-get properties :status) "active"))
    (enabled . ,(if (plist-member properties :enabled) (plist-get properties :enabled) t))))

(defun drover-cheap-test-provider (id &rest properties)
  "Return the catalog entry of the provider ID.
PROPERTIES are :integration, its integration's identifier, the
provider's own by default, :none for none; :key, non-nil when its
settings hold a key; :activation."
  (let ((integration (if (plist-member properties :integration) (plist-get properties :integration) id)))
    `((id . ,id) (integrationID . ,(unless (eq integration :none) integration)) (name . ,id)
      (activation . ,(or (plist-get properties :activation) "enabled"))
      (settings . ((baseURL . "https://example.invalid")
                   ,@(and (plist-get properties :key) '((apiKey . "placeholder"))))))))

(defconst drover-cheap-test-reaver-providers
  (list (drover-cheap-test-provider "opencode" :key t)
        (drover-cheap-test-provider "github-copilot")
        (drover-cheap-test-provider "fireworks-ai")
        (drover-cheap-test-provider "ai-gateway-openai" :integration "ai-gateway" :key t)
        (drover-cheap-test-provider "ai-gateway-anthropic" :integration "ai-gateway" :key t)
        (drover-cheap-test-provider "ai-gateway-misc" :integration "ai-gateway" :key t)
        (drover-cheap-test-provider "prodsec-gateway" :key t)
        (drover-cheap-test-provider "secateur-gateway-anthropic" :integration :none :key t)
        (drover-cheap-test-provider "secateur-gateway-openai" :integration :none :key t)
        (drover-cheap-test-provider "secateur-gateway-misc" :integration :none :key t))
  "The providers of Reaver's service, as it lists them.")

(defconst drover-cheap-test-reaver-models
  (list (drover-cheap-test-model "github-copilot" "claude-opus-5.5" "claude-opus" "2026-09-22" :cost 4)
        (drover-cheap-test-model "github-copilot" "claude-haiku-4.5" "claude-haiku" "2025-10-15"
                                 :name "Claude Haiku 4.5 (latest)" :cost 1 :variants '("high" "max"))
        (drover-cheap-test-model "github-copilot" "claude-haiku-5.5" "claude-haiku" "2026-10-07"
                                 :name "Claude Haiku 5.5" :cost 0.1
                                 :variants '("low" "medium" "high" "xhigh" "max"))
        (drover-cheap-test-model "github-copilot" "gpt-6-luna" "gpt-luna" "2026-09-22" :cost 0.1)
        (drover-cheap-test-model "github-copilot" "gpt-5.6-luna" "gpt-luna" "2026-07-09" :cost 0.2)
        (drover-cheap-test-model "github-copilot" "gemini-3.8-flash" "gemini-flash" "2026-09-02" :cost 0.75)
        (drover-cheap-test-model "ai-gateway-anthropic" "claude-haiku-4-5" "claude-haiku" "2025-11-18"
                                 :name "Claude Haiku 4.5" :cost 0.74 :variants nil)
        (drover-cheap-test-model "ai-gateway-openai" "gpt-5.6-luna" "gpt-luna" "2026-07-09" :cost 0.148)
        (drover-cheap-test-model "ai-gateway-openai" "gpt-6-luna" "gpt-luna" "2026-09-22" :cost 0.074)
        (drover-cheap-test-model "ai-gateway-openai" "gpt-5-nano" "gpt-nano" "2025-08-07" :cost 0.037)
        (drover-cheap-test-model "ai-gateway-misc" "fw-deepseek-v4.1-flash" "deepseek-flash" "2026-09-10" :cost 0.179)
        (drover-cheap-test-model "ai-gateway-misc" "fw-glm-5.3-flash" "glm-flash" "2026-08-26" :cost 0.122)
        (drover-cheap-test-model "ai-gateway-misc" "deepseek-v4-flash" "deepseek-flash" "2026-04-24" :cost 0.1406)
        (drover-cheap-test-model "ai-gateway-misc" "deepseek-v4-flash-0731" "deepseek-flash" "2026-04-24" :cost 0.3256)
        (drover-cheap-test-model "fireworks-ai" "accounts/fireworks/routers/deepseek-flash-latest"
                                 "deepseek-flash" "2026-10-17" :cost 0.3)
        (drover-cheap-test-model "fireworks-ai" "accounts/fireworks/models/deepseek-v4p1-flash-1017"
                                 "deepseek-flash" "2026-10-17" :cost 0.3)
        (drover-cheap-test-model "fireworks-ai" "accounts/fireworks/models/deepseek-v4p1-flash-1017-priority"
                                 "deepseek-flash" "2026-10-17" :cost 0.375)
        (drover-cheap-test-model "fireworks-ai" "accounts/fireworks/routers/deepseek-v4p1-flash-1017-spot"
                                 "deepseek-flash" "2026-10-17" :cost 0.21)
        (drover-cheap-test-model "fireworks-ai" "accounts/fireworks/models/deepseek-v4p1-flash"
                                 "deepseek-flash" "2026-09-10" :cost 0.3)
        (drover-cheap-test-model "fireworks-ai" "accounts/fireworks/routers/deepseek-v4p1-flash-spot"
                                 "deepseek-flash" "2026-09-10" :cost 0.21)
        (drover-cheap-test-model "fireworks-ai" "accounts/fireworks/models/glm-5p3-flash" "glm-flash" "2026-08-26" :cost 0.15)
        (drover-cheap-test-model "fireworks-ai" "accounts/fireworks/routers/glm-flash-latest" "glm-flash" "2026-08-26"
                                 :name "GLM Flash Latest (GLM 5.3 Flash)" :cost 0.15)
        (drover-cheap-test-model "fireworks-ai" "accounts/fireworks/models/minimax-m3p1-flash" "minimax" "2026-10-19" :cost 0.15)
        (drover-cheap-test-model "prodsec-gateway" "claude-haiku-5-5" "claude-haiku" nil
                                 :name "Claude Haiku 5.5" :cost 0.1)
        (drover-cheap-test-model "secateur-gateway-anthropic" "claude-haiku-4-5" "claude-haiku" "2025-10-15" :cost 1)
        (drover-cheap-test-model "secateur-gateway-openai" "gpt-6-luna" "gpt-luna" "2026-09-22" :cost 0.1))
  "The cheap models of Reaver's service and a few others, as it lists them.")

(defconst drover-cheap-test-reaver-integrations
  '(((id . "fireworks-ai") (connections . (((type . "credential") (label . "API key") (method . "key")))))
    ((id . "github-copilot") (connections . (((type . "credential") (label . "OAuth") (method . "oauth")))))
    ((id . "ai-gateway") (connections . nil))
    ((id . "prodsec-gateway") (connections . nil))
    ((id . "openai") (connections . nil)))
  "Reaver's integrations, as /api/integration tells them: Copilot and Fireworks connected.")

(defconst drover-cheap-test-core-providers
  (list (drover-cheap-test-provider "opencode" :key t)
        (drover-cheap-test-provider "openai" :activation "auto")
        (drover-cheap-test-provider "github-copilot")
        (drover-cheap-test-provider "fireworks-ai")
        (drover-cheap-test-provider "ai-gateway-openai" :integration :none :key t)
        (drover-cheap-test-provider "ai-gateway-anthropic" :integration :none :key t)
        (drover-cheap-test-provider "ai-gateway-misc" :integration :none :key t)
        (drover-cheap-test-provider "prodsec-gateway" :integration :none :key t)
        (drover-cheap-test-provider "secateur-gateway-anthropic" :integration :none :key t)
        (drover-cheap-test-provider "secateur-gateway-openai" :integration :none :key t))
  "The providers of Core's service: the relay's of no integration.")

(defconst drover-cheap-test-core-models
  (append
   (list (drover-cheap-test-model "openai" "gpt-6-luna-fast" "gpt-luna" "2026-09-21" :cost nil)
         (drover-cheap-test-model "openai" "gpt-6-luna" "gpt-luna" "2026-09-21" :cost nil
                                  :variants '("none" "low" "medium" "high" "xhigh" "max"))
         (drover-cheap-test-model "openai" "gpt-5.6-luna" "gpt-luna" "2026-07-08" :cost nil)
         (drover-cheap-test-model "openai" "gpt-6-sol" "gpt-sol" "2026-09-21" :cost nil)
         (drover-cheap-test-model "opencode" "deepseek-v4.1-flash-free" "deepseek-flash" "2026-09-09" :cost 0)
         (drover-cheap-test-model "opencode" "glm-5.3-flash-free" "glm-flash" "2026-08-25" :cost 0))
   (seq-remove (lambda (model) (equal (alist-get 'providerID model) "fireworks-ai"))
               drover-cheap-test-reaver-models))
  "The cheap models of Core's service, Copilot's and Fireworks' though not signed in.")

(defconst drover-cheap-test-core-integrations
  '(((id . "openai") (connections . (((type . "credential") (label . "OAuth") (method . "oauth")))))
    ((id . "github-copilot") (connections . nil))
    ((id . "fireworks-ai") (connections . nil)))
  "Core's integrations: ChatGPT's login alone.")

(defun drover-cheap-test-resolve (kind models providers integrations)
  "Return \"PROVIDER/ID CLASS\" a host of KIND resolves from its catalogs, or nil."
  (when-let* ((cheap (drover-cheap-resolve models providers
                                           (if (eq integrations 'unknown)
                                               'unknown
                                             (drover-cheap-integration-auths integrations))
                                           kind)))
    (format "%s %s" (plist-get cheap :model) (plist-get cheap :class))))

(defun drover-cheap-test-without (models &rest providers)
  "Return MODELS without those of PROVIDERS."
  (seq-remove (lambda (model) (member (alist-get 'providerID model) providers)) models))

;;;; How the service is signed in

(defun drover-cheap-test-signed-out (integration)
  "Return Reaver's integrations, INTEGRATION not connected."
  (mapcar (lambda (entry)
            (if (equal (alist-get 'id entry) integration) `((id . ,integration) (connections)) entry))
          drover-cheap-test-reaver-integrations))

(ert-deftest drover-cheap-test-integration-auths ()
  "A login is a subscription, a key or the environment an API; no connection, nothing."
  (should (equal (drover-cheap-integration-auths
                  (append drover-cheap-test-reaver-integrations
                          '(((id . "anthropic") (connections . (((method . "env")))))
                            ((id . "claude") (connections . (((method . "key")) ((method . "oauth")))))
                            "garbage" ((connections . (((method . "oauth"))))))))
                 '(("fireworks-ai" . api) ("github-copilot" . subscription)
                   ("anthropic" . api) ("claude" . subscription))))
  (should-not (drover-cheap-integration-auths nil))
  (should-not (drover-cheap-integration-auths 'garbage)))

(ert-deftest drover-cheap-test-provider-auth ()
  "A provider's integration tells how it is signed in, else its key, else a plugin's own."
  (let ((reaver (drover-cheap-integration-auths drover-cheap-test-reaver-integrations))
        (core (drover-cheap-integration-auths drover-cheap-test-core-integrations)))
    (dolist (case `(("github-copilot" ,reaver subscription)
                    ("fireworks-ai" ,reaver api)
                    ("ai-gateway-anthropic" ,reaver api)
                    ("prodsec-gateway" ,reaver api)
                    ("secateur-gateway-openai" ,reaver api)
                    ("openai" ,core subscription)
                    ("github-copilot" ,core nil)
                    ("fireworks-ai" ,core nil)
                    ("ai-gateway-anthropic" ,core api)
                    ("opencode" ,core api)
                    ("github-copilot" unknown api)))
      (let ((provider (or (seq-find (lambda (provider) (equal (alist-get 'id provider) (car case)))
                                    (if (eq (cadr case) core)
                                        drover-cheap-test-core-providers
                                      drover-cheap-test-reaver-providers))
                          (error "No provider %s" (car case)))))
        (should (equal (list (car case) (drover-cheap-provider-auth provider (cadr case)))
                       (list (car case) (nth 2 case)))))))
  (should (eq (drover-cheap-provider-auth '((id . "local") (settings . nil)) nil) 'plugin)))

;;;; A MongoDB host

(defun drover-cheap-test-chain (kind models providers integrations)
  "Return \"PROVIDER/ID\" of the chain a host of KIND resolves from its catalogs."
  (mapcar (lambda (model) (plist-get model :model))
          (drover-cheap-chain models providers (drover-cheap-integration-auths integrations) kind)))

(ert-deftest drover-cheap-test-mongodb-host ()
  "A MongoDB host takes the current first, then Copilot, the Grove gateway, Fireworks, the ProdSec gateway."
  (let ((models drover-cheap-test-reaver-models)
        (providers drover-cheap-test-reaver-providers)
        (integrations drover-cheap-test-reaver-integrations))
    (dolist (case
             `(;; Copilot's newest Haiku, not the one it calls latest.
               (,models "github-copilot/claude-haiku-5.5 github-copilot")
               ;; Copilot missing: the Grove gateway's Haiku 4.5 is outdated, the
               ;; ProdSec gateway offering 5.5; the gateway's Luna, current, first.
               (,(drover-cheap-test-without models "github-copilot")
                "ai-gateway-openai/gpt-6-luna grove-bedrock")
               ;; No Haiku 5.5 anywhere: the gateway's 4.5 is current, the better tier.
               (,(drover-cheap-test-without models "github-copilot" "prodsec-gateway")
                "ai-gateway-anthropic/claude-haiku-4-5 grove-bedrock")
               ;; No Luna there either: its newest open-weight flash, DeepSeek's over GLM's.
               (,(drover-cheap-test-without models "github-copilot" "ai-gateway-openai")
                "ai-gateway-misc/fw-deepseek-v4.1-flash grove-bedrock")
               ;; Fireworks' DeepSeek V4.1 of the latest snapshot: not its premium
               ;; edition, its router without a version nor its spot capacity.
               (,(drover-cheap-test-without models "github-copilot" "ai-gateway-openai" "ai-gateway-misc")
                "fireworks-ai/accounts/fireworks/models/deepseek-v4p1-flash-1017 fireworks")
               ;; Then the ProdSec gateway's Haiku, undated.
               (,(drover-cheap-test-without models "github-copilot" "ai-gateway-openai" "ai-gateway-misc"
                                            "fireworks-ai")
                "prodsec-gateway/claude-haiku-5-5 prodsec")
               ;; The gateway's Haiku 4.5 alone: current, nothing newer offered.
               (,(drover-cheap-test-without models "github-copilot" "ai-gateway-openai" "ai-gateway-misc"
                                            "fireworks-ai" "prodsec-gateway")
                "ai-gateway-anthropic/claude-haiku-4-5 grove-bedrock")
               ;; Secateur's copies are never chosen: no cheap model.
               (,(drover-cheap-test-without models "github-copilot" "ai-gateway-anthropic" "ai-gateway-openai"
                                            "ai-gateway-misc" "fireworks-ai" "prodsec-gateway")
                nil)))
      (should (equal (list (length (car case)) (drover-cheap-test-resolve 'mongodb (car case) providers integrations))
                     (list (length (car case)) (cadr case)))))
    ;; Copilot not signed in is skipped, as is a provider disabled.
    (should (equal (drover-cheap-test-resolve 'mongodb models providers
                                              (drover-cheap-test-signed-out "github-copilot"))
                   "ai-gateway-openai/gpt-6-luna grove-bedrock"))
    (should (equal (drover-cheap-test-resolve
                    'mongodb models
                    (mapcar (lambda (provider)
                              (if (equal (alist-get 'id provider) "github-copilot")
                                  (drover-cheap-test-provider "github-copilot" :activation "disabled")
                                provider))
                            drover-cheap-test-reaver-providers)
                    integrations)
                   "ai-gateway-openai/gpt-6-luna grove-bedrock"))
    ;; A provider no class of MongoDB's names is never used there, even its own
    ;; Haiku, whose version leaves the others current.
    (should (equal (drover-cheap-test-resolve
                    'mongodb
                    (cons (drover-cheap-test-model "anthropic" "claude-haiku-6" "claude-haiku" "2026-11-01")
                          (drover-cheap-test-without models "github-copilot" "prodsec-gateway"))
                    (cons (drover-cheap-test-provider "anthropic" :key t) providers)
                    integrations)
                   "ai-gateway-anthropic/claude-haiku-4-5 grove-bedrock"))
    ;; Nor is the legacy Grove gateway on Azure, of no class now.
    (should (equal (drover-cheap-test-resolve
                    'mongodb
                    (cons (drover-cheap-test-model "grove-openai" "gpt-6-luna" "gpt-luna" "2026-09-22")
                          (drover-cheap-test-without models "github-copilot" "ai-gateway-anthropic"
                                                     "ai-gateway-openai" "ai-gateway-misc"))
                    (cons (drover-cheap-test-provider "grove-openai" :integration :none :key t) providers)
                    integrations)
                   "fireworks-ai/accounts/fireworks/models/deepseek-v4p1-flash-1017 fireworks"))
    (should-not (assq 'grove-azure drover-cheap-provider-classes))
    (should-not (memq 'grove-azure (drover-cheap--order 'mongodb)))
    (should-not (drover-cheap-provider-class "grove-openai" 'api 'mongodb))
    (should (eq (drover-cheap-provider-class "grove-openai" 'plugin 'personal) 'api))))

(ert-deftest drover-cheap-test-mongodb-chain ()
  "Reaver tries Copilot's Haiku 5.5, the gateway's Luna 6 and DeepSeek V4.1, then Fireworks'."
  (should (equal (drover-cheap-test-chain 'mongodb drover-cheap-test-reaver-models drover-cheap-test-reaver-providers
                                          drover-cheap-test-reaver-integrations)
                 '("github-copilot/claude-haiku-5.5"
                   "ai-gateway-openai/gpt-6-luna"
                   "ai-gateway-misc/fw-deepseek-v4.1-flash"
                   "fireworks-ai/accounts/fireworks/models/deepseek-v4p1-flash-1017"
                   "prodsec-gateway/claude-haiku-5-5"
                   ;; Outdated, last.
                   "ai-gateway-anthropic/claude-haiku-4-5")))
  ;; Every model ranked: the current first, by class, then tier; spot capacity never.
  (let ((ranked (mapcar (lambda (model) (plist-get model :model))
                        (drover-cheap-rank drover-cheap-test-reaver-models drover-cheap-test-reaver-providers
                                           (drover-cheap-integration-auths drover-cheap-test-reaver-integrations)
                                           'mongodb))))
    (should (equal (seq-take ranked 9)
                   '("github-copilot/claude-haiku-5.5"
                     "github-copilot/gpt-6-luna"
                     "ai-gateway-openai/gpt-6-luna"
                     "ai-gateway-misc/fw-deepseek-v4.1-flash"
                     "ai-gateway-misc/fw-glm-5.3-flash"
                     "fireworks-ai/accounts/fireworks/models/deepseek-v4p1-flash-1017"
                     "fireworks-ai/accounts/fireworks/models/deepseek-v4p1-flash-1017-priority"
                     "fireworks-ai/accounts/fireworks/models/deepseek-v4p1-flash"
                     "fireworks-ai/accounts/fireworks/models/glm-5p3-flash")))
    (should (equal (nth 9 ranked) "prodsec-gateway/claude-haiku-5-5"))
    ;; The outdated after, by class too: Copilot's first, the newer version first.
    (should (equal (nthcdr 10 ranked)
                   '("github-copilot/claude-haiku-4.5"
                     "github-copilot/gpt-5.6-luna"
                     "ai-gateway-anthropic/claude-haiku-4-5"
                     "ai-gateway-openai/gpt-5.6-luna"
                     "ai-gateway-misc/deepseek-v4-flash"
                     "ai-gateway-misc/deepseek-v4-flash-0731"
                     "fireworks-ai/accounts/fireworks/routers/deepseek-flash-latest"
                     "fireworks-ai/accounts/fireworks/routers/glm-flash-latest")))
    (should-not (seq-some (lambda (id) (string-search "spot" id)) ranked))))

(ert-deftest drover-cheap-test-mongodb-host-described ()
  "The model chosen tells its provider, family, tier, class, version, release, variants and the models after it."
  (let ((cheap (drover-cheap-resolve drover-cheap-test-reaver-models drover-cheap-test-reaver-providers
                                     (drover-cheap-integration-auths drover-cheap-test-reaver-integrations)
                                     'mongodb)))
    (should (equal (seq-take cheap 18)
                   (list :model "github-copilot/claude-haiku-5.5" :provider-id "github-copilot"
                         :provider-name "github-copilot" :id "claude-haiku-5.5" :name "Claude Haiku 5.5"
                         :family "claude-haiku" :tier 'haiku :class 'github-copilot :auth 'subscription)))
    (should (equal (plist-get cheap :released) (drover-cheap-test-date "2026-10-07")))
    (should (equal (plist-get cheap :version) "5.5"))
    (should (plist-member cheap :outdated))
    (should-not (plist-get cheap :outdated))
    (should (equal (plist-get cheap :variants) '("low" "medium" "high" "xhigh" "max")))
    (should (equal (mapcar (lambda (model) (plist-get model :model)) (plist-get cheap :fallbacks))
                   (cdr (drover-cheap-test-chain 'mongodb drover-cheap-test-reaver-models
                                                 drover-cheap-test-reaver-providers
                                                 drover-cheap-test-reaver-integrations))))
    (let ((last (car (last (plist-get cheap :fallbacks)))))
      (should (equal (list (plist-get last :model) (plist-get last :version) (plist-get last :outdated))
                     '("ai-gateway-anthropic/claude-haiku-4-5" "4.5" t)))
      (should-not (plist-member last :fallbacks)))))

;;;; Versions

(ert-deftest drover-cheap-test-versions ()
  "A model's identifier tells its version; the newest the usable providers offer is current."
  (dolist (case '(("claude-haiku-5.5" "claude-haiku" (5 5))
                  ("claude-haiku-5-5" "claude-haiku" (5 5))
                  ("claude-haiku-4-5-20251001" "claude-haiku" (4 5))
                  ("claude-haiku-4-20250101" "claude-haiku" (4))
                  ("claude-3-5-haiku-20241022" "claude-haiku" (3 5))
                  ("anthropic.claude-haiku-5-5-v1:0" "claude-haiku" (5 5))
                  ("gpt-6-luna" "gpt-luna" (6))
                  ("gpt-5.6-luna" "gpt-luna" (5 6))
                  ("gpt-6-luna-fast" "gpt-luna" (6))
                  ("fw-deepseek-v4.1-flash" "deepseek-flash" (4 1))
                  ("accounts/fireworks/models/deepseek-v4p1-flash-1017" "deepseek-flash" (4 1))
                  ("deepseek-v4-flash-0731" "deepseek-flash" (4))
                  ("deepseek-v4.1-flash-free" "deepseek-flash" (4 1))
                  ("accounts/fireworks/routers/deepseek-flash-latest" "deepseek-flash" nil)
                  ("accounts/fireworks/models/glm-5p3-flash" "glm-flash" (5 3))
                  ("fw-glm-5.3-flash" "glm-flash" (5 3))
                  ("glm-flash-latest" "glm-flash" nil)
                  ("gemini-3.8-flash" "gemini-flash" nil)))
    ;; A model of a version is known by its identifier; one of none, by
    ;; the family its catalog gives it.
    (let ((model (drover-cheap-test-model "api" (car case) (and (null (nth 2 case)) (nth 1 case)) "2026-09-22")))
      (should (equal (list (car case) (drover-cheap--family model)
                           (drover-cheap--version model (drover-cheap--family model)))
                     case))))
  (should-not (drover-cheap--family (drover-cheap-test-model "api" "glm-flash-latest" nil "2026-09-22")))
  ;; Versions compare as numbers: 6 is newer than 5.6, 5.10 than 5.9.
  (let ((providers (list (drover-cheap-test-provider "api" :key t))))
    (dolist (case '((("gpt-5.6-luna" "gpt-6-luna") "api/gpt-6-luna")
                    (("gpt-5.10-luna" "gpt-5.9-luna") "api/gpt-5.10-luna")))
      (should (equal (plist-get (drover-cheap-resolve
                                 (mapcar (lambda (id)
                                           ;; The older dated later, as catalogs may.
                                           (drover-cheap-test-model "api" id "gpt-luna"
                                                                    (if (equal id (car (car case)))
                                                                        "2026-12-01" "2026-01-01")))
                                         (car case))
                                 providers nil 'personal)
                                :model)
                     (cadr case))))
    ;; A family of no version told: none outdated, the dates decide.
    (should (equal (mapcar (lambda (model) (list (plist-get model :model) (plist-get model :outdated)))
                           (drover-cheap-rank
                            (list (drover-cheap-test-model "api" "glm-flash-latest" "glm-flash" "2026-08-26")
                                  (drover-cheap-test-model "api" "glm-flash-turbo" "glm-flash" "2026-09-26"))
                            providers nil 'personal))
                   '(("api/glm-flash-turbo" nil) ("api/glm-flash-latest" nil))))
    ;; Versions come from the identifier alone: whatever its date, the older is outdated.
    (let ((ranked (drover-cheap-rank
                   (list (drover-cheap-test-model "api" "claude-haiku-4-5" "claude-haiku" "2026-12-01")
                         (drover-cheap-test-model "api" "claude-haiku-5-5" "claude-haiku" nil)
                         (drover-cheap-test-model "api" "gpt-6-luna" "gpt-luna" "2026-09-22"))
                   providers nil 'personal)))
      (should (equal (mapcar (lambda (model) (list (plist-get model :model) (plist-get model :outdated))) ranked)
                     '(("api/claude-haiku-5-5" nil) ("api/gpt-6-luna" nil) ("api/claude-haiku-4-5" t)))))))

(ert-deftest drover-cheap-test-spot-excluded ()
  "Spot capacity is never chosen, however cheap; other exclusions are options."
  (let ((providers (list (drover-cheap-test-provider "fireworks-ai" :key t)))
        (spot (list (drover-cheap-test-model "fireworks-ai" "accounts/fireworks/routers/deepseek-v4p1-flash-spot"
                                             "deepseek-flash" "2026-09-10" :cost 0.21)
                    (drover-cheap-test-model "fireworks-ai" "accounts/fireworks/routers/deepseek-v4p1-flash-1017-spot"
                                             "deepseek-flash" "2026-10-17" :cost 0.21))))
    (should-not (drover-cheap-resolve spot providers nil 'personal))
    (should (equal (plist-get (drover-cheap-resolve
                               (cons (drover-cheap-test-model "fireworks-ai" "accounts/fireworks/models/deepseek-v4p1-flash"
                                                              "deepseek-flash" "2026-09-10" :cost 0.3)
                                     spot)
                               providers nil 'personal)
                              :model)
                   "fireworks-ai/accounts/fireworks/models/deepseek-v4p1-flash"))
    (let ((drover-cheap-excluded-models nil))
      (should (equal (plist-get (drover-cheap-resolve spot providers nil 'personal) :model)
                     "fireworks-ai/accounts/fireworks/routers/deepseek-v4p1-flash-1017-spot")))
    (let ((drover-cheap-excluded-models '("-1017")))
      (should (equal (plist-get (drover-cheap-resolve spot providers nil 'personal) :model)
                     "fireworks-ai/accounts/fireworks/routers/deepseek-v4p1-flash-spot")))))

;;;; A personal host

(ert-deftest drover-cheap-test-personal-host ()
  "A personal host takes the current first, then subscriptions, MongoDB's relay, then APIs."
  (let ((models drover-cheap-test-core-models)
        (providers drover-cheap-test-core-providers)
        (integrations drover-cheap-test-core-integrations))
    ;; ChatGPT's Luna, though the relay offers Haiku: the provider first, then the tier.
    ;; Its premium edition, of the same day, comes after.
    (should (equal (drover-cheap-test-resolve 'personal models providers integrations)
                   "openai/gpt-6-luna subscription"))
    ;; Copilot signed in, a subscription too: its Haiku, a better tier.
    (should (equal (drover-cheap-test-resolve
                    'personal models providers
                    (cons '((id . "github-copilot") (connections . (((method . "oauth")))))
                          (cdr integrations)))
                   "github-copilot/claude-haiku-5.5 subscription"))
    ;; No subscription: the relay's newest Haiku, the ProdSec gateway's.
    (should (equal (drover-cheap-test-resolve 'personal models providers nil)
                   "prodsec-gateway/claude-haiku-5-5 mongodb-ai"))
    ;; Undated everywhere, still the newest: versions decide, not dates.
    (should (equal (drover-cheap-test-resolve 'personal (drover-cheap-test-without models "github-copilot")
                                              providers nil)
                   "prodsec-gateway/claude-haiku-5-5 mongodb-ai"))
    ;; No relay: OpenCode's free open-weight flash, the newest family's.
    (should (equal (drover-cheap-test-resolve
                    'personal models
                    (seq-filter (lambda (provider) (member (alist-get 'id provider) '("opencode" "openai")))
                                providers)
                    nil)
                   "opencode/deepseek-v4.1-flash-free api"))
    ;; Secateur's copies are no part of MongoDB's relay, nor of the APIs.
    (should-not (drover-cheap-test-resolve
                 'personal
                 (drover-cheap-test-without models "openai" "opencode" "github-copilot" "ai-gateway-anthropic"
                                            "ai-gateway-openai" "ai-gateway-misc" "prodsec-gateway")
                 providers integrations))))

(ert-deftest drover-cheap-test-personal-chain ()
  "Core tries ChatGPT's Luna 6, the relay's Haiku 5.5, Luna 6 and DeepSeek V4.1, then OpenCode's."
  (should (equal (drover-cheap-test-chain 'personal drover-cheap-test-core-models drover-cheap-test-core-providers
                                          drover-cheap-test-core-integrations)
                 '("openai/gpt-6-luna"
                   "prodsec-gateway/claude-haiku-5-5"
                   "ai-gateway-openai/gpt-6-luna"
                   "ai-gateway-misc/fw-deepseek-v4.1-flash"
                   "opencode/deepseek-v4.1-flash-free"
                   ;; Outdated, last.
                   "ai-gateway-anthropic/claude-haiku-4-5"))))

(ert-deftest drover-cheap-test-ties-and-exclusions ()
  "Ties of a day go to the edition not premium, the cheaper, the shorter; some models never."
  (let ((providers (list (drover-cheap-test-provider "api" :key t)))
        (drover-cheap-premium-regexp "-\\(?:fast\\|priority\\)\\'"))
    (dolist (case
             `(((("luna-b" 0.2) ("luna-a" 0.2) ("luna-fast" 0.01)) "api/luna-a")
               ((("luna-long" 0.1) ("luna-x" 0.1)) "api/luna-x")
               ((("luna-x" 0.2) ("luna-long" 0.1)) "api/luna-long")))
      (should (equal (plist-get (drover-cheap-resolve
                                 (mapcar (lambda (entry)
                                           (drover-cheap-test-model "api" (car entry) "gpt-luna" "2026-09-22"
                                                                    :cost (cadr entry)))
                                         (car case))
                                 providers nil 'personal)
                                :model)
                     (cadr case))))
    ;; Disabled or deprecated models, and models of no tier, are not chosen.
    (should-not (drover-cheap-resolve
                 (list (drover-cheap-test-model "api" "a" "gpt-luna" "2026-09-22" :enabled nil)
                       (drover-cheap-test-model "api" "b" "gpt-luna" "2026-09-22" :status "deprecated")
                       (drover-cheap-test-model "api" "c" "gpt-nano" "2026-09-22")
                       (drover-cheap-test-model "api" "d" "gpt-sol" "2026-09-22"))
                 providers nil 'personal))
    ;; A model without a family is known by its identifier.
    (should (equal (plist-get (drover-cheap-resolve
                               (list (drover-cheap-test-model "api" "glm-5.3-flash" nil "2026-08-26")
                                     (drover-cheap-test-model "api" "deepseek-v4.1-flash" "" "2026-09-10")
                                     (drover-cheap-test-model "api" "gpt-6-terra" nil "2026-10-26"))
                               providers nil 'personal)
                              :model)
                   "api/deepseek-v4.1-flash"))))

(ert-deftest drover-cheap-test-options ()
  "The kinds of hosts, classes and tiers are data; a kind without an order resolves nothing."
  (let ((models drover-cheap-test-core-models)
        (providers drover-cheap-test-core-providers)
        (integrations drover-cheap-test-core-integrations))
    (let ((drover-cheap-families '((open-weight :family "\\`\\(?:deepseek\\|glm\\)-flash\\'"))))
      (should (equal (drover-cheap-test-resolve 'personal models providers integrations)
                     "ai-gateway-misc/fw-deepseek-v4.1-flash mongodb-ai")))
    ;; An order of APIs alone: the relay's providers among them, ChatGPT's login not.
    (let ((drover-cheap-provider-orders '((thrifty api))))
      (should (equal (drover-cheap-test-resolve 'thrifty models providers integrations)
                     "prodsec-gateway/claude-haiku-5-5 api"))
      (should-not (drover-cheap-test-resolve 'personal models providers integrations))
      (should (string-search "no order" (drover-cheap-none-reason 'personal))))
    (let ((drover-cheap-excluded-providers '("\\`openai\\'")))
      (should (equal (drover-cheap-test-resolve 'personal models providers integrations)
                     "prodsec-gateway/claude-haiku-5-5 mongodb-ai")))
    (let ((drover-cheap-host-kind 'mongodb))
      (should (equal (plist-get (drover-cheap-resolve drover-cheap-test-reaver-models
                                                      drover-cheap-test-reaver-providers
                                                      (drover-cheap-integration-auths
                                                       drover-cheap-test-reaver-integrations))
                                :model)
                     "github-copilot/claude-haiku-5.5"))
      (should (string-search "github-copilot, grove-bedrock" (drover-cheap-none-reason))))))

;;;; This host's, from its service

(defvar drover-cheap-test-requests nil
  "The requests the stubbed service was sent, newest first.")

(defmacro drover-cheap-test-with-service (catalogs integrations &rest body)
  "Run BODY with a stubbed local service telling CATALOGS and INTEGRATIONS.
CATALOGS is a variable holding an alist (KIND . DATA), read as the
catalogs are fetched, so that BODY may change it; INTEGRATIONS one
holding the data of /api/integration, or `missing' for a service
without the route."
  (declare (indent 2) (debug t))
  `(let ((drover-cheap--auths nil)
         (drover-cheap--epoch 0)
         (drover-cheap--waiters nil)
         (drover-cheap--signed-in-waiters nil)
         (drover-cheap--resolved nil)
         (drover-cheap--following t)
         (drover-cheap-host-kind 'mongodb)
         (opencode-catalog-generation 0)
         (drover-cheap-test-requests nil)
         (cached nil))
     (cl-letf (((symbol-function 'opencode-service-read-registration)
                (lambda () (list :url "http://127.0.0.1:1" :password "x" :pid (emacs-pid))))
               ((symbol-function 'opencode-catalog-fetch)
                (lambda (kind callback &optional directory _else)
                  (should (equal directory (file-name-as-directory (expand-file-name "~"))))
                  (push (list 'catalog kind) drover-cheap-test-requests)
                  (unless (equal (alist-get kind cached) (alist-get kind ,catalogs))
                    (setf (alist-get kind cached) (alist-get kind ,catalogs))
                    (cl-incf opencode-catalog-generation))
                  (funcall callback (alist-get kind cached))))
               ((symbol-function 'opencode-catalog-cached-p)
                (lambda (kind &optional _directory) (assq kind cached)))
               ((symbol-function 'opencode-catalog)
                (lambda (kind &optional _directory) (alist-get kind cached)))
               ((symbol-function 'opencode-request)
                (lambda (method path &rest arguments)
                  (push (list method path (plist-get arguments :directory)) drover-cheap-test-requests)
                  (if (eq ,integrations 'missing)
                      (funcall (plist-get arguments :else)
                               (opencode-error--make :status 404 :method method :path path :message "Not Found"))
                    (funcall (plist-get arguments :then) (list (cons 'data ,integrations)))))))
       ,@body)))

(ert-deftest drover-cheap-test-fetch ()
  "The catalogs and integrations are read once, the model resolved again only as they change."
  (let ((catalogs `((model . ,drover-cheap-test-reaver-models) (provider . ,drover-cheap-test-reaver-providers)))
        (integrations drover-cheap-test-reaver-integrations)
        (results nil))
    (drover-cheap-test-with-service catalogs integrations
      (should (equal (plist-get (drover-model-cheap) :model) "github-copilot/claude-haiku-5.5"))
      (should (equal (reverse drover-cheap-test-requests)
                     '((catalog model) (catalog provider) (get "/api/integration" nil))))
      (setq drover-cheap-test-requests nil)
      (should (equal (plist-get (drover-model-cheap) :model) "github-copilot/claude-haiku-5.5"))
      (should-not drover-cheap-test-requests)
      (let ((resolved (cdr drover-cheap--resolved)))
        (drover-model-cheap-fetch (lambda (result) (push result results)))
        (should (eq (car results) resolved)))
      ;; Copilot signed out: the integrations change, read again.
      (setq integrations (drover-cheap-test-signed-out "github-copilot")
            drover-cheap-test-requests nil)
      (drover-cheap--integrations-changed)
      (drover-model-cheap-fetch (lambda (result) (push result results)))
      (should (member '(get "/api/integration" nil) drover-cheap-test-requests))
      (should (equal (plist-get (car results) :model) "ai-gateway-openai/gpt-6-luna"))
      ;; The gateway's Luna gone too: the catalog changes, the model with it.
      (setf (alist-get 'model catalogs)
            (drover-cheap-test-without drover-cheap-test-reaver-models "github-copilot" "ai-gateway-openai"))
      (drover-model-cheap-fetch (lambda (result) (push result results)))
      (should (equal (plist-get (car results) :model) "ai-gateway-misc/fw-deepseek-v4.1-flash"))
      ;; Nothing cheap left: none, with why.
      (setf (alist-get 'model catalogs) nil)
      (drover-model-cheap-fetch (lambda (result) (push result results)))
      (should (string-search "no provider of github-copilot" (plist-get (car results) :none)))
      (should-not (drover-model-cheap)))))

(ert-deftest drover-cheap-test-fetch-failures ()
  "A service not running is never asked; one without integrations still resolves; failures are told."
  (let ((catalogs `((model . ,drover-cheap-test-core-models) (provider . ,drover-cheap-test-core-providers)))
        (integrations 'missing)
        (results nil))
    (drover-cheap-test-with-service catalogs integrations
      (setq drover-cheap-host-kind 'personal)
      ;; Without integrations, a provider of one counts as signed in by key:
      ;; ChatGPT's login is no subscription then.
      (drover-model-cheap-fetch (lambda (result) (push result results)))
      (should (eq (cdr drover-cheap--auths) 'unknown))
      (should (equal (plist-get (car results) :model) "prodsec-gateway/claude-haiku-5-5"))
      (cl-letf (((symbol-function 'opencode-service-read-registration) (lambda () nil)))
        (setq drover-cheap-test-requests nil)
        (drover-model-cheap-fetch (lambda (result) (push result results)))
        (should (equal (car results) '(:failure "OpenCode's service does not run")))
        (should-not drover-cheap-test-requests))
      (cl-letf (((symbol-function 'opencode-request)
                 (lambda (method path &rest arguments)
                   (funcall (plist-get arguments :else)
                            (opencode-error--make :status 500 :method method :path path :message "Boom")))))
        (drover-cheap--integrations-changed)
        (drover-model-cheap-fetch (lambda (result) (push result results)))
        (should (string-search "Boom" (plist-get (car results) :failure)))))))

(ert-deftest drover-cheap-test-signed-in ()
  "How the service is signed in to a provider is unknown until read, then told."
  (let ((catalogs `((model . ,drover-cheap-test-core-models) (provider . ,drover-cheap-test-core-providers)))
        (integrations drover-cheap-test-core-integrations)
        (called 0))
    (drover-cheap-test-with-service catalogs integrations
      (cl-flet ((then () (cl-incf called)))
        ;; Not read yet: unknown, read, and told once read.
        (should (eq (drover-cheap-signed-in "openai" #'then) 'unknown))
        (should (= called 1))
        (should (member '(get "/api/integration" nil) drover-cheap-test-requests))
        ;; ChatGPT's login, a key, a provider of no integration, none.
        (setq drover-cheap-test-requests nil)
        (should (eq (drover-cheap-signed-in "openai" #'then) 'subscription))
        (should (eq (drover-cheap-signed-in "ai-gateway-openai") 'api))
        (should-not (drover-cheap-signed-in "nowhere"))
        (should-not drover-cheap-test-requests)
        (should (= called 1))
        ;; The integrations changed: unknown until read again.
        (setq integrations '(((id . "openai") (connections))))
        (drover-cheap--integrations-changed)
        (should (eq (drover-cheap-signed-in "openai" #'then) 'unknown))
        (should (= called 2))
        (should-not (eq (drover-cheap-signed-in "openai") 'subscription))
        ;; A read under way is not started again; all waiting are told once.
        (let ((pending nil))
          (cl-letf (((symbol-function 'drover-model-cheap-fetch) (lambda (callback) (push callback pending))))
            (drover-cheap--integrations-changed)
            (should (eq (drover-cheap-signed-in "openai") 'unknown))
            (should (eq (drover-cheap-signed-in "openai" #'then) 'unknown))
            (should (eq (drover-cheap-signed-in "anthropic" #'then) 'unknown))
            (should (= (length pending) 1))
            (funcall (car pending) '(:none "none"))
            (should (= called 4))
            (should-not drover-cheap--signed-in-waiters)
            ;; A failure tells none, and a next question reads again.
            (setq pending nil)
            (should (eq (drover-cheap-signed-in "openai" #'then) 'unknown))
            (funcall (car pending) '(:failure "Boom"))
            (should (= called 4))
            (should (eq (drover-cheap-signed-in "openai" #'then) 'unknown))
            (should (= (length pending) 2))))
        ;; A service not running is never asked.
        (cl-letf (((symbol-function 'opencode-service-read-registration) (lambda () nil)))
          (setq drover-cheap--signed-in-waiters nil
                drover-cheap-test-requests nil)
          (drover-cheap--integrations-changed)
          (should (eq (drover-cheap-signed-in "openai" #'then) 'unknown))
          (should-not drover-cheap-test-requests)
          (should-not drover-cheap--signed-in-waiters))))))

;;;; Asking it

(defvar drover-cheap-test-asked nil
  "The generations the stubbed service was asked, (PATH MODEL PROMPT), newest first.")

(defun drover-cheap-test-error (tag status &rest data)
  "Return the `opencode-error' of a generation, TAG and STATUS, DATA its other fields."
  (opencode-error--make :status status :tag tag :method 'post :path "/api/experimental/generate/text"
                        :message (or (plist-get data :message) "failed")
                        :data (and tag `((_tag . ,tag)
                                         ,@(cl-loop for (key value) on data by #'cddr
                                                    collect (cons (intern (substring (symbol-name key) 1)) value))))))

(defmacro drover-cheap-test-with-generation (answer &rest body)
  "Run BODY with a stubbed service answering generations through ANSWER.
ANSWER is called with the path and the model reference asked, and
returns the text the model writes, an `opencode-error', or `hang' for
no answer yet.  The cheap model is Reaver's, its chain behind it."
  (declare (indent 1) (debug t))
  `(let ((drover-cheap-test-asked nil)
         (drover-cheap--former-path nil)
         (drover-cheap-attempts 4)
         (drover-cheap-variant "low"))
     (cl-letf (((symbol-function 'opencode-service-read-registration)
                (lambda () (list :url "http://127.0.0.1:1" :password "x" :pid (emacs-pid))))
               ((symbol-function 'drover-model-cheap-fetch)
                (lambda (callback)
                  (funcall callback (drover-cheap-resolve drover-cheap-test-reaver-models
                                                          drover-cheap-test-reaver-providers
                                                          (drover-cheap-integration-auths
                                                           drover-cheap-test-reaver-integrations)
                                                          'mongodb))))
               ((symbol-function 'opencode-request)
                (lambda (method path &rest arguments)
                  (should (eq method 'post))
                  (should (plist-member arguments :directory))
                  (should-not (plist-get arguments :directory))
                  (let* ((body (plist-get arguments :body))
                         (model (alist-get 'model body))
                         (answer (funcall ,answer path model)))
                    (push (list path (format "%s/%s" (alist-get 'providerID model) (alist-get 'id model))
                                (alist-get 'prompt body))
                          drover-cheap-test-asked)
                    (cond
                     ((eq answer 'hang))
                     ((stringp answer) (funcall (plist-get arguments :then) `((data (text . ,answer)))))
                     (t (funcall (plist-get arguments :else) answer)))))))
       ,@body)))

(defun drover-cheap-test-asked-models ()
  "Return the models the stubbed service was asked, in order."
  (mapcar #'cadr (reverse drover-cheap-test-asked)))

(ert-deftest drover-cheap-test-generate ()
  "The cheap model answers the prompt, the instructions folded before it, at its variant."
  (let ((results nil)
        (bodies nil))
    (drover-cheap-test-with-generation (lambda (_path model) (push model bodies) "feat: add it")
      (drover-cheap-generate "Changes:\n+x" "Write a commit message." (lambda (result) (push result results)))
      (should (equal (car results) '(:text "feat: add it" :model "github-copilot/claude-haiku-5.5"
                                           :name "Claude Haiku 5.5")))
      (should (equal (car drover-cheap-test-asked)
                     '("/api/experimental/generate/text" "github-copilot/claude-haiku-5.5"
                       "Write a commit message.\n\nChanges:\n+x")))
      (should (equal (car bodies) '((providerID . "github-copilot") (id . "claude-haiku-5.5") (variant . "low"))))
      ;; Without instructions, the prompt alone; a model without the variant runs its default.
      (setq drover-cheap-test-asked nil)
      (drover-cheap-generate "Summarize" "  " (lambda (result) (push result results)) :variant "minimal")
      (should (equal (nth 2 (car drover-cheap-test-asked)) "Summarize"))
      (should (equal (car bodies) '((providerID . "github-copilot") (id . "claude-haiku-5.5"))))
      ;; A model named is the one asked, at the variant asked.
      (setq drover-cheap-test-asked nil)
      (drover-cheap-generate "Summarize" nil (lambda (result) (push result results))
                             :model "openai/gpt-6-luna" :variant "none")
      (should (equal (drover-cheap-test-asked-models) '("openai/gpt-6-luna")))
      (should (equal (car bodies) '((providerID . "openai") (id . "gpt-6-luna") (variant . "none"))))
      (should (equal (plist-get (car results) :model) "openai/gpt-6-luna"))
      (setq drover-cheap-test-asked nil)
      (drover-cheap-generate "Summarize" nil (lambda (result) (push result results)) :model "gpt-6-luna")
      (should-not drover-cheap-test-asked)
      (should (string-search "PROVIDER/ID" (plist-get (car results) :failure))))))

(ert-deftest drover-cheap-test-generate-fallback ()
  "A model unavailable, out of quota or not signed in leaves the next of the chain to answer."
  (dolist (error (list (drover-cheap-test-error "ProviderRequestError" 502 :reason "rate_limit")
                       (drover-cheap-test-error "ProviderRequestError" 502 :reason "authentication")
                       (drover-cheap-test-error "ProviderRequestError" 502 :reason "overloaded")
                       (drover-cheap-test-error "ProviderRequestError" 502 :reason "provider")
                       (drover-cheap-test-error "ModelNotFoundError" 404)
                       (drover-cheap-test-error "ProviderNotFoundError" 404)
                       (drover-cheap-test-error "UnsupportedModelError" 422)
                       (drover-cheap-test-error "ServiceUnavailableError" 503)
                       (drover-cheap-test-error "InvalidRequestError" 400 :field "model")
                       (drover-cheap-test-error "InvalidRequestError" 400 :field "model.variant")
                       (drover-cheap-test-error nil 500)
                       (drover-cheap-test-error nil nil)))
    (let ((results nil))
      (drover-cheap-test-with-generation
          (lambda (_path model) (if (equal (alist-get 'providerID model) "github-copilot") error "feat: add it"))
        (drover-cheap-generate "Changes" nil (lambda (result) (push result results)))
        (should (equal (list (opencode-error-tag error) (drover-cheap-test-asked-models))
                       (list (opencode-error-tag error)
                             '("github-copilot/claude-haiku-5.5" "ai-gateway-openai/gpt-6-luna"))))
        (should (equal results '((:text "feat: add it" :model "ai-gateway-openai/gpt-6-luna" :name "gpt-6-luna"))))))))

(ert-deftest drover-cheap-test-generate-no-fallback ()
  "What any model would fail alike is told at once, no other model asked."
  (dolist (error (list (drover-cheap-test-error "ProviderRequestError" 502 :reason "content_filter"
                                                :message "Refused by the content filter")
                       (drover-cheap-test-error "ProviderRequestError" 502 :reason "invalid_request")
                       (drover-cheap-test-error "InvalidRequestError" 400)
                       (drover-cheap-test-error "InvalidRequestError" 400 :field "prompt")
                       (drover-cheap-test-error "LocationNotFoundError" 404)
                       (drover-cheap-test-error nil 401)
                       (drover-cheap-test-error nil 400)))
    (let ((results nil))
      (drover-cheap-test-with-generation (lambda (_path _model) error)
        (drover-cheap-generate "Changes" nil (lambda (result) (push result results)))
        (should (equal (list (opencode-error-status error) (opencode-error-tag error) (drover-cheap-test-asked-models))
                       (list (opencode-error-status error) (opencode-error-tag error)
                             '("github-copilot/claude-haiku-5.5"))))
        (should (= (length results) 1))
        (should (string-prefix-p "github-copilot/claude-haiku-5.5: POST" (plist-get (car results) :failure))))))
  ;; A model answering nothing is a failure too, the next not asked.
  (let ((results nil))
    (drover-cheap-test-with-generation (lambda (_path _model) "  ")
      (drover-cheap-generate "Changes" nil (lambda (result) (push result results)))
      (should (= (length drover-cheap-test-asked) 1))
      (should (string-search "no text" (plist-get (car results) :failure))))))

(ert-deftest drover-cheap-test-generate-bounded ()
  "No more models are asked than `drover-cheap-attempts'; every failure is told."
  (let ((results nil))
    (drover-cheap-test-with-generation
        (lambda (_path model)
          (drover-cheap-test-error "ProviderRequestError" 502 :reason "rate_limit"
                                   :message (format "Quota of %s exceeded" (alist-get 'providerID model))))
      (drover-cheap-generate "Changes" nil (lambda (result) (push result results)))
      (should (equal (drover-cheap-test-asked-models)
                     '("github-copilot/claude-haiku-5.5" "ai-gateway-openai/gpt-6-luna"
                       "ai-gateway-misc/fw-deepseek-v4.1-flash"
                       "fireworks-ai/accounts/fireworks/models/deepseek-v4p1-flash-1017")))
      (should (= (length results) 1))
      (let ((failure (plist-get (car results) :failure)))
        (should (string-search "Quota of github-copilot exceeded" failure))
        (should (string-search "Quota of fireworks-ai exceeded" failure))
        (should-not (string-search "prodsec" failure)))
      (setq drover-cheap-test-asked nil results nil)
      (drover-cheap-generate "Changes" nil (lambda (result) (push result results)) :attempts 2)
      (should (= (length drover-cheap-test-asked) 2))
      ;; The whole chain, when asked to.
      (setq drover-cheap-test-asked nil results nil)
      (drover-cheap-generate "Changes" nil (lambda (result) (push result results)) :attempts 10)
      (should (= (length drover-cheap-test-asked) 6))
      ;; A model named is the only one asked.
      (setq drover-cheap-test-asked nil results nil)
      (drover-cheap-generate "Changes" nil (lambda (result) (push result results)) :model "openai/gpt-6-luna")
      (should (= (length drover-cheap-test-asked) 1)))))

(ert-deftest drover-cheap-test-generate-former-path ()
  "A service knowing generation by its former path alone is asked there; its invalid request is a model's."
  (let ((results nil))
    (drover-cheap-test-with-generation
        (lambda (path model)
          (cond
           ((equal path "/api/experimental/generate/text")
            (opencode-error--make :status 404 :method 'post :path path :message "Not Found"))
           ;; Before 2.2, a model the service cannot select is an invalid request, untold.
           ((equal (alist-get 'providerID model) "github-copilot")
            (opencode-error--make :status 400 :tag "InvalidRequestError" :method 'post :path path
                                  :message "Model unavailable" :data '((_tag . "InvalidRequestError"))))
           (t "Done: it works")))
      (drover-cheap-generate "Summarize" nil (lambda (result) (push result results)))
      (should (equal (mapcar (lambda (asked) (list (car asked) (cadr asked))) (reverse drover-cheap-test-asked))
                     '(("/api/experimental/generate/text" "github-copilot/claude-haiku-5.5")
                       ("/api/experimental/generate" "github-copilot/claude-haiku-5.5")
                       ("/api/experimental/generate" "ai-gateway-openai/gpt-6-luna"))))
      (should (equal (plist-get (car results) :text) "Done: it works"))
      (should drover-cheap--former-path))))

(ert-deftest drover-cheap-test-generate-without-error-data ()
  "Without the object of an error, a provider's error is taken for one of availability."
  (let ((results nil))
    (drover-cheap-test-with-generation
        (lambda (_path model)
          (if (equal (alist-get 'providerID model) "github-copilot")
              (drover-cheap-test-error "ProviderRequestError" 502 :reason "content_filter")
            "feat: add it"))
      (cl-letf (((symbol-function 'opencode-error-data) nil))
        (should-not (fboundp 'opencode-error-data))
        (drover-cheap-generate "Changes" nil (lambda (result) (push result results)))
        (should (= (length drover-cheap-test-asked) 2))
        ;; An invalid request tells no field then: the request's, but before 2.2.
        (should-not (drover-cheap-fallback-p (drover-cheap-test-error "InvalidRequestError" 400 :field "model")))
        (let ((drover-cheap--former-path t))
          (should (drover-cheap-fallback-p (drover-cheap-test-error "InvalidRequestError" 400)))))
      (should (equal (plist-get (car results) :model) "ai-gateway-openai/gpt-6-luna")))))

(ert-deftest drover-cheap-test-generate-cancel-and-failures ()
  "A request cancelled calls back never and asks no other model; failures to begin are told."
  (let ((results nil)
        (else nil))
    (drover-cheap-test-with-generation (lambda (_path _model) 'hang)
      (cl-letf* ((request (symbol-function 'opencode-request))
                 ((symbol-function 'opencode-request)
                  (lambda (method path &rest arguments)
                    (setq else (plist-get arguments :else))
                    (apply request method path arguments))))
        (let ((cancel (drover-cheap-generate "Changes" nil (lambda (result) (push result results)))))
          (should (functionp cancel))
          (funcall cancel)
          (funcall else (drover-cheap-test-error "ProviderRequestError" 502 :reason "rate_limit"))
          (should (= (length drover-cheap-test-asked) 1))
          (should-not results)))
      ;; A service not running is never asked, nor started.
      (setq drover-cheap-test-asked nil)
      (cl-letf (((symbol-function 'opencode-service-read-registration) (lambda () nil)))
        (drover-cheap-generate "Changes" nil (lambda (result) (push result results))))
      (should-not drover-cheap-test-asked)
      (should (equal results '((:failure "OpenCode's service does not run"))))
      ;; No cheap model: said so, nothing asked; the catalogs unread: told.
      (dolist (case '(((:none "no provider offers one") (:failure "no cheap model, no provider offers one" :none t))
                      ((:failure "Boom") (:failure "Boom"))))
        (setq results nil)
        (cl-letf (((symbol-function 'drover-model-cheap-fetch) (lambda (callback) (funcall callback (car case)))))
          (drover-cheap-generate "Changes" nil (lambda (result) (push result results))))
        (should-not drover-cheap-test-asked)
        (should (equal results (cdr case)))))))

;;;; OpenCode's titles

(defmacro drover-cheap-test-with-title-file (&rest body)
  "Run BODY with `drover-cheap-title-file' a file of a new temporary directory, `file'."
  (declare (indent 0) (debug t))
  `(let* ((directory (make-temp-file "drover-cheap-title-" t))
          (file (expand-file-name "opencode.json" directory))
          (drover-cheap-title-file file)
          (drover-cheap--title-foreign nil)
          (drover-cheap-variant "low"))
     (unwind-protect
         (progn ,@body)
       (delete-directory directory t))))

(defun drover-cheap-test-title-model (file)
  "Return the model FILE names for OpenCode's title agent, an alist."
  (let ((data (json-parse-string (with-temp-buffer (insert-file-contents file) (buffer-string))
                                 :object-type 'alist)))
    (should (equal (alist-get (intern "$schema") data) "https://opencode.ai/config.json"))
    (alist-get 'model (alist-get 'title (alist-get 'agents data)))))

(ert-deftest drover-cheap-test-title-file ()
  "The file is opencode.json where the service finds its global configuration, unless set."
  (let ((drover-cheap-title-file nil)
        (process-environment (copy-sequence process-environment)))
    (setenv "OPENCODE_CONFIG_DIR" nil)
    (setenv "XDG_CONFIG_HOME" nil)
    (should (equal (drover-cheap-title-file) (expand-file-name "~/.config/opencode/opencode.json")))
    ;; An empty variable counts as unset, as the service has it.
    (setenv "XDG_CONFIG_HOME" "")
    (should (equal (drover-cheap-title-file) (expand-file-name "~/.config/opencode/opencode.json")))
    (setenv "XDG_CONFIG_HOME" "/xdg")
    (should (equal (drover-cheap-title-file) (expand-file-name "/xdg/opencode/opencode.json")))
    (setenv "OPENCODE_CONFIG_DIR" "/oc")
    (should (equal (drover-cheap-title-file) (expand-file-name "/oc/opencode.json")))
    (let ((drover-cheap-title-file "/elsewhere/title.json"))
      (should (equal (drover-cheap-title-file) (expand-file-name "/elsewhere/title.json"))))))

(ert-deftest drover-cheap-test-title-apply ()
  "The model is written as it changes, by provider and identifier; no model deletes the file."
  (drover-cheap-test-with-title-file
    (let ((writes 0)
          (write (symbol-function 'drover-cheap--title-write)))
      (cl-letf (((symbol-function 'drover-cheap--title-write)
                 (lambda (&rest arguments) (cl-incf writes) (apply write arguments))))
        ;; The variant when the model offers it; an identifier holding slashes stays whole.
        (drover-cheap-title-apply '(:model "fireworks-ai/accounts/fireworks/models/deepseek"
                                           :provider-id "fireworks-ai" :id "accounts/fireworks/models/deepseek"
                                           :variants ("low" "high")))
        (should (equal (drover-cheap-test-title-model file)
                       '((providerID . "fireworks-ai") (model . "accounts/fireworks/models/deepseek")
                         (variant . "low"))))
        (should-not (directory-files (file-name-directory file) nil "\\.tmp\\'"))
        ;; The same model again: not written again.
        (drover-cheap-title-apply '(:model "fireworks-ai/accounts/fireworks/models/deepseek"
                                           :provider-id "fireworks-ai" :id "accounts/fireworks/models/deepseek"
                                           :variants ("low" "high")))
        (should (= writes 1))
        ;; Without the variant, the model's default.
        (drover-cheap-title-apply '(:model "a/haiku" :provider-id "a" :id "haiku" :variants nil))
        (should (equal (drover-cheap-test-title-model file) '((providerID . "a") (model . "haiku"))))
        (should (= writes 2))
        ;; The catalogs unread: left as it is.
        (drover-cheap-title-apply '(:failure "OpenCode's service does not run"))
        (should (equal (drover-cheap-test-title-model file) '((providerID . "a") (model . "haiku"))))
        ;; No cheap model: OpenCode titles as it does by default.
        (drover-cheap-title-apply '(:none "no provider offers one"))
        (should-not (file-exists-p file))
        (drover-cheap-title-apply '(:none "no provider offers one"))
        (should-not (file-exists-p file))
        (should (= writes 2))))))

(ert-deftest drover-cheap-test-title-foreign-file ()
  "A file holding more than the title's model is never written nor deleted, which is said once."
  (drover-cheap-test-with-title-file
    (let ((warnings nil)
          (foreign "{\"agents\": {\"title\": {\"model\": \"a/b\", \"system\": \"Mine\"}}}\n"))
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (_type message &rest _) (push message warnings))))
        (dolist (text (list foreign "{\"model\": \"a/b\"}" "not JSON"))
          (with-temp-file file (insert text))
          (drover-cheap-title-apply '(:model "a/haiku" :provider-id "a" :id "haiku" :variants nil))
          (drover-cheap-title-apply '(:none "no provider offers one"))
          (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string)) text)))
        (should (= (length warnings) 1))
        (should (string-search "holds more than the model of OpenCode" (car warnings)))
        ;; A blank file is the mode's to write.
        (with-temp-file file (insert "\n"))
        (drover-cheap-title-apply '(:model "a/haiku" :provider-id "a" :id "haiku" :variants nil))
        (should (equal (drover-cheap-test-title-model file) '((providerID . "a") (model . "haiku"))))))))

(ert-deftest drover-cheap-test-title-mode ()
  "The mode names this host's cheap model, again as the service changes it, and deletes it once off."
  (let ((catalogs `((model . ,drover-cheap-test-reaver-models) (provider . ,drover-cheap-test-reaver-providers)))
        (integrations drover-cheap-test-reaver-integrations))
    (drover-cheap-test-with-service catalogs integrations
      (drover-cheap-test-with-title-file
        (let ((drover-cheap--title-timer nil))
          (unwind-protect
              (progn
                (drover-cheap-title-mode 1)
                (should (timerp drover-cheap--title-timer))
                (cancel-timer drover-cheap--title-timer)
                (drover-cheap--title-update)
                (should-not drover-cheap--title-timer)
                (should (equal (drover-cheap-test-title-model file)
                               '((providerID . "github-copilot") (model . "claude-haiku-5.5") (variant . "low"))))
                ;; Copilot signed out: the next model, once the change settles.
                (setq integrations (drover-cheap-test-signed-out "github-copilot"))
                (drover-cheap--integrations-changed)
                (should (timerp drover-cheap--title-timer))
                (cancel-timer drover-cheap--title-timer)
                (drover-cheap--title-update)
                (should (equal (alist-get 'providerID (drover-cheap-test-title-model file)) "ai-gateway-openai"))
                (drover-cheap-title-mode -1)
                (should-not (file-exists-p file))
                ;; Off, changes leave the file alone.
                (drover-cheap--integrations-changed)
                (should-not drover-cheap--title-timer))
            (drover-cheap-title-mode -1)
            (when (timerp drover-cheap--title-timer)
              (cancel-timer drover-cheap--title-timer))))))))

(provide 'drover-cheap-test)
;;; drover-cheap-test.el ends here
