;;; drover-opencode-fixtures.el --- OpenCode catalogs for drover's tests  -*- lexical-binding: t; -*-

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

;; The OpenCode service's agent, model and provider catalogs, shaped
;; like the responses recorded in opencode.el's test/fixtures/v2, with
;; Claude entries shaped like the variants OpenCode's variant.ts gives
;; them on the Anthropic, Copilot and Bedrock packages, Opus 5.5 with a
;; context size and a long-context price tier.  The recorded
;; files themselves are read by `drover-opencode-fixtures-recorded',
;; which skips the test when opencode.el's tests are not installed.

;;; Code:

(require 'ert)
(require 'seq)
(require 'opencode)
(require 'drover-opencode)

(defun drover-opencode-fixtures-json (text)
  "Parse JSON TEXT the way opencode.el decodes the service's responses."
  (json-parse-string text :object-type 'alist :array-type 'list
                     :null-object nil :false-object nil))

(defun drover-opencode-fixtures-claude-variants (&rest efforts)
  "Return the adaptive variants of EFFORTS as OpenCode's variant.ts spells them."
  (mapcar (lambda (effort)
            `((id . ,effort)
              (settings (thinking (type . "adaptive") (display . "summarized"))
                        (effort . ,effort))))
          efforts))

(defun drover-opencode-fixtures-bedrock-claude-variants (&rest efforts)
  "Return the Bedrock Converse Claude variants of EFFORTS.
They are spelled as OpenCode's variant.ts spells them."
  (mapcar (lambda (effort)
            `((id . ,effort)
              (body (additionalModelRequestFields
                     (thinking (type . "adaptive") (display . "summarized"))
                     (output_config (effort . ,effort))))))
          efforts))

(defconst drover-opencode-fixtures-providers
  (drover-opencode-fixtures-json
   "[{\"id\": \"opencode\", \"name\": \"OpenCode\", \"activation\": \"enabled\",
      \"package\": \"@opencode/ai/providers/openai-compatible\",
      \"settings\": {\"apiKey\": \"public\", \"baseURL\": \"https://opencode.ai/inference/openai/v1\"}},
     {\"id\": \"openai\", \"name\": \"OpenAI\", \"activation\": \"auto\",
      \"package\": \"@opencode/ai/providers/openai\",
      \"settings\": {\"transport\": \"websocket\", \"encoding\": \"zstd\",
                     \"baseURL\": \"https://chatgpt.com/backend-api/codex\"},
      \"headers\": {\"originator\": \"opencode\"}},
     {\"id\": \"anthropic\", \"name\": \"Anthropic\", \"package\": \"@opencode/ai/providers/anthropic\"},
     {\"id\": \"github-copilot\", \"name\": \"GitHub Copilot\",
      \"package\": \"aisdk:@ai-sdk/github-copilot\"},
     {\"id\": \"amazon-bedrock\", \"name\": \"Amazon Bedrock\",
      \"package\": \"@opencode/ai/providers/amazon-bedrock\"}]")
  "Provider catalog: the OpenCode and OpenAI entries recorded in opencode.el's
test/fixtures/v2/provider.json, and Anthropic, Copilot and Bedrock entries
shaped like them.")

(defconst drover-opencode-fixtures-models
  (append
   (drover-opencode-fixtures-json
    "[{\"id\": \"gpt-5.5\", \"modelID\": \"gpt-5.5\", \"providerID\": \"openai\", \"family\": \"gpt\",
       \"name\": \"GPT-5.5\", \"package\": \"@opencode/ai/providers/openai\",
       \"settings\": {\"encoding\": \"zstd\", \"baseURL\": \"https://chatgpt.com/backend-api/codex\"},
       \"variants\": [
         {\"id\": \"none\", \"settings\": {\"reasoningEffort\": \"none\", \"reasoningSummary\": \"auto\",
                                       \"include\": [\"reasoning.encrypted_content\"]}},
         {\"id\": \"low\", \"settings\": {\"reasoningEffort\": \"low\", \"reasoningSummary\": \"auto\",
                                      \"include\": [\"reasoning.encrypted_content\"]}},
         {\"id\": \"medium\", \"settings\": {\"reasoningEffort\": \"medium\", \"reasoningSummary\": \"auto\",
                                         \"include\": [\"reasoning.encrypted_content\"]}},
         {\"id\": \"high\", \"settings\": {\"reasoningEffort\": \"high\", \"reasoningSummary\": \"auto\",
                                       \"include\": [\"reasoning.encrypted_content\"]}},
         {\"id\": \"xhigh\", \"settings\": {\"reasoningEffort\": \"xhigh\", \"reasoningSummary\": \"auto\",
                                        \"include\": [\"reasoning.encrypted_content\"]}}],
       \"status\": \"active\", \"enabled\": true},
      {\"id\": \"gpt-6.1-sol-fast\", \"modelID\": \"gpt-6.1-sol\", \"providerID\": \"openai\",
       \"family\": \"gpt-sol\", \"name\": \"GPT-6.1 Sol Fast\", \"package\": \"@opencode/ai/providers/openai\",
       \"settings\": {\"encoding\": \"zstd\", \"baseURL\": \"https://chatgpt.com/backend-api/codex\"},
       \"body\": {\"service_tier\": \"priority\"},
       \"variants\": [
         {\"id\": \"low\", \"settings\": {\"reasoningEffort\": \"low\", \"reasoningSummary\": \"auto\",
                                      \"include\": [\"reasoning.encrypted_content\"]}},
         {\"id\": \"high\", \"settings\": {\"reasoningEffort\": \"high\", \"reasoningSummary\": \"auto\",
                                       \"include\": [\"reasoning.encrypted_content\"]}},
         {\"id\": \"max\", \"settings\": {\"reasoningEffort\": \"max\", \"reasoningSummary\": \"auto\",
                                      \"include\": [\"reasoning.encrypted_content\"]}}],
       \"status\": \"active\", \"enabled\": true},
      {\"id\": \"big-pickle\", \"modelID\": \"big-pickle\", \"providerID\": \"opencode\",
       \"family\": \"big-pickle\", \"name\": \"Big Pickle\",
       \"compatibility\": {\"reasoningField\": \"reasoning_content\"},
       \"package\": \"@opencode/ai/providers/openai-compatible\",
       \"settings\": {\"apiKey\": \"public\", \"baseURL\": \"https://opencode.ai/inference/openai/v1\",
                      \"provider\": \"opencode\"},
       \"variants\": [], \"status\": \"active\", \"enabled\": true}]")
   `(((id . "claude-opus-5-5") (providerID . "anthropic") (name . "Claude Opus 5.5")
      (package . "@opencode/ai/providers/anthropic")
      (limit (context . 1000000) (output . 128000))
      ;; Twice the price past 200k tokens of context, as models.dev lists it.
      (cost ((input . 5)) ((input . 10) (tier (type . "context") (size . 200000))))
      (variants . ,(drover-opencode-fixtures-claude-variants "low" "medium" "high" "xhigh" "max")))
     ((id . "claude-sonnet-5-5") (providerID . "anthropic") (name . "Claude Sonnet 5.5")
      (package . "@opencode/ai/providers/anthropic")
      (variants . ,(drover-opencode-fixtures-claude-variants "low" "medium" "high" "xhigh" "max")))
     ((id . "claude-opus-4-6") (providerID . "anthropic") (name . "Claude Opus 4.6")
      (package . "@opencode/ai/providers/anthropic")
      (variants . ,(drover-opencode-fixtures-claude-variants "low" "medium" "high" "max")))
     ((id . "claude-opus-5.5") (modelID . "claude-opus-5.5") (providerID . "github-copilot")
      (name . "Claude Opus 5.5") (package . "@opencode/ai/providers/anthropic")
      (settings (baseURL . "https://api.githubcopilot.com/v1") (endpoint . "messages"))
      (variants . ,(drover-opencode-fixtures-claude-variants "low" "medium" "high")))
     ((id . "claude-opus-5-5") (modelID . "anthropic.claude-opus-5-5-v1:0")
      (providerID . "amazon-bedrock") (name . "Claude Opus 5.5")
      (package . "@opencode/ai/providers/amazon-bedrock")
      (variants . ,(drover-opencode-fixtures-bedrock-claude-variants "low" "medium" "high")))))
  "Model catalog: three OpenAI and OpenCode entries recorded in opencode.el's
test/fixtures/v2/model.json, trimmed, and Claude entries shaped like the
variants OpenCode's variant.ts gives them.")

(defconst drover-opencode-fixtures-agents
  (drover-opencode-fixtures-json
   "[{\"id\": \"build\", \"name\": \"Build\", \"mode\": \"primary\", \"hidden\": false},
     {\"id\": \"general\", \"name\": \"General\", \"mode\": \"subagent\", \"hidden\": false},
     {\"id\": \"compaction\", \"name\": \"Compaction\", \"mode\": \"primary\", \"hidden\": true},
     {\"id\": \"plan\", \"name\": \"Plan\", \"mode\": \"primary\", \"hidden\": false,
      \"model\": {\"id\": \"gpt-6.1-sol-fast\", \"providerID\": \"openai\", \"variant\": \"high\"}}]")
  "Agent catalog: agents recorded in opencode.el's test/fixtures/v2/agent.json,
the plan agent pinning a model and variant.")

(defun drover-opencode-fixtures-model-info (provider-id id)
  "Return the catalog entry of PROVIDER-ID's model ID."
  (seq-find (lambda (model)
              (and (equal (alist-get 'providerID model) provider-id)
                   (equal (alist-get 'id model) id)))
            drover-opencode-fixtures-models))

(defun drover-opencode-fixtures-model (provider-id id &optional settings)
  "Return the `drover-model' of PROVIDER-ID's model ID, with model SETTINGS added.
SETTINGS are merged over the catalog entry's, as the user's
configuration of the model would be."
  (let ((info (copy-alist (drover-opencode-fixtures-model-info provider-id id))))
    (when settings
      (setf (alist-get 'settings info)
            (drover-merge-settings (alist-get 'settings info) settings)))
    (drover-opencode-model info drover-opencode-fixtures-providers)))

(defun drover-opencode-fixtures-agent-listing (&optional default)
  "Return the agent catalog as the service lists it, DEFAULT first.
DEFAULT stands for the agent configured as `default_agent'."
  (if-let* ((entry (seq-find (lambda (agent) (equal (alist-get 'id agent) default))
                             drover-opencode-fixtures-agents)))
      (cons entry (remq entry drover-opencode-fixtures-agents))
    drover-opencode-fixtures-agents))

(defun drover-opencode-fixtures-defaults (&optional default)
  "Return the engine's defaults data of the catalogs, listing agent DEFAULT first.
The default model is Big Pickle."
  (drover-opencode--defaults-from
   (drover-opencode-fixtures-agent-listing default) drover-opencode-fixtures-models
   drover-opencode-fixtures-providers
   (drover-opencode-fixtures-model-info "opencode" "big-pickle")))

(defun drover-opencode-fixtures-catalog-model (defaults provider-id id)
  "Return the `drover-model' of PROVIDER-ID's model ID in the catalog of DEFAULTS."
  (seq-find (lambda (model)
              (and (equal (drover-model-provider-id model) provider-id)
                   (equal (drover-model-id model) id)))
            (plist-get defaults :catalog)))

(defun drover-opencode-fixtures-primary-agents (&optional _directory)
  "Stand in for `opencode-primary-agents' over the agent catalog."
  (seq-filter #'opencode-agent-selectable-p drover-opencode-fixtures-agents))

(defun drover-opencode-fixtures-recorded (name)
  "Return opencode.el's recorded v2 response NAME.
The test is skipped when opencode.el's tests are not installed."
  (let* ((library (locate-library "opencode"))
         (file (and library
                    (expand-file-name (concat "test/fixtures/v2/" name)
                                      (file-name-directory (file-truename library))))))
    (unless (and file (file-readable-p file))
      (ert-skip (format "opencode.el's recorded %s is not installed" name)))
    (with-temp-buffer
      (insert-file-contents file)
      (drover-opencode-fixtures-json (buffer-string)))))

(provide 'drover-opencode-fixtures)
;;; drover-opencode-fixtures.el ends here
