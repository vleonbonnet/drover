;;; drover-codex-test.el --- Tests of the Codex engine  -*- lexical-binding: t; -*-

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

;; Every test reads Codex's local state from the throwaway homes of
;; `drover-test-with-homes', never the user's.

;;; Code:

(require 'drover-test-helpers)
(require 'drover-codex)

(defconst drover-codex-test-models
  "{\"models\": [{\"slug\": \"gpt-a\", \"display_name\": \"GPT-A\",
                  \"default_reasoning_level\": \"medium\",
                  \"supported_reasoning_levels\": [{\"effort\": \"low\"},
                                                   {\"effort\": \"medium\"},
                                                   {\"effort\": \"high\"}]},
                 {\"slug\": \"gpt-b\", \"display_name\": \"GPT-B\",
                  \"default_reasoning_level\": \"low\"}]}"
  "Codex's models cache.")

(defun drover-codex-test-write-config (content)
  "Write CONTENT as Codex's config.toml."
  (drover-test-write (expand-file-name "config.toml" drover-codex-home) content))

(ert-deftest drover-codex-test-startup-defaults ()
  "Codex starts on config.toml's model, the selected profile first."
  (drover-test-with-homes
    (drover-test-write (expand-file-name "models_cache.json" drover-codex-home) drover-codex-test-models)
    (drover-codex-test-write-config
     "model = \"gpt-a\" # comment\n[projects.'C:\\x']\nmodel = \"ignored\"\n")
    (let* ((defaults (drover-codex-defaults))
           (model (plist-get defaults :model)))
      (should (equal (drover-model-label model) "OpenAI: GPT-A"))
      (should (equal (drover-model-variants model) '((low) (medium) (high))))
      (should (eq (drover-model-default-variant model) 'medium))
      (should (equal (drover-model-engines model) '(codex)))
      (should (drover-model-runs-on-p model 'codex))
      (should (eq (drover-codex-effort "gpt-b" (plist-get defaults :config)) 'low)))
    (drover-codex-test-write-config
     (concat "profile = \"deep\"\nmodel = \"gpt-a\"\n"
             "model_reasoning_effort = \"low\"\n"
             "[profiles.deep]\nmodel = \"gpt-b\"\n"
             "model_reasoning_effort = \"high\"\n"))
    (let ((model (plist-get (drover-codex-defaults) :model)))
      (should (equal (drover-model-id model) "gpt-b"))
      (should (eq (drover-model-default-variant model) 'high)))))

(ert-deftest drover-codex-test-config ()
  "Only plain strings are read; tables other than the profile's are set aside."
  (drover-test-with-homes
    (should-not (drover-codex-config))
    (should-not (plist-get (drover-codex-defaults) :model))
    (drover-codex-test-write-config
     (concat "# model = \"commented\"\n"
             "model_provider = 'azure'\n"
             "approval_policy = \"never\"\n"
             "max_tokens = 100\n"
             "profile = \"fast lane\"\n"
             "[[mcp_servers.list]]\nmodel = \"in an array\"\n"
             "[profiles.\"fast lane\"]\nmodel = \"o4-mini\"\n"
             "[profiles.other]\nmodel = \"gpt-d\"\n"))
    (should (equal (drover-codex-config)
                   '(("model" . "o4-mini")
                     ("profile" . "fast lane")
                     ("approval_policy" . "never")
                     ("model_provider" . "azure"))))
    (let ((model (plist-get (drover-codex-defaults) :model)))
      (should (equal (drover-model-id model) "o4-mini"))
      ;; A provider other than OpenAI names itself; without a cache
      ;; entry, the model has no name nor default, and unknown variants.
      (should (equal (drover-model-label model) "azure: o4-mini"))
      (should-not (drover-model-variants model))
      (should-not (drover-model-variants-known model))
      (should-not (drover-model-default-variant model))
      ;; Codex runs it, whatever family its name tells.
      (should (equal (drover-model-engines model) '(codex))))))

(ert-deftest drover-codex-test-effort ()
  "The configured effort wins over the model's default in the cache."
  (drover-test-with-homes
    (should-not (drover-codex-effort "gpt-a" nil))
    (drover-test-write (expand-file-name "models_cache.json" drover-codex-home) drover-codex-test-models)
    (should (eq (drover-codex-effort "gpt-a" nil) 'medium))
    (should (eq (drover-codex-effort "gpt-a" '(("model_reasoning_effort" . "xhigh"))) 'xhigh))
    (should-not (drover-codex-effort "gpt-z" nil))))

(ert-deftest drover-codex-test-acp-model-efforts ()
  "The model identifiers Codex's ACP agent lists apart tell each model's levels."
  (should (equal (drover-codex-acp-model-efforts
                  '("gpt-5.5[low]" "gpt-5.5[medium]" "gpt-6-astra[ultra]" "gpt-5.5[xhigh]"
                    "gpt-5.5[low]" "plain" nil "[low]"))
                 '(("gpt-5.5" low medium xhigh) ("gpt-6-astra" ultra))))
  (should-not (drover-codex-acp-model-efforts nil)))

(ert-deftest drover-codex-test-acp-models ()
  "A Codex session's models take their own levels, the one it runs those it offers."
  (drover-test-with-homes
    (drover-test-write (expand-file-name "models_cache.json" drover-codex-home) drover-test-codex-models-cache)
    ;; As codex-acp 1.13 lists them: model values, names without "gpt-",
    ;; and apart, every model with every level.
    (let* ((options '(("gpt-5.5" . "5.5") ("gpt-6-astra" . "6 Astra") ("gpt-next" . "Next")))
           (efforts (drover-codex-acp-model-efforts
                     '("gpt-5.5[low]" "gpt-5.5[medium]" "gpt-5.5[high]" "gpt-5.5[xhigh]"
                       "gpt-6-astra[low]" "gpt-6-astra[max]")))
           (models (drover-codex-acp-models options "gpt-6-astra" '(low medium high) efforts)))
      (should (equal (mapcar #'drover-model-label models)
                     '("OpenAI: GPT-5.5" "OpenAI: GPT-6-Astra" "OpenAI: Next")))
      (should (equal (mapcar (lambda (model) (mapcar #'car (drover-model-variants model))) models)
                     '((low medium high xhigh) (low medium high) nil)))
      (should (equal (mapcar #'drover-model-variants-known models) '(t t nil)))
      (should (equal (mapcar #'drover-model-default-variant models) '(xhigh low nil)))
      (should (equal (drover-model-engines (car models)) '(codex)))
      ;; Without levels listed apart, the cache's.
      (let ((models (drover-codex-acp-models options "gpt-next" nil nil)))
        (should (equal (mapcar (lambda (model) (mapcar #'car (drover-model-variants model))) models)
                       '((low medium high xhigh) (low medium high xhigh max ultra) nil)))
        ;; The session runs a model without levels: known to have none.
        (should (equal (mapcar #'drover-model-variants-known models) '(t t t))))
      ;; The same model as the engine's defaults describe it.
      (drover-codex-test-write-config "model = \"gpt-5.5\"\n")
      (should (equal (plist-get (drover-codex-defaults) :model)
                     (car (drover-codex-acp-models options nil nil nil)))))))

(ert-deftest drover-codex-test-registered ()
  "Loading the file registers the engine."
  (should (memq 'codex (drover-engines)))
  (should (equal (drover-engine-label 'codex) "Codex"))
  (should (drover-engine-property 'codex :volatile))
  (should-not (drover-engine-property 'codex :agents))
  (should-not (drover-engine-property 'codex :model-resets-variant))
  (drover-test-with-engines
    (drover-codex--register)
    (should (equal (drover-engines) '(codex)))
    (drover-codex-unload-function)
    (should-not (drover-engines))))

(ert-deftest drover-codex-test-defaults-callback ()
  "The engine's defaults come at once; a failure is warned about, without callback."
  (drover-test-with-homes
    (drover-codex-test-write-config "model = \"gpt-a\"\n")
    (let (delivered warnings)
      (funcall (drover-engine-property 'codex :defaults) "/any/"
               (lambda (defaults) (push defaults delivered)))
      (should (= (length delivered) 1))
      (should (equal (drover-model-id (plist-get (car delivered) :model)) "gpt-a"))
      (should (equal (plist-get (car delivered) :config) '(("model" . "gpt-a"))))
      (setq delivered nil)
      (cl-letf (((symbol-function 'drover-codex-config) (lambda () (error "Unreadable")))
                ((symbol-function 'display-warning)
                 (lambda (type message &rest _) (push (list type message) warnings))))
        (drover-codex--fetch-defaults "/any/" (lambda (defaults) (push defaults delivered))))
      (should-not delivered)
      (should (equal (caar warnings) 'drover))
      (should (string-match-p "Codex.s defaults.*Unreadable" (cadar warnings))))))

(ert-deftest drover-codex-test-resolve ()
  "Explicit choices win; else config.toml's model and the model's effort."
  (drover-test-with-homes
    (drover-test-write (expand-file-name "models_cache.json" drover-codex-home) drover-codex-test-models)
    (let* ((gpt-a (drover-model-create :id "gpt-a" :engines '(codex)))
           (gpt-b (drover-model-create :id "gpt-b" :engines '(codex)))
           (defaults (list :model gpt-a :config '(("model_reasoning_effort" . "high")))))
      (should (equal (drover-engine-resolve 'codex defaults nil nil nil)
                     (list :agent nil :model gpt-a :variant 'high)))
      (should (equal (drover-engine-resolve 'codex defaults "reviewer" gpt-b 'low)
                     (list :agent "reviewer" :model gpt-b :variant 'low)))
      (should (equal (drover-engine-resolve 'codex (list :model gpt-a :config nil) nil gpt-b nil)
                     (list :agent nil :model gpt-b :variant 'low)))
      (should (equal (drover-engine-resolve 'codex nil nil nil nil)
                     (list :agent nil :model nil :variant nil))))))

(ert-deftest drover-codex-test-last-used ()
  "The newest rollout of any day tells when Codex ran last."
  (drover-test-with-homes
    (should-not (drover-codex--last-used))
    (let ((old (expand-file-name "sessions/2026/10/02/rollout-a.jsonl" drover-codex-home))
          (new (expand-file-name "sessions/2026/09/30/rollout-b.jsonl" drover-codex-home))
          (other (expand-file-name "sessions/2026/10/02/notes.jsonl" drover-codex-home)))
      (drover-test-write old "{}\n")
      (drover-test-write new "{}\n")
      (drover-test-write other "{}\n")
      (set-file-times old (seconds-to-time 1000000))
      (set-file-times new (seconds-to-time 2000000))
      (set-file-times other (seconds-to-time 3000000))
      ;; A session outliving its day keeps writing under the day it began.
      (should (= (drover-codex--last-used) 2000000.0))
      (should (= (funcall (drover-engine-property 'codex :last-used)) 2000000.0)))))

(ert-deftest drover-codex-test-last-used-engine ()
  "The engine whose own records changed last is the one used last."
  (drover-test-with-homes
    (drover-test-with-backend nil
      (drover-test-with-engines
        (drover-claude-code--register)
        (drover-codex--register)
        (should-not (drover-last-used-engine))
        (let ((history (expand-file-name "history.jsonl" drover-claude-code-home))
              (rollout (expand-file-name "sessions/2026/10/02/rollout-a.jsonl" drover-codex-home)))
          (drover-test-write history "{}\n")
          (drover-test-write rollout "{}\n")
          (set-file-times history (seconds-to-time 1000000))
          (set-file-times rollout (seconds-to-time 2000000))
          (should (eq (drover-last-used-engine) 'codex))
          (set-file-times history (seconds-to-time 3000000))
          (should (eq (drover-last-used-engine) 'claude-code)))))))

;;;; Where conversations ran

(ert-deftest drover-codex-test-session-directory ()
  "A conversation's directory is the cwd its rollout begins with, archived or not."
  (drover-test-with-homes
    (let ((id "019a0b1c-2d3e-7f40-8123-456789abcdef")
          (meta (lambda (cwd)
                  (concat "{\"timestamp\":\"2026-10-31T12:00:00Z\",\"type\":\"session_meta\","
                          "\"payload\":{\"id\":\"x\",\"cwd\":\"" cwd "\",\"instructions\":\""
                          (make-string 70000 ?i) "\"}}\n{\"type\":\"event_msg\"}\n"))))
      (should-not (drover-codex-session-directory id))
      (drover-test-write (expand-file-name (concat "sessions/2026/10/31/rollout-2026-10-31T12-00-00-" id ".jsonl")
                                           drover-codex-home)
                         (funcall meta "/work/drover"))
      (should (equal (drover-codex-session-directory id) "/work/drover/"))
      (let (found failed)
        (drover-codex--directory id (lambda (directory) (setq found directory)) #'ignore)
        (should (equal found "/work/drover/"))
        (drover-codex--directory "unknown" #'ignore (lambda (message) (setq failed message)))
        (should (string-match-p "No rollout" failed)))
      (drover-test-write (expand-file-name "archived_sessions/rollout-2026-10-30T12-00-00-other.jsonl"
                                           drover-codex-home)
                         (funcall meta "/work/old"))
      (should (equal (drover-codex-session-directory "other") "/work/old/"))
      (should (eq (drover-engine-property 'codex :directory) #'drover-codex--directory)))))

(provide 'drover-codex-test)
;;; drover-codex-test.el ends here
