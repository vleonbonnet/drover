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
      ;; entry, the model has no name, variants nor default.
      (should (equal (drover-model-label model) "azure: o4-mini"))
      (should-not (drover-model-variants model))
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

(provide 'drover-codex-test)
;;; drover-codex-test.el ends here
