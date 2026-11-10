;;; drover-model-test.el --- Tests of engines and models  -*- lexical-binding: t; -*-

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

;;; Code:

(require 'drover-test-helpers)
(require 'drover-model)

(defmacro drover-model-test-reading (answers &rest body)
  "Run BODY answering `completing-read' with ANSWERS in turn.
Each call is recorded in `reads', newest first, as (PROMPT CANDIDATES
METADATA DEFAULT): CANDIDATES all the completions, METADATA the
table's metadata."
  (declare (indent 1) (debug t))
  `(let ((answers ,answers)
         (reads nil))
     (cl-letf (((symbol-function 'completing-read)
                (lambda (prompt table _predicate _require-match _initial _history default &rest _)
                  (push (list prompt (all-completions "" table) (completion-metadata "" table nil) default)
                        reads)
                  (pop answers))))
       ,@body)))

;;;; Models

(ert-deftest drover-model-test-engines-for ()
  "Qualified engines run any provider's model; the others their family's."
  (should (equal (drover-model-engines-for "anthropic" "claude-opus-5-5") '(opencode claude-code)))
  (should (equal (drover-model-engines-for "amazon-bedrock" "anthropic.claude-opus-5-5-v1:0")
                 '(opencode claude-code)))
  (should (equal (drover-model-engines-for "openai" "gpt-5.5") '(opencode codex)))
  (should (equal (drover-model-engines-for "github-copilot" "gemini-3-pro") '(opencode gemini-cli)))
  (should (equal (drover-model-engines-for "openrouter" "some-model") '(opencode)))
  (should (equal (drover-model-engines-for nil "claude-opus-5-5") '(claude-code)))
  (should (equal (drover-model-engines-for nil "GPT-5.5") '(codex)))
  (should-not (drover-model-engines-for nil "llama-4"))
  (let ((drover-model-qualified-engines '(opencode other)))
    (should (equal (drover-model-engines-for "x" "y") '(opencode other)))))

(ert-deftest drover-model-test-id-for ()
  (let ((qualified (drover-model-create :provider-id "anthropic" :id "claude-opus-5-5"
                                        :engines '(opencode claude-code)))
        (bare (drover-model-create :id "claude-opus-5-5" :engines '(claude-code opencode))))
    (should (drover-model-runs-on-p qualified 'opencode))
    (should-not (drover-model-runs-on-p qualified 'codex))
    (should-not (drover-model-runs-on-p nil 'opencode))
    (should (equal (drover-model-id-for qualified 'opencode) "anthropic/claude-opus-5-5"))
    (should (equal (drover-model-id-for qualified 'claude-code) "claude-opus-5-5"))
    (should-not (drover-model-id-for qualified 'codex))
    ;; A qualified engine needs a provider.
    (should-not (drover-model-id-for bare 'opencode))
    (should (equal (drover-model-id-for bare 'claude-code) "claude-opus-5-5"))))

(ert-deftest drover-model-test-id-for-front-end ()
  "A front end listing a model under another name is sent that name, others the identifier."
  (let ((opus (drover-model-create :id "claude-opus-5-5"
                                   :aliases '(((claude-code . agent-shell) . "opus"))
                                   :engines '(claude-code))))
    (should (equal (drover-model-alias opus 'claude-code 'agent-shell) "opus"))
    (should-not (drover-model-alias opus 'claude-code 'claude-code-ide))
    (should (equal (drover-model-id-for opus 'claude-code 'agent-shell) "opus"))
    (should (equal (drover-model-id-for opus 'claude-code 'claude-code-ide) "claude-opus-5-5"))
    (should (equal (drover-model-id-for opus 'claude-code) "claude-opus-5-5"))
    ;; An engine that cannot run it is sent nothing, alias or not.
    (should-not (drover-model-id-for opus 'codex 'agent-shell))
    (should (equal (drover-model-key opus) '(nil . "claude-opus-5-5")))))

(ert-deftest drover-model-test-merge ()
  "A model takes from another report of it what it does not know."
  (let* ((older (drover-model-create :provider-name "Anthropic" :id "claude-opus-5-5" :name "Opus 5.5"
                                     :variants '((low) (max)) :default-variant 'low
                                     :aliases '(((claude-code . agent-shell) . "opus")
                                                ((claude-code . other) . "o"))
                                     :engines '(claude-code)))
         (unknown (drover-model-create :id "claude-opus-5-5" :variants-known nil
                                       :aliases '(((claude-code . other) . "opus-5.5"))
                                       :engines '(claude-code)))
         (merged (drover-model-merge unknown older)))
    (should (equal (drover-model-variants merged) '((low) (max))))
    (should (eq (drover-model-default-variant merged) 'low))
    (should (drover-model-variants-known merged))
    (should (equal (drover-model-label merged) "Anthropic: Opus 5.5"))
    (should (equal (drover-model-aliases merged)
                   '(((claude-code . other) . "opus-5.5") ((claude-code . agent-shell) . "opus"))))
    ;; Neither is modified.
    (should-not (drover-model-variants-known unknown))
    (should (equal (drover-model-aliases unknown) '(((claude-code . other) . "opus-5.5"))))
    ;; Known variants, none included, stay.
    (let ((none (drover-model-create :id "claude-opus-5-5" :engines '(claude-code))))
      (should-not (drover-model-variants (drover-model-merge none older))))
    ;; Reported twice in one list: one model, at its first place.
    (let ((other (drover-model-create :id "other" :engines '(claude-code))))
      (should (equal (mapcar #'drover-model-id (drover-model-merge-all (list unknown other older)))
                     '("claude-opus-5-5" "other")))
      (should (drover-model-variants-known (car (drover-model-merge-all (list unknown other older))))))))

(ert-deftest drover-model-test-unknown-variants ()
  "Variants no source told are unknown: pickers offer none, and say so."
  (let ((model (drover-model-create :id "m" :variants-known nil)))
    (should (equal (drover-model-annotation model) "  m  {effort unknown}"))
    (drover-model-test-reading '("low")
      (should-not (drover-read-variant model))
      (should-not reads))))

(ert-deftest drover-model-test-short-name ()
  "Lists name a model without the family every model of it shares."
  (should (equal (drover-model-short-name "Claude Opus 5.5") "Opus 5.5"))
  (should (equal (drover-model-short-name "GPT-5.5") "GPT-5.5"))
  (should (equal (drover-model-short-name "Claude") "Claude"))
  (should-not (drover-model-short-name nil)))

(ert-deftest drover-model-test-labels ()
  (let ((named (drover-model-create :provider-id "openai" :provider-name "OpenAI" :id "gpt-5.5"
                                    :name "GPT-5.5" :variants '((low :effort "low") (high :effort "high"))
                                    :default-variant 'high))
        (bare (drover-model-create :provider-id "openai" :id "gpt-5.5-mini" :variants '((low) (high))))
        (plain (drover-model-create :id "claude-opus-5-5")))
    (should (equal (drover-model-label named) "OpenAI: GPT-5.5"))
    (should (equal (drover-model-label bare) "openai: gpt-5.5-mini"))
    (should (equal (drover-model-label plain) "claude-opus-5-5"))
    (should (equal (drover-model-annotation named) "  gpt-5.5  {low,high} default:high"))
    (should (equal (drover-model-annotation bare) "  gpt-5.5-mini  {low,high} default:provider"))
    (should (equal (drover-model-annotation plain) "  claude-opus-5-5"))
    (should (eq (get-text-property 0 'face (drover-model-annotation plain)) 'completions-annotations))
    ;; A label taken twice is told apart by the identifier.
    (let ((twin (drover-model-create :provider-id "openai" :provider-name "OpenAI" :id "gpt-5.5-2026"
                                     :name "GPT-5.5")))
      (should (equal (mapcar #'car (drover-model-candidates (list named twin plain)))
                     '("OpenAI: GPT-5.5" "OpenAI: GPT-5.5 [gpt-5.5-2026]" "claude-opus-5-5")))
      (should (eq (cdr (assoc "OpenAI: GPT-5.5" (drover-model-candidates (list named twin)))) named)))))

(ert-deftest drover-model-test-effort-candidates ()
  "A variant offers the effort it sends, then its name."
  (let ((model (drover-model-create :id "m" :variants '((high :effort "high") (max :effort "xhigh") (low)))))
    (should (equal (drover-model-effort-candidates model 'high) '("high" "high")))
    (should (equal (drover-model-effort-candidates model 'max) '("xhigh" "max")))
    (should (equal (drover-model-effort-candidates model 'low) '("low")))
    (should (equal (drover-model-effort-candidates model 'other) '("other")))
    (should (equal (drover-model-effort-candidates nil 'max) '("max")))
    (should-not (drover-model-effort-candidates model nil))))

(ert-deftest drover-model-test-anthropic-key ()
  "Claude model identifiers give their family and version, whichever order they use."
  (should (equal (drover-model-anthropic-key "claude-opus-5-5") "opus-5-5"))
  (should (equal (drover-model-anthropic-key "claude-opus-5.5-fast") "opus-5-5"))
  (should (equal (drover-model-anthropic-key "anthropic.claude-opus-5-5-v1:0") "opus-5-5"))
  (should (equal (drover-model-anthropic-key "claude-3-7-sonnet-20250219") "sonnet-3-7"))
  (should (equal (drover-model-anthropic-key "claude-sonnet-4-20250514") "sonnet-4"))
  (should (equal (drover-model-anthropic-key "claude-mythos-preview") "mythos-preview"))
  (should-not (drover-model-anthropic-key "gpt-5.5")))

;;;; Engines

(ert-deftest drover-model-test-engine-registry ()
  (drover-test-with-engines
    (should-not (drover-engines))
    (drover-test-register-engine 'zeta)
    (drover-test-register-engine 'codex :label "Codex")
    (drover-test-register-engine 'alpha)
    (drover-test-register-engine 'opencode)
    ;; The known order first, then the others as they registered.
    (should (equal (drover-engines) '(opencode codex zeta alpha)))
    (should (equal (drover-engine-label 'codex) "Codex"))
    (should (equal (drover-engine-label 'zeta) "Zeta"))
    (should (equal (drover-engine-label 'unknown) "unknown"))
    (should-not (drover-engine-property 'unknown :label))
    ;; Registering again replaces the properties and keeps the place.
    (drover-register-engine 'zeta :label "Z")
    (should (equal (drover-engine-label 'zeta) "Z"))
    (should-not (drover-engine-property 'zeta :agents))
    (should (equal (drover-engines) '(opencode codex zeta alpha)))
    (drover-register-model-source 'zeta #'ignore)
    (drover-register-model-source 'zeta #'identity -1)
    (should (equal (drover-engine-model-sources 'zeta) '(identity ignore)))
    ;; Forgetting an engine leaves the sources, which their files remove.
    (drover-unregister-engine 'zeta)
    (should (equal (drover-engines) '(opencode codex alpha)))
    (should (equal (drover-engine-model-sources 'zeta) '(identity ignore)))
    (drover-unregister-model-source 'zeta #'ignore)
    (drover-unregister-model-source 'zeta #'identity)
    (should-not drover--model-sources)))

(ert-deftest drover-model-test-build-agent ()
  "An engine's build agent is a string, or a function's answer; nil otherwise."
  (drover-test-with-engines
    (drover-test-register-engine 'opencode :build-agent "build")
    (drover-test-register-engine 'claude-code :build-agent (lambda () "build"))
    (drover-test-register-engine 'codex)
    (drover-test-register-engine 'zeta :build-agent (lambda () ""))
    (drover-test-register-engine 'alpha :build-agent (lambda () (error "Broken")))
    (should (equal (drover-engine-build-agent 'opencode) "build"))
    (should (equal (drover-engine-build-agent 'claude-code) "build"))
    (should-not (drover-engine-build-agent 'codex))
    (should-not (drover-engine-build-agent 'zeta))
    (should-not (drover-engine-build-agent 'unknown))
    (cl-letf (((symbol-function 'drover-report-failure) #'ignore))
      (should-not (drover-engine-build-agent 'alpha)))))

(ert-deftest drover-model-test-claude-code-build-agent ()
  "Claude Code builds while `drover-claude-code-agent-modes' runs build as a mode."
  (require 'drover-claude-code)
  (let ((drover-claude-code-agent-modes '(("plan" "plan") ("build" "auto"))))
    (should (equal (drover-engine-build-agent 'claude-code) "build")))
  (let ((drover-claude-code-agent-modes '(("plan" "plan"))))
    (should-not (drover-engine-build-agent 'claude-code))))

(ert-deftest drover-model-test-fake-engine ()
  "Fake engines deliver their defaults and record the calls, as tests expect."
  (drover-test-with-engines
    (drover-test-register-engine 'fake :volatile t)
    (let ((delivered nil))
      (funcall (drover-engine-property 'fake :defaults) "/work/" (lambda (data) (push data delivered)))
      (should-not delivered)
      (setq drover-test-engine-defaults '((fake :model "m")))
      (funcall (drover-engine-property 'fake :defaults) "/work/" (lambda (data) (push data delivered)))
      (should (equal delivered '((:model "m"))))
      (should (drover-engine-property 'fake :volatile))
      (should (equal drover-test-engine-calls '((:defaults fake "/work/") (:defaults fake "/work/")))))))

(ert-deftest drover-model-test-engine-resolve ()
  (drover-test-with-engines
    (let ((model (drover-test-model "m")))
      (drover-test-register-engine 'plain)
      (drover-test-register-engine 'resolving
                                   :resolve (lambda (defaults agent model variant)
                                              (list :agent (or agent (plist-get defaults :agent))
                                                    :model model :variant (or variant 'default))))
      (should (equal (drover-engine-resolve 'plain '(:agent "build") "plan" model nil)
                     (list :agent "plan" :model model :variant nil)))
      (should (equal (drover-engine-resolve 'unknown nil nil nil 'high)
                     '(:agent nil :model nil :variant high)))
      (should (equal (drover-engine-resolve 'resolving '(:agent "build") nil model nil)
                     (list :agent "build" :model model :variant 'default))))))

(ert-deftest drover-model-test-last-used-engine ()
  "The engine that took a prompt last; one failing counts as unused."
  (drover-test-with-engines
    (should-not (drover-last-used-engine))
    (drover-test-register-engine 'opencode)
    (drover-test-register-engine 'claude-code)
    (drover-test-register-engine 'codex)
    (drover-test-register-engine 'broken :last-used (lambda () (error "No database")))
    (should-not (drover-last-used-engine))
    (setq drover-test-engine-last-used '((claude-code . 1000000.0) (codex . 2000000.0)))
    (should (eq (drover-last-used-engine) 'codex))
    (setf (alist-get 'claude-code drover-test-engine-last-used) 3000000.0)
    (should (eq (drover-last-used-engine) 'claude-code))
    (setf (alist-get 'opencode drover-test-engine-last-used) 4000000.0)
    (should (eq (drover-last-used-engine) 'opencode))
    ;; A tie goes to the engine listed first.
    (setf (alist-get 'codex drover-test-engine-last-used) 4000000.0)
    (should (eq (drover-last-used-engine) 'opencode))))

(ert-deftest drover-model-test-agent-names ()
  "The engines' agents, OpenCode's first, once each."
  (drover-test-with-engines
    (drover-test-register-engine 'claude-code)
    (drover-test-register-engine 'opencode)
    (drover-test-register-engine 'broken :agents (lambda (_) (error "No service")))
    (drover-test-register-engine 'mute :agents nil)
    (setq drover-test-engine-agents '((claude-code "plan" "build") (opencode "build" "plan" "review")))
    (should (equal (drover-agent-names "/work/") '("build" "plan" "review")))
    (should (member '(:agents opencode "/work/") drover-test-engine-calls))
    (should (member '(:agents claude-code "/work/") drover-test-engine-calls))
    (setq drover-test-engine-agents nil)
    (should-not (drover-agent-names "/work/"))))

;;;; Model sources

(ert-deftest drover-model-test-sources-first-answer ()
  "Each engine answers through its first source returning models."
  (drover-test-with-engines
    (let* ((calls nil)
           (source (lambda (name models)
                     (lambda (directory) (push (cons name directory) calls) models)))
           (live (drover-model-create :id "opus" :engines '(claude-code)))
           (oauth (drover-model-create :id "claude-opus-5-5" :engines '(claude-code)))
           (catalog (drover-model-create :provider-id "anthropic" :id "claude-opus-5-5"
                                         :engines '(opencode claude-code)))
           (shell (drover-model-create :provider-id "openai" :id "gpt-5.5" :engines '(opencode codex)))
           (codex (drover-model-create :id "gpt-5.5" :engines '(codex)))
           (oauth-source (funcall source 'oauth (list oauth)))
           (live-source (funcall source 'live nil)))
      (drover-register-model-source 'opencode (funcall source 'shell (list shell)) 10)
      (drover-register-model-source 'opencode (funcall source 'catalog (list catalog)))
      (drover-register-model-source 'claude-code oauth-source)
      (drover-register-model-source 'claude-code live-source -10)
      (drover-register-model-source 'codex (funcall source 'codex (list codex)))
      (drover-register-model-source 'other (lambda (_) (error "Broken")))
      (let ((default-directory "/work/"))
        ;; Claude Code first, then Codex, then OpenCode; a failing source
        ;; answers nothing.
        (should (equal (drover-models) (list oauth codex catalog))))
      (should (equal (reverse calls)
                     '((live . "/work/") (oauth . "/work/") (codex . "/work/") (catalog . "/work/"))))
      ;; A source answering stops the search.
      (setq calls nil)
      (drover-register-model-source 'claude-code (funcall source 'live (list live)) -10)
      (drover-unregister-model-source 'claude-code live-source)
      (should (equal (drover-models "/elsewhere/") (list live codex catalog)))
      (should (equal (mapcar #'car (reverse calls)) '(live codex catalog)))
      ;; Without its catalog, OpenCode takes the next source.
      (dolist (function (drover-engine-model-sources 'claude-code))
        (drover-unregister-model-source 'claude-code function))
      (drover-unregister-engine 'claude-code)
      (setq drover--model-sources (seq-remove (lambda (s) (eq (car s) 'opencode)) drover--model-sources))
      (drover-register-model-source 'opencode (funcall source 'shell (list shell)) 10)
      (drover-register-model-source 'opencode (funcall source 'catalog nil))
      (should (equal (drover-models "/work/") (list codex shell))))))

(ert-deftest drover-model-test-sources-dedupe ()
  "A model reported twice is the first report, in the order of engines."
  (drover-test-with-engines
    (let ((first (drover-model-create :provider-id "anthropic" :id "claude-opus-5-5" :name "First"))
          (second (drover-model-create :provider-id "anthropic" :id "claude-opus-5-5" :name "Second"))
          (bare (drover-model-create :id "claude-opus-5-5" :name "Bare")))
      (drover-register-model-source 'opencode (lambda (_) (list second)))
      (drover-register-model-source 'claude-code (lambda (_) (list first bare)))
      (should (equal (mapcar #'drover-model-name (drover-models "/")) '("First" "Bare")))
      (let ((drover-model-engine-order '(opencode)))
        (should (equal (mapcar #'drover-model-name (drover-models "/")) '("Second" "Bare"))))
      ;; Engines outside the order come after it.
      (drover-register-model-source 'extra (lambda (_) (list (drover-model-create :id "x"))))
      (should (equal (mapcar #'drover-model-id (drover-models "/"))
                     '("claude-opus-5-5" "claude-opus-5-5" "x"))))))

(ert-deftest drover-model-test-keeping-models ()
  "While models are kept, each engine's are made once per directory, until they change."
  (drover-test-with-engines
    (let* ((calls nil)
           (opus (drover-model-create :id "claude-opus-5-5" :engines '(claude-code)))
           (fable (drover-model-create :id "claude-fable-5-1" :engines '(claude-code)))
           (models (list opus)))
      (drover-register-model-source 'claude-code (lambda (directory) (push directory calls) models))
      ;; Made anew each time otherwise.
      (dotimes (_ 2) (drover--engine-models 'claude-code "/a/"))
      (should (equal calls '("/a/" "/a/")))
      (setq calls nil)
      (drover-keeping-models
        (dotimes (_ 3)
          (should (equal (drover--engine-models 'claude-code "/a/") (list opus)))
          (drover--engine-models 'claude-code "/b/"))
        (should (equal (reverse calls) '("/a/" "/b/")))
        ;; Nested, the same are kept.
        (drover-keeping-models
          (drover--engine-models 'claude-code "/a/"))
        (should (= (length calls) 2))
        ;; Changed models are made anew, and kept again.
        (setq models (list opus fable) calls nil)
        (let ((default-directory "/a/"))
          (drover-models-changed 'claude-code))
        (should (equal (drover--engine-models 'claude-code "/a/") (list opus fable)))
        (should (equal (drover--engine-models 'claude-code "/b/") (list opus fable)))
        (should (equal (reverse calls) '("/a/" "/b/"))))
      ;; Kept no longer once it returns.
      (setq calls nil)
      (drover--engine-models 'claude-code "/a/")
      (should (equal calls '("/a/"))))))

;;;; Remembered models

(defmacro drover-model-test-remembering (&rest body)
  "Run BODY remembering models, in a fresh home with no engine nor source.
Warnings are recorded in `warnings', newest first."
  (declare (indent 0) (debug t))
  `(drover-test-with-homes
     (drover-test-with-engines
       (let ((drover-remember-models t)
             (warnings nil))
         (cl-letf (((symbol-function 'drover-warn)
                    (lambda (format-string &rest args)
                      (push (apply #'format-message format-string args) warnings))))
           ,@body)))))

(defun drover-model-test-forget-session ()
  "Forget what this Emacs read of the remembered models, as a new session would."
  (setq drover--model-memory nil
        drover--model-memory-stamp 'unread
        drover--model-memory-warned nil))

(defun drover-model-test-read-file ()
  "Return the data of the file of remembered models, read."
  (with-temp-buffer
    (insert-file-contents (drover-model-memory-file))
    (read (current-buffer))))

(defconst drover-model-test-opus
  (drover-model-create :provider-name "Anthropic" :id "opus" :name "Opus"
                       :variants '((low) (high :effort "high")) :default-variant 'high
                       :engines '(claude-code))
  "A model a live Claude Code session offers.")

(defconst drover-model-test-gpt
  (drover-model-create :provider-id "openai" :provider-name "OpenAI" :id "gpt-6" :name "GPT-6"
                       :engines '(opencode codex))
  "A model a live OpenCode session offers.")

(ert-deftest drover-model-test-memory-written ()
  "What a live source answers is written to the state directory, as Lisp data."
  (drover-model-test-remembering
    (let ((live (list drover-model-test-opus)))
      (drover-register-model-source 'claude-code (lambda (_) live) -10 t)
      (should-not (file-exists-p (drover-model-memory-file)))
      (should (equal (drover-models "/work/") live))
      (should (file-in-directory-p (drover-model-memory-file) drover-state-directory))
      (let* ((form (drover-model-test-read-file))
             (entry (alist-get 'claude-code (plist-get form :engines))))
        (should (eql (plist-get form :version) 2))
        (should (< (- (float-time) (plist-get entry :time)) 60))
        (should (equal (plist-get entry :models)
                       '((:provider-id nil :provider-name "Anthropic" :id "opus" :name "Opus"
                          :variants ((low) (high :effort "high")) :default-variant high
                          :variants-known t :aliases nil :engines (claude-code))))))
      ;; Written whole through a temporary file: none is left.
      (should (equal (directory-files drover-state-directory nil "\\`[^.]\\|\\`\\.models-")
                     '("models.eld")))
      (with-temp-buffer
        (insert-file-contents (drover-model-memory-file))
        (should (string-match-p "-\\*- mode: lisp-data" (buffer-string))))
      ;; A source not live, such as a service's catalog, is not remembered.
      (drover-register-model-source 'opencode (lambda (_) (list drover-model-test-gpt)))
      (drover-models "/work/")
      (should-not (alist-get 'opencode (plist-get (drover-model-test-read-file) :engines)))
      (should-not warnings))))

(ert-deftest drover-model-test-memory-read-back ()
  "A new session offers what was remembered, telling the day, until a live answer."
  (drover-model-test-remembering
    (let ((live (list drover-model-test-opus)))
      (drover-register-model-source 'claude-code (lambda (_) live) -10 t)
      (drover-models "/work/")
      ;; A new Emacs session: no live session, no source even.
      (drover-model-test-forget-session)
      (setq drover--model-sources nil)
      (let ((models (drover-models "/work/")))
        (should (= (length models) 1))
        (let ((opus (car models)))
          (should (equal (drover-model-label opus) "Anthropic: Opus"))
          (should (equal (drover-model-id opus) "opus"))
          (should (equal (drover-model-variants opus) '((low) (high :effort "high"))))
          (should (eq (drover-model-default-variant opus) 'high))
          (should (equal (drover-model-engines opus) '(claude-code)))
          (should (numberp (drover-model-remembered opus)))
          (should (string-suffix-p (format-time-string "  remembered from %Y-%m-%d")
                                   (drover-model-annotation opus)))
          (should (drover-model-runs-on-p opus 'claude-code))))
      ;; Off, nothing is remembered.
      (let ((drover-remember-models nil))
        (should-not (drover-models "/work/")))
      ;; A live answer wins, and replaces what was remembered.
      (let ((sonnet (drover-model-create :id "sonnet" :engines '(claude-code))))
        (drover-register-model-source 'claude-code (lambda (_) (list sonnet)) -10 t)
        (should (equal (drover-models "/work/") (list sonnet)))
        (should-not (drover-model-remembered (car (drover-models "/work/"))))
        (should-not (string-search "remembered" (drover-model-annotation sonnet)))
        (setq drover--model-sources nil)
        (drover-model-test-forget-session)
        (should (equal (mapcar #'drover-model-id (drover-models "/work/")) '("sonnet"))))
      (should-not warnings))))

(ert-deftest drover-model-test-memory-accumulates-variants ()
  "A model's variants, told while a session runs it, stay known once it switched models."
  (drover-model-test-remembering
    (let* ((opus-levels '((low) (medium) (high) (xhigh) (max)))
           (sonnet-levels '((low) (medium) (high)))
           (live nil))
      (drover-register-model-source 'claude-code (lambda (_) live) -10 t)
      (cl-flet ((answer (opus sonnet)
                  (setq live (list (drover-model-create :id "opus" :variants opus :variants-known (and opus t)
                                                        :engines '(claude-code))
                                   (drover-model-create :id "sonnet" :variants sonnet
                                                        :variants-known (and sonnet t)
                                                        :engines '(claude-code)))))
                (variants ()
                  (mapcar (lambda (model)
                            (cons (drover-model-variants-known model) (drover-model-variants model)))
                          (drover-models "/work/"))))
        ;; The session runs Opus: Sonnet's variants are unknown, not Opus's.
        (answer opus-levels nil)
        (should (equal (variants) (list (cons t opus-levels) (list nil))))
        ;; It switched to Sonnet: Opus keeps what was told of it.
        (answer nil sonnet-levels)
        (should (equal (variants) (list (cons t opus-levels) (cons t sonnet-levels))))
        ;; So does the file, for the next Emacs session.
        (setq drover--model-sources nil)
        (drover-model-test-forget-session)
        (should (equal (variants) (list (cons t opus-levels) (cons t sonnet-levels))))
        ;; A live answer naming another set of models replaces it.
        (drover-register-model-source 'claude-code (lambda (_) live) -10 t)
        (setq live (list (drover-model-create :id "haiku" :variants-known nil :engines '(claude-code))))
        (should (equal (mapcar #'drover-model-id (drover-models "/work/")) '("haiku")))
        (setq drover--model-sources nil)
        (should (equal (mapcar #'drover-model-id (drover-models "/work/")) '("haiku"))))
      (should-not warnings))))

(ert-deftest drover-model-test-memory-session-only ()
  "Not remembering, variants still accumulate in this Emacs session, and no file is written."
  (drover-model-test-remembering
    (let ((drover-remember-models nil)
          (live (list (drover-model-create :id "opus" :variants '((low)) :engines '(claude-code))
                      (drover-model-create :id "sonnet" :variants-known nil :engines '(claude-code)))))
      (drover-register-model-source 'claude-code (lambda (_) live) -10 t)
      (drover-models "/work/")
      (setq live (list (drover-model-create :id "opus" :variants-known nil :engines '(claude-code))
                       (drover-model-create :id "sonnet" :variants '((high)) :engines '(claude-code))))
      (should (equal (mapcar #'drover-model-variants (drover-models "/work/")) '(((low)) ((high)))))
      (should-not (file-exists-p (drover-model-memory-file)))
      ;; Once the sessions are gone, nothing stands in.
      (setq drover--model-sources nil)
      (should-not (drover-models "/work/")))))

(ert-deftest drover-model-test-memory-aliases ()
  "A model keeps the name a front end takes for it, whichever source names it later."
  (drover-model-test-remembering
    (let* ((key '(claude-code . agent-shell))
           (live nil)
           (model (lambda (id &optional alias)
                    (drover-model-create :id id :aliases (and alias (list (cons key alias)))
                                         :engines '(claude-code)))))
      (drover-register-model-source 'claude-code (lambda (_) live) -10 t)
      (setq live (list (funcall model "claude-opus-5-5" "opus") (funcall model "claude-opus-5")))
      (drover-models "/work/")
      ;; A source listing identifiers alone, such as the CLI's catalog.
      (setq live (list (funcall model "claude-opus-5-5") (funcall model "claude-opus-5")))
      (should (equal (mapcar (lambda (model) (drover-model-id-for model 'claude-code 'agent-shell))
                             (drover-models "/work/"))
                     '("opus" "claude-opus-5")))
      ;; The alias moved to a newer model: the older one is sent by identifier.
      (setq live (list (funcall model "claude-opus-6" "opus") (funcall model "claude-opus-5-5")))
      (should (equal (mapcar (lambda (model) (drover-model-id-for model 'claude-code 'agent-shell))
                             (drover-models "/work/"))
                     '("opus" "claude-opus-5-5")))
      (should-not warnings))))

(ert-deftest drover-model-test-memory-version-1 ()
  "Models remembered in the first format are read, their variants unknown."
  (drover-model-test-remembering
    (drover-test-write (drover-model-memory-file)
                       (prin1-to-string
                        `(:version 1 :engines
                                   ((claude-code :time ,(float-time)
                                                 :models ((:provider-id nil :provider-name "Anthropic"
                                                           :id "opus" :name "Opus"
                                                           :variants ((low) (high)) :default-variant high
                                                           :engines (claude-code))))))))
    (let ((models (drover-models "/work/")))
      (should (equal (mapcar #'drover-model-label models) '("Anthropic: Opus")))
      ;; Version 1 gave every model the levels of the one a session ran.
      (should-not (drover-model-variants (car models)))
      (should-not (drover-model-default-variant (car models)))
      (should-not (drover-model-variants-known (car models)))
      (should (drover-model-remembered (car models))))
    (should-not warnings)
    ;; The next live answer writes the current format.
    (drover-register-model-source 'claude-code (lambda (_) (list drover-model-test-opus)) -10 t)
    (drover-models "/work/")
    (should (eql (plist-get (drover-model-test-read-file) :version) 2))))

(ert-deftest drover-model-test-memory-order ()
  "Live sources answer first, then the memory, then sources deeper than it."
  (drover-model-test-remembering
    (let ((live nil)
          (deep (list (drover-model-create :id "deep" :engines '(claude-code)))))
      (drover-register-model-source 'claude-code (lambda (_) live) -10 t)
      (drover-register-model-source 'claude-code (lambda (_) deep) (1+ drover-model-memory-depth))
      ;; Nothing remembered yet: the deeper source answers.
      (should (equal (drover-models "/work/") deep))
      (setq live (list drover-model-test-opus))
      (should (equal (drover-models "/work/") live))
      (setq live nil)
      (should (equal (mapcar #'drover-model-id (drover-models "/work/")) '("opus")))
      ;; A source above the memory, live or not, answers before it.
      (drover-register-model-source 'claude-code (lambda (_) deep) 0)
      (should (equal (drover-models "/work/") deep)))))

(ert-deftest drover-model-test-memory-written-when-changed ()
  "The file is written when the models change, or were offered another day only."
  (drover-model-test-remembering
    (let* ((live (list drover-model-test-opus))
           (writes 0)
           (write (symbol-function 'drover--model-memory-write)))
      (cl-letf (((symbol-function 'drover--model-memory-write)
                 (lambda (memory) (cl-incf writes) (funcall write memory))))
        (drover-register-model-source 'claude-code (lambda (_) live) -10 t)
        (drover-models "/work/")
        (drover-models "/work/")
        (drover-models-changed 'claude-code)
        (should (= writes 1))
        (setq live (list drover-model-test-opus (drover-model-create :id "haiku" :engines '(claude-code))))
        (drover-models "/work/")
        (should (= writes 2))
        ;; The same models, offered on a later day than remembered.
        (setf (plist-get (alist-get 'claude-code drover--model-memory) :time)
              (- (float-time) (* 2 86400)))
        (drover-models "/work/")
        (should (= writes 3))
        (drover-models "/work/")
        (should (= writes 3))))))

(ert-deftest drover-model-test-memory-changed-without-picker ()
  "A live source telling its models changed is remembered at once."
  (drover-model-test-remembering
    (let ((live nil)
          (told nil)
          (drover-models-changed-hook nil))
      (drover-register-model-source 'codex (lambda (_) live) 0 t)
      (add-hook 'drover-models-changed-hook (lambda (engine) (push engine told)))
      (setq live (list drover-model-test-gpt))
      (drover-models-changed 'codex)
      (should (equal told '(codex)))
      (should (equal (mapcar (lambda (data) (plist-get data :id))
                             (plist-get (alist-get 'codex (plist-get (drover-model-test-read-file) :engines))
                                        :models))
                     '("gpt-6"))))))

(ert-deftest drover-model-test-memory-corrupt ()
  "A file that is not remembered models is ignored with one warning, then replaced."
  (dolist (content '("(:version 1 :engines ((claude-code :time"
                     "not lisp ) data"
                     "(:version 0 :models (\"opus\"))"
                     "(:version 1 :engines ((claude-code :time \"today\" :models nil)))"
                     "(:version 1 :engines ((claude-code :time 1.0 :models ((:id nil)))))"
                     "[1 2 3]"))
    (drover-model-test-remembering
      (drover-test-write (drover-model-memory-file) content)
      (should-not (drover-models "/work/"))
      (should-not (drover-models "/work/"))
      (should (= (length warnings) 1))
      (should (string-match-p "Ignoring the models remembered in" (car warnings)))
      (drover-register-model-source 'claude-code (lambda (_) (list drover-model-test-opus)) -10 t)
      (drover-models "/work/")
      (setq drover--model-sources nil)
      (drover-model-test-forget-session)
      (should (equal (mapcar #'drover-model-id (drover-models "/work/")) '("opus")))
      (should (= (length warnings) 1)))))

(ert-deftest drover-model-test-memory-unwritable ()
  "A file that cannot be written is reported once; models are still offered."
  (drover-model-test-remembering
    (drover-test-write (expand-file-name "state" home) "a file, not a directory")
    (let ((live (list drover-model-test-opus)))
      (drover-register-model-source 'claude-code (lambda (_) live) -10 t)
      (should (equal (drover-models "/work/") live))
      (setq live (list drover-model-test-gpt))
      (should (equal (drover-models "/work/") live))
      (should (= (length warnings) 1))
      (should (string-match-p "Could not remember models in" (car warnings))))))

(ert-deftest drover-model-test-memory-shared ()
  "What another Emacs remembered is read again once the file changed."
  (drover-model-test-remembering
    (drover-register-model-source 'claude-code (lambda (_) (list drover-model-test-opus)) -10 t)
    (drover-models "/work/")
    (setq drover--model-sources nil)
    (let ((other (drover-model-test-read-file)))
      (setf (plist-get other :engines)
            (append (plist-get other :engines)
                    `((codex :time ,(float-time)
                             :models ((:id "gpt-6" :name "GPT-6" :engines (codex)))))))
      (drover-test-write (drover-model-memory-file) (prin1-to-string other))
      ;; The file changes within the second it was written in.
      (set-file-times (drover-model-memory-file) (time-add nil 5)))
    (should (equal (mapcar #'drover-model-id (drover-models "/work/")) '("opus" "gpt-6")))))

(ert-deftest drover-model-test-forget-models ()
  "Forgetting deletes the file and what this session read of it."
  (drover-model-test-remembering
    (drover-register-model-source 'claude-code (lambda (_) (list drover-model-test-opus)) -10 t)
    (drover-models "/work/")
    (setq drover--model-sources nil)
    (should (drover-models "/work/"))
    (drover-forget-models)
    (should-not (file-exists-p (drover-model-memory-file)))
    (should-not (drover-models "/work/"))))

;;;; Reading

(ert-deftest drover-model-test-read-model ()
  (drover-test-with-engines
    (let ((gpt (drover-model-create :provider-id "openai" :provider-name "OpenAI" :id "gpt-5.5"
                                    :name "GPT-5.5" :variants '((high :effort "high"))))
          (opus (drover-model-create :id "claude-opus-5-5" :name "Opus 5.5")))
      (drover-register-model-source 'opencode (lambda (_) (list gpt)))
      (drover-register-model-source 'claude-code (lambda (_) (list opus)))
      (drover-model-test-reading '("OpenAI: GPT-5.5" "Default (Opus 5.5)")
        (should (eq (drover-read-model opus "/work/") gpt))
        (should-not (drover-read-model opus "/work/"))
        (pcase-let ((`(,prompt ,candidates ,metadata ,default) (car reads)))
          (should (equal prompt "Model: "))
          ;; Default first, then the models in the order they came.
          (should (equal candidates '("Default (Opus 5.5)" "Opus 5.5" "OpenAI: GPT-5.5")))
          (should (equal default "Default (Opus 5.5)"))
          (should (eq (completion-metadata-get metadata 'category) 'drover-model))
          (should (eq (completion-metadata-get metadata 'display-sort-function) #'identity))
          (let ((annotate (completion-metadata-get metadata 'annotation-function)))
            (should (equal (funcall annotate "OpenAI: GPT-5.5") "  gpt-5.5  {high} default:provider"))
            (should-not (funcall annotate "Default (Opus 5.5)")))))
      (drover-model-test-reading '("Default")
        (should-not (drover-read-model))
        (should (equal (car (nth 1 (car reads))) "Default"))))))

(ert-deftest drover-model-test-models-changed ()
  "A change runs every function of the hook with the engine, one failing too."
  (let* ((calls nil)
         (drover-models-changed-hook
          (list (lambda (engine) (push (list 'first engine) calls))
                (lambda (_engine) (error "Broken"))
                (lambda (engine) (push (list 'last engine) calls)))))
    (drover-models-changed 'claude-code)
    (should (equal calls '((last claude-code) (first claude-code))))))

(ert-deftest drover-model-test-read-model-late-models ()
  "Models arriving while the picker reads are offered; the picker then stops listening."
  (drover-test-with-engines
    (let* ((late nil)
           (opus (drover-model-create :id "claude-opus-5-5" :name "Opus 5.5" :provider-name "Anthropic"))
           (drover-models-changed-hook nil)
           (seen nil))
      (drover-register-model-source 'claude-code (lambda (_) late))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt table &rest _)
                   (push (all-completions "" table) seen)
                   (setq late (list opus))
                   (drover-models-changed 'claude-code)
                   (push (all-completions "" table) seen)
                   (should (equal (funcall (completion-metadata-get (completion-metadata "" table nil)
                                                                    'annotation-function)
                                           "Anthropic: Opus 5.5")
                                  (drover-model-annotation opus)))
                   "Anthropic: Opus 5.5")))
        (should (eq (drover-read-model nil "/work/") opus)))
      (should (equal seen '(("Default" "Anthropic: Opus 5.5") ("Default"))))
      (should-not drover-models-changed-hook)
      ;; Quitting stops listening too.
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) (signal 'quit nil))))
        (should (eq (condition-case nil (drover-read-model nil "/work/") (quit 'quit)) 'quit)))
      (should-not drover-models-changed-hook))))

(ert-deftest drover-model-test-refresh-completion ()
  "The minibuffer's candidates are computed anew and shown again by each UI."
  (let ((minibuffer (generate-new-buffer " *drover-test-minibuffer*"))
        (calls nil))
    (unwind-protect
        (let ((drover-completion-refresh-functions
               (list (lambda () (push (list 'first (current-buffer) completion-all-sorted-completions) calls))
                     (lambda () (error "Broken"))
                     (lambda () (push (list 'last (current-buffer)) calls)))))
          (with-current-buffer minibuffer
            (setq completion-all-sorted-completions '("stale")))
          (drover--refresh-completion minibuffer)
          (should (equal calls (list (list 'last minibuffer) (list 'first minibuffer nil))))
          (setq calls nil)
          (kill-buffer minibuffer)
          (drover--refresh-completion minibuffer)
          (drover--refresh-completion nil)
          (should-not calls))
      (when (buffer-live-p minibuffer)
        (kill-buffer minibuffer)))))

(defvar vertico--input)
(defvar icomplete-mode)

(ert-deftest drover-model-test-refresh-completion-uis ()
  "Vertico computes candidates again for any input, Icomplete shows them, each when on."
  (let ((exhibited nil))
    (cl-letf (((symbol-function 'vertico--exhibit) (lambda () (push (list 'vertico vertico--input) exhibited)))
              ((symbol-function 'icomplete-exhibit) (lambda () (push 'icomplete exhibited))))
      (with-temp-buffer
        (drover--refresh-vertico)
        (drover--refresh-icomplete)
        (should-not exhibited)
        (setq-local vertico--input '("opus" . 4))
        (drover--refresh-vertico)
        (should (equal exhibited '((vertico t))))
        (let ((icomplete-mode t))
          (drover--refresh-icomplete))
        (should (eq (car exhibited) 'icomplete))))
    (should (equal drover-completion-refresh-functions
                   '(drover--refresh-completions-list drover--refresh-icomplete drover--refresh-vertico)))))

(ert-deftest drover-model-test-read-variant ()
  (let ((model (drover-model-create :id "m" :variants '((low :effort "low") (high :effort "high"))
                                    :default-variant 'high)))
    (drover-model-test-reading '("low" "Default (high)")
      (should (eq (drover-read-variant model) 'low))
      (should-not (drover-read-variant model))
      (should (equal (nth 1 (car reads)) '("Default (high)" "low" "high")))
      (should (eq (completion-metadata-get (nth 2 (car reads)) 'display-sort-function) #'identity)))
    (drover-model-test-reading nil
      ;; No variant: nothing to ask.
      (should-not (drover-read-variant (drover-model-create :id "m")))
      (should-not reads))))

(ert-deftest drover-model-test-read-agent ()
  (drover-test-with-engines
    (drover-test-register-engine 'opencode)
    (setq drover-test-engine-agents '((opencode "build" "plan")))
    (drover-model-test-reading '("plan" "Default (build)")
      (should (equal (drover-read-agent "build" "/work/") "plan"))
      (should-not (drover-read-agent "build" "/work/"))
      (should (equal (nth 1 (car reads)) '("Default (build)" "build" "plan"))))
    (setq drover-test-engine-agents nil)
    (drover-model-test-reading '("Default (build)")
      ;; Without agents, the default alone.
      (should-not (drover-read-agent "build" "/work/"))
      (should-error (drover-read-agent nil "/work/") :type 'user-error))))

;;;; Local state

(ert-deftest drover-model-test-json ()
  (let* ((directory (make-temp-file "drover-json" t))
         (file (expand-file-name "settings.json" directory))
         (drover--json-cache (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (should-not (drover-read-json file))
          (drover-test-write file "{\"a\": {\"b\": [1, 2]}, \"c\": null, \"d\": false}")
          (should (equal (drover-read-json file) '((a (b 1 2)) (c) (d))))
          (should (drover-json-object-p (drover-read-json file)))
          (should-not (drover-json-object-p '(1 2)))
          (should-not (drover-json-object-p "a"))
          ;; Cached while the file is unchanged.
          (let ((first (drover-read-json-cached file)))
            (should (eq (drover-read-json-cached file) first))
            (drover-test-write file "{\"e\": 1}")
            (set-file-times file (time-add (current-time) 10))
            (should (equal (drover-read-json-cached file) '((e . 1)))))
          (drover-test-write file "not json")
          (should-not (drover-read-json file)))
      (delete-directory directory t))))

(ert-deftest drover-model-test-file-mtime-and-getenv ()
  (let ((file (make-temp-file "drover-mtime")))
    (unwind-protect
        (progn
          (set-file-times file (seconds-to-time 1000000))
          (should (= (drover-file-mtime file) 1000000.0))
          (should-not (drover-file-mtime (concat file "-missing"))))
      (delete-file file)))
  (let ((process-environment (append '("DROVER_SET=yes" "DROVER_EMPTY=") process-environment)))
    (should (equal (drover-getenv "DROVER_SET") "yes"))
    (should-not (drover-getenv "DROVER_EMPTY"))
    (should-not (drover-getenv "DROVER_UNSET_AT_ALL"))))

(ert-deftest drover-model-test-merge-settings ()
  (let ((base '((model . "a") (env (A . "1") (B . "2")) (list 1 2)))
        (over '((env (B . "3") (C . "4")) (list 3) (effort . "high"))))
    ;; Objects merge key by key; new keys come first, as `setf' adds them.
    (should (equal (drover-merge-settings base over)
                   '((effort . "high") (model . "a") (env (C . "4") (A . "1") (B . "3")) (list 3))))
    ;; Neither argument changes.
    (should (equal base '((model . "a") (env (A . "1") (B . "2")) (list 1 2))))))

(ert-deftest drover-model-test-with-homes ()
  "Engine homes point into a fresh directory, deleted afterwards."
  (let (kept)
    (drover-test-with-homes
      (setq kept home)
      (should (string-prefix-p home drover-claude-code-home))
      (should (string-prefix-p home drover-codex-home))
      (should (string-prefix-p home (symbol-value 'drover-opencode-database)))
      (should (equal (funcall drover-claude-code-start-flags-function) ""))
      (should-not (getenv "ANTHROPIC_MODEL"))
      (should (file-directory-p home)))
    (should-not (file-exists-p kept))
    (should-not (string-prefix-p kept drover-claude-code-home))))

(provide 'drover-model-test)
;;; drover-model-test.el ends here
