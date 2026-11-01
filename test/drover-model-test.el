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
