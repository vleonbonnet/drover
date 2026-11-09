;;; drover-selection-test.el --- Destination selection tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Catalogs and sessions here are fixtures; no service is contacted.

;;; Code:

(require 'drover-test-helpers)
(require 'drover-selection)
(require 'drover-prompt)
(require 'drover-schedule)

(defmacro drover-selection-test-with (&rest body)
  "Run BODY with Astra offered by two engines and two providers."
  (declare (indent 0) (debug t))
  `(drover-test-with-backend nil
                             (drover-test-with-engines
                              (let* ((oc (drover-model-create :provider-id "openai" :provider-name "OpenAI"
                                                              :id "gpt-6-astra" :name "GPT-6 Astra"
                                                              :variants '((low) (high) (max)) :default-variant 'high
                                                              :engines '(opencode codex)))
                                     (copilot (drover-model-create :provider-id "github-copilot" :provider-name "Copilot"
                                                                   :id "gpt-6-astra" :name "GPT-6 Astra"
                                                                   :variants '((low) (high)) :engines '(opencode codex)))
                                     (codex (drover-model-create :provider-name "OpenAI" :id "gpt-6-astra" :name "GPT-6-Astra"
                                                                 :variants '((low) (high) (ultra)) :default-variant 'high
                                                                 :remembered 1000000.0 :engines '(codex)))
                                     (catalogs (list (cons 'codex (list codex)) (cons 'opencode (list oc copilot))))
                                     (choice (car (drover-selection-models catalogs))))
                                (drover-test-register-engine 'opencode :model-resets-variant t)
                                (drover-test-register-engine 'codex)
                                (drover-register-model-source 'opencode (lambda (_) (list oc copilot)))
                                (drover-register-model-source 'codex (lambda (_) (list codex)))
                                ,@body))))

(ert-deftest drover-selection-test-one-model-native-offerings ()
  "A live OpenCode catalog and remembered Codex catalog offer one logical Astra."
  (drover-selection-test-with
   (should (= (length (drover-selection-models (drover-model-catalogs))) 1))
   (should (equal (drover-model-label choice) "GPT-6 Astra"))
   (should-not (drover-model-provider-id choice))
   (should-not (drover-model-remembered choice))
   (should (assq 'ultra (drover-model-variants choice)))
   (dolist (engine '(opencode codex))
     (let* ((resolved (drover-selection-resolve engine (list :model choice :selection t :variant 'high)
                                                nil (alist-get engine catalogs) t))
            (native (plist-get (plist-get resolved :options) :model)))
       (should (eq native (if (eq engine 'opencode) oc codex)))
       (should (equal (drover-model-id-for native engine)
                      (if (eq engine 'opencode) "openai/gpt-6-astra" "gpt-6-astra")))))
   (should (equal (drover-model-variants oc) '((low) (high) (max))))
   (should (drover-model-remembered codex))))

(ert-deftest drover-selection-test-provider-and-effort-constraints ()
  "Provider and effort constrain actual offerings, not guessed model families."
  (drover-selection-test-with
   (let ((options (list :model choice :selection t :provider "github-copilot")))
     (should (eq (plist-get (plist-get (drover-selection-resolve 'opencode options nil (list oc copilot) t)
                                       :options) :model) copilot))
     (should (plist-get (drover-selection-resolve 'codex options nil (list codex) t) :failure)))
   (let ((options (list :model choice :selection t :variant 'ultra)))
     (should (plist-get (drover-selection-resolve 'opencode options nil (list oc) t) :failure))
     (should-not (plist-get (drover-selection-resolve 'codex options nil (list codex) t) :failure)))
   (should (equal (drover-selection-providers catalogs "gpt-6-astra" 'max) '(("OpenAI" . "openai"))))))

(ert-deftest drover-selection-test-native-effort-translation ()
  "An effort is resolved to the destination's variant identifier."
  (let* ((model (drover-model-create :provider-id "gateway" :id "gpt-6-astra"
                                     :variants '((deep :effort "high")) :engines '(opencode)))
         (choice (car (drover-selection-models (list (list 'opencode model)))))
         (result (drover-selection-resolve 'opencode (list :model choice :selection t :variant 'high)
                                           nil (list model) t)))
    (should (equal (drover-model-variants choice) '((high))))
    (should (eq (plist-get (plist-get result :options) :variant) 'deep))))

(ert-deftest drover-selection-test-identity-is-not-display-name ()
  "Editions and unrelated opaque models do not merge by a friendly name."
  (let* ((models (mapcar (lambda (pair) (drover-model-create :provider-id (car pair)
                                                             :id (cdr pair) :name "Astra"))
                         '(("openai" . "gpt-6-astra") ("openai" . "gpt-6-astra-fast")
                           ("one" . "deployment") ("two" . "deployment"))))
         (catalogs (list (cons 'opencode models))))
    (should (= (length (drover-selection-models catalogs)) 4))
    (let ((drover-selection-model-aliases '((("one" . "deployment") . "gpt-6-astra"))))
      (should (= (length (drover-selection-models catalogs)) 3)))))

(ert-deftest drover-selection-test-existing-session-switch-capability ()
  "An existing session can change models only when its backend supports that change."
  (drover-selection-test-with
   (let* ((old (drover-model-create :provider-id "openai" :id "gpt-6-sol" :engines '(opencode)))
          (current (list :model old :variant 'low))
          (options (list :model choice :selection t :variant 'high)))
     (should-not (plist-get (drover-selection-resolve 'opencode options current (list oc) '(:model :variant))
                            :failure))
     (should (plist-get (drover-selection-resolve 'opencode options current (list oc) '(:variant)) :failure))
     (should (plist-get (drover-selection-resolve 'opencode options current (list oc) '(:model)) :failure))
     (should (equal (plist-get (drover-selection-resolve 'opencode nil current (list oc) nil) :effective)
                    (list :agent nil :model old :variant 'low))))))

(ert-deftest drover-selection-test-independent-provider-keeps-model ()
  "A provider-only choice changes the serving provider of the current model."
  (drover-selection-test-with
   (let* ((current (list :model oc :variant 'low))
          (result (drover-selection-resolve 'opencode '(:provider "github-copilot" :selection t)
                                            current (list copilot oc) '(:model :variant))))
     (should (eq (plist-get (plist-get result :options) :model) copilot)))
   (should (eq (plist-get (plist-get (drover-selection-resolve 'opencode (list :model choice :selection t)
                                                               (list :model oc) (list copilot oc) t)
                                     :options) :model) oc))))

(ert-deftest drover-selection-test-picker-search-and-constraints ()
  "Text searches new harness names and existing session names, not their harness annotations."
  (drover-selection-test-with
   (let* ((drover-prompt-buffer-name " *drover selection test*")
          (buffer (get-buffer-create drover-prompt-buffer-name))
          (session-buffer (generate-new-buffer "Repair authentication"))
          (session (drover-test-session "s" :engine 'opencode))
          (targets '((:backend fake :engine opencode :label "OpenCode session")
                     (:backend fake :engine codex :label "Codex session"))))
     (unwind-protect
         (progn
           (with-current-buffer buffer
             (setq-local drover-prompt-picked-model choice
                         drover-prompt-model-selection t))
           (let* ((candidates (drover-prompt--candidates (list (cons session session-buffer)) targets))
                  (predicate (drover-prompt--target-predicate candidates))
                  (names (mapcar #'car candidates))
                  (completion-ignore-case t))
             (should (equal (all-completions "Repair" names predicate) '("Repair authentication")))
             (should-not (member "Repair authentication" (all-completions "OpenCode" names predicate)))
             (should (funcall predicate "[New OpenCode session…]"))
             (should (funcall predicate "[New Codex session…]"))
             (with-current-buffer buffer (setq drover-prompt-picked-variant 'ultra))
             (should-not (funcall predicate "[New OpenCode session…]"))
             (should-not (funcall predicate "Repair authentication"))
             (should (funcall predicate "[New Codex session…]"))))
       (kill-buffer buffer)
       (kill-buffer session-buffer)))))

(ert-deftest drover-selection-test-apply-failure-retains-prompt ()
  "A refused option does not send or consume the prompt, even with a late success callback."
  (drover-selection-test-with
   (let* ((drover-prompt-buffer-name " *drover selection test*")
          (buffer (get-buffer-create drover-prompt-buffer-name))
          (session (drover-test-session "s" :engine 'opencode))
          (late nil))
     (unwind-protect
         (progn
           (apply #'drover-register-backend 'fake
                  :apply-options (lambda (_session _options then else)
                                   (setq late then) (funcall else "Model refused"))
                  (alist-get 'fake drover--backends))
           (with-current-buffer buffer
             (insert "Please fix it")
             (setq-local drover-prompt-picked-model choice drover-prompt-model-selection t)
             (drover-prompt--send-to-session session (drover-prompt-content buffer) buffer))
           (funcall late)
           (should (equal (with-current-buffer buffer (buffer-string)) "Please fix it"))
           (should-not (seq-find (lambda (call) (eq (car call) :send)) drover-test-calls)))
       (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest drover-selection-test-scheduled-choice-round-trip ()
  "A scheduled prompt keeps independent choices, not a cached resolved route."
  (drover-selection-test-with
   (let* ((options (list :model choice :selection t :provider "openai" :variant 'ultra))
          (job (list :options options))
          (read-back (plist-get (drover-schedule--job-in (drover-schedule--job-out job)) :options)))
     (should (drover-schedule--options-valid-p read-back))
     (should (equal read-back options)))))

(ert-deftest drover-selection-test-current-metadata-cannot-expand-catalog ()
  "A stale current record cannot make a catalog's unsupported effort eligible."
  (drover-selection-test-with
   (let ((old (copy-sequence oc)))
     (setf (drover-model-variants old) '((ultra)))
     (should (plist-get (drover-selection-resolve
                         'opencode (list :model choice :selection t :variant 'ultra)
                         (list :model old) (list oc) '(:model :variant)) :failure)))))

(ert-deftest drover-selection-test-effort-only-resolves-provider-switch ()
  "An effort-only choice includes the model when it needs another provider."
  (drover-selection-test-with
   (let* ((current (list :model copilot :variant 'high))
          (options '(:selection t :variant max))
          (result (drover-selection-resolve 'opencode options current (list copilot oc) '(:model :variant))))
     (should (eq (plist-get (plist-get result :options) :model) oc))
     (should (eq (plist-get (plist-get result :options) :variant) 'max))
     (should (plist-get (drover-selection-resolve 'opencode options current (list copilot oc) '(:variant))
                        :failure)))))

(ert-deftest drover-selection-test-pending-send-cannot-repeat ()
  "A pending switch prevents duplicate sends and keeps a subsequently edited draft."
  (drover-selection-test-with
   (let* ((drover-prompt-buffer-name " *drover pending selection test*")
          (buffer (get-buffer-create drover-prompt-buffer-name))
          (session (drover-test-session "s" :engine 'opencode))
          (pending nil))
     (unwind-protect
         (progn
           (apply #'drover-register-backend 'fake
                  :apply-options (lambda (_session _options then &optional _else) (setq pending then))
                  (alist-get 'fake drover--backends))
           (with-current-buffer buffer
             (insert "Please fix it")
             (setq-local drover-prompt-picked-model choice drover-prompt-model-selection t)
             (let ((content (drover-prompt-content buffer)))
               (drover-prompt--send-to-session session content buffer)
               (should-error (drover-prompt--send-to-session session content buffer) :type 'user-error))
             (insert " and keep this draft"))
           (cl-letf (((symbol-function 'drover-prompt--sent) #'ignore))
             (funcall pending)
             (funcall pending))
           (should (= 1 (seq-count (lambda (call) (eq (car call) :send)) drover-test-calls)))
           (should-not (buffer-local-value 'drover-prompt--sending buffer))
           (should (equal (with-current-buffer buffer (buffer-string)) "Please fix it and keep this draft")))
       (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest drover-selection-test-chosen-remote-catalog ()
  "An existing remote target supplies its picker and resolution from its owner's catalog."
  (drover-selection-test-with
   (let* ((session (drover-test-session "s" :engine 'opencode :host "owner" :directory "/there/"))
          (remote (copy-sequence copilot))
          (drover-prompt-remote-functions
           (list :catalog (lambda (host _local directory &optional _wait)
                            (should (equal host "owner"))
                            (should (equal directory "/there/"))
                            (list :engines (list (list 'opencode :models (list remote))))))))
     (setf (drover-model-variants remote) '((ultra)))
     (with-temp-buffer
       (setq-local drover-prompt--chosen-session (list :session session))
       (should (equal (drover-prompt--selection-catalogs (current-buffer)) (list (list 'opencode remote))))
       (let ((resolved (drover-prompt--resolve session nil (list :model choice :selection t :variant 'ultra)
                                               (current-buffer))))
         (should (eq (plist-get (plist-get resolved :options) :model) remote)))))))

(ert-deftest drover-selection-test-steps-stop-on-refusal-and-timeout ()
  "Strict switching stops once, including when a timed-out step answers late."
  (dolist (timeout '(nil t))
    (let (next failures advanced sent fire)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_time _repeat callback &rest args)
                   (setq fire (lambda () (apply callback args)))
                   (timer-create))))
        (drover-run-steps
         (list (cons "model" (lambda (callback) (setq next callback)))
               (cons "effort" (lambda (callback) (setq advanced t) (funcall callback))))
         (lambda () (setq sent t)) (lambda (reason) (push reason failures)))
        (if timeout (funcall fire) (funcall next "Refused")))
      (funcall next)
      (should (= 1 (length failures)))
      (should-not advanced)
      (should-not sent))))

(ert-deftest drover-selection-test-native-claude-provider ()
  "Native identifiers do not confuse the model vendor with a configured serving provider."
  (require 'drover-claude-code)
  (let ((process-environment nil))
    (should (equal (drover-selection-provider 'claude-code (drover-claude-code-model "claude-opus-5-5" nil nil))
                   "anthropic"))
    (setenv "CLAUDE_CODE_USE_BEDROCK" "1")
    (let ((model (drover-claude-code-model "claude-opus-5-5" nil nil)))
      (should (equal (drover-selection-provider 'claude-code model) "amazon-bedrock"))
      (should (equal (drover-model-id-for model 'claude-code) "claude-opus-5-5")))
    (setenv "CLAUDE_CODE_USE_BEDROCK" nil)
    (let ((model (drover-claude-code-model
                  "claude-opus-5-5" '((project (env (ANTHROPIC_BASE_URL . "https://example.invalid")))) nil)))
      (should-not (drover-selection-provider 'claude-code model)))))

(provide 'drover-selection-test)
;;; drover-selection-test.el ends here
