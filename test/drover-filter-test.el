;;; drover-filter-test.el --- Filter checklist tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'drover-test-helpers)
(require 'drover-filter)
(require 'drover-keys)

(defmacro drover-filter-test-with (&rest body)
  "Run BODY in a checklist with three model choices."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (drover-filter-mode)
     (setq drover-filter--choices '(("Alpha" . "a") ("Beta" . "b") ("Gamma" . "c"))
           drover-filter--title "Models")
     (drover-filter--draw)
     ,@body))

(ert-deftest drover-filter-test-exclude-from-all ()
  "Unchecking one choice excludes only it, even when new values arrive."
  (drover-filter-test-with
    (should (equal (buffer-string) "[x] Alpha\n[x] Beta\n[x] Gamma\n"))
    (forward-line 1)
    (drover-filter--toggle)
    (should (equal drover-filter--selection '(:exclude "b")))
    (should (equal (buffer-string) "[x] Alpha\n[ ] Beta\n[x] Gamma\n"))
    (should (drover-filter-selection-match-p "new model" drover-filter--selection))
    (should (drover-filter-selection-match-p nil drover-filter--selection))
    (should (= (line-number-at-pos) 2))
    (drover-filter--toggle)
    (should-not drover-filter--selection)))

(ert-deftest drover-filter-test-none-and-invert ()
  "None matches nothing; inversion preserves inclusion and exclusion intent."
  (drover-filter-test-with
    (drover-filter--none)
    (should (equal drover-filter--selection '(:include)))
    (should-not (drover-filter-selection-match-p "new" drover-filter--selection))
    (drover-filter--toggle)
    (should (equal drover-filter--selection '(:include "a")))
    (drover-filter--invert)
    (should (equal drover-filter--selection '(:exclude "a")))
    (drover-filter--invert)
    (should (equal drover-filter--selection '(:include "a")))
    (drover-filter--all)
    (should-not drover-filter--selection)
    (drover-filter--invert)
    (should (equal drover-filter--selection '(:include)))
    (drover-filter--invert)
    (should-not drover-filter--selection)))

(ert-deftest drover-filter-test-search-retains-selection ()
  "Searching hides choices without losing their selection; bulk actions are global."
  (drover-filter-test-with
    (let ((last-command-event ?b)) (drover-filter--search))
    (should (equal (buffer-string) "[x] Beta\n"))
    (drover-filter--toggle)
    (should (equal drover-filter--selection '(:exclude "b")))
    (drover-filter--invert)
    (should (equal drover-filter--selection '(:include "b")))
    (drover-filter--clear-search)
    (should (equal (buffer-string) "[ ] Alpha\n[x] Beta\n[ ] Gamma\n"))
    (let ((last-command-event ?z)) (drover-filter--search))
    (should (equal (buffer-string) "No matching choices\n"))
    (drover-filter--toggle)
    (should (equal drover-filter--selection '(:include "b")))
    (drover-filter--backspace)
    (should (string-prefix-p "[ ] Alpha" (buffer-string)))
    (dolist (char '(?g ?q ?h ?n ?p))
      (should (eq (key-binding (string char)) #'drover-filter--search)))))

(ert-deftest drover-filter-test-read-apply-cancel ()
  "Apply returns the draft; cancel keeps the original and destroys the editor."
  (dolist (accept '(nil t))
    (let ((current '(:exclude "a"))
          (original-window (selected-window))
          (original-buffer (window-buffer))
          editor)
      (cl-letf (((symbol-function 'recursive-edit)
                 (lambda ()
                   (setq editor (current-buffer))
                   (should (equal drover-filter--selection current))
                   (should (equal (buffer-string) "[ ] Alpha\n[x] Beta\n"))
                   (drover-filter--toggle)
                   (cl-letf (((symbol-function 'exit-recursive-edit) #'ignore))
                     (if accept (drover-filter--apply) (drover-filter--cancel))))))
        (should (equal (drover-filter-read "Models" '(("Alpha" . "a") ("Beta" . "b")) current)
                       (unless accept current))))
      (should (equal current '(:exclude "a")))
      (should-not (buffer-live-p editor))
      (should (eq (selected-window) original-window))
      (should (eq (window-buffer) original-buffer)))))

(ert-deftest drover-filter-test-legacy-and-unknown ()
  "Legacy values reopen checked, including values no session reports any more."
  (dolist (current '("a" ("a") (:include "a") (:exclude "b")))
    (drover-filter-test-with
      (setq drover-filter--selection current)
      (drover-filter--draw)
      (should (string-prefix-p "[x] Alpha" (buffer-string)))
      (drover-filter--toggle)
      (should-not (drover-filter-selection-match-p "a" drover-filter--selection))))
  (cl-letf (((symbol-function 'drover-filter-read)
             (lambda (_prompt choices current)
               (should (equal choices '(("Alpha" . "a") ("gone" . "gone") ("(unknown)"))))
               current)))
    (should (equal (drover--read-filter "Models" '(("Alpha" . "a")) '(:exclude "gone" nil))
                   '(:exclude "gone" nil)))))

(ert-deftest drover-filter-test-matching-fields ()
  "Every categorical field supports several values, exclusions and none."
  (let ((session (drover-test-session "a" :project "p" :status 'done :agent "build"
                                      :model "a" :provider-name "OpenAI")))
    (dolist (entry '((:project . "p") (:statuses . done) (:agent . "build")
                     (:model . "a") (:provider . "OpenAI") (:harness . fake) (:host . local)))
      (pcase-let ((`(,key . ,value) entry))
        (should (drover-filter-match-p session (list key (list :include "other" value))))
        (should-not (drover-filter-match-p session (list key (list :exclude value))))
        (should (drover-filter-match-p session (list key '(:exclude "other"))))
        (should-not (drover-filter-match-p session (list key '(:include))))))
    (should (drover-filter-active-p '(:model (:include))))
    (should-not (drover-filter-match-p session '(:model (:include "a") :agent (:exclude "build"))))
    (should (drover-filter-match-p (drover-test-session "missing") '(:model (:exclude "a"))))
    (should-not (drover-filter-match-p (drover-test-session "missing") '(:model (:exclude nil))))))

(ert-deftest drover-filter-test-labels ()
  "Menu values and search descriptions explain inclusion, exclusion and none."
  (should (equal (drover--filter-label :model '(:exclude "a" "b")) "except a or b"))
  (should (equal (drover--filter-label :statuses '(:include done waiting)) "done or needs you"))
  (should (equal (drover--filter-label :harness '(:exclude opencode)) "except OpenCode"))
  (should (equal (drover--filter-label :provider '(:include)) "none"))
  (should (equal (drover-filter-description '(:model (:exclude "a") :agent (:include "build" "plan")))
                 "agent build or plan, model except a"))
  (should (equal (drover-filter-description '(:model (:include))) "model none")))

(ert-deftest drover-filter-test-through-transient ()
  "The real command loop edits a checklist and returns to the same menu."
  (drover-test-with-backend (list (drover-test-session "a" :model "Alpha")
                                  (drover-test-session "b" :model "Beta"))
                            (save-window-excursion
                              (with-temp-buffer
                                (setq-local drover-view-p t)
                                (use-local-map (let ((map (make-sparse-keymap)))
                                                 (define-key map (kbd "V") #'drover-view)
                                                 map))
                                (set-window-buffer (selected-window) (current-buffer))
                                (unwind-protect
                                    (progn
                                      (execute-kbd-macro (kbd "V = m b SPC RET"))
                                      (should (equal drover-filter '(:model (:exclude "Beta"))))
                                      (should (eq (oref transient--prefix command) 'drover-view))
                                      (execute-kbd-macro (kbd "= m M-n C-g"))
                                      (should (equal drover-filter '(:model (:exclude "Beta"))))
                                      (should (eq (oref transient--prefix command) 'drover-view))
                                      (execute-kbd-macro (kbd "= m M-a RET"))
                                      (should-not drover-filter)
                                      (should (eq (oref transient--prefix command) 'drover-view)))
                                  (when transient--prefix (execute-kbd-macro (kbd "C-g"))))))))

(provide 'drover-filter-test)
;;; drover-filter-test.el ends here
