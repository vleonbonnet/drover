;;; drover-prompt-test.el --- Tests of the prompt  -*- lexical-binding: t; -*-

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

;; The prompt is tested against the fake backend and fake engines of
;; `drover-test-helpers' only.  `drover-prompt-test-with' gives the fake
;; sessions the buffers of `drover-prompt-test-buffers', names the
;; prompt buffer for the test, and stubs its display and the refresh
;; after the command, which would otherwise run in a later test; tests
;; of those display and refresh for real.  Pickers are stubbed through
;; `completing-read'.

;;; Code:

(require 'drover-test-helpers)
(require 'drover-prompt)
(require 'org-id)
(require 'project)

(defvar drover-prompt-test-buffers nil
  "Buffers of the fake sessions, as an alist (ID . BUFFER).")

(defun drover-prompt-test--give-buffers ()
  "Make the fake backend tell the buffers of `drover-prompt-test-buffers'."
  (apply #'drover-register-backend 'fake
         :buffer (lambda (session)
                   (alist-get (drover-session-id session) drover-prompt-test-buffers nil nil #'equal))
         (alist-get 'fake drover--backends)))

(defmacro drover-prompt-test-with (&rest body)
  "Run BODY with the fake backend serving no session and no engine registered.
The prompt buffer is named for the test and killed afterwards, as are
the buffers of `drover-prompt-test-buffers'.  Nothing is displayed, and
the volatile defaults are refreshed only when BODY asks.  No feature is
required, no new session offered, and the default engine is `fake'."
  (declare (indent 0) (debug t))
  `(drover-test-with-backend nil
     (drover-test-with-engines
       (let ((drover-prompt-buffer-name "*drover prompt test*")
             (drover-prompt-require-features nil)
             (drover-prompt-new-targets nil)
             (drover-prompt-default-engine 'fake)
             (drover-prompt-test-buffers nil)
             (org-mode-hook nil)
             (kill-ring nil))
         (unwind-protect
             (cl-letf (((symbol-function 'drover-prompt--display) #'ignore)
                       ((symbol-function 'drover-prompt--schedule-refresh) #'ignore))
               (drover-prompt-test--give-buffers)
               ,@body)
           (when-let* ((buffer (get-buffer drover-prompt-buffer-name)))
             (kill-buffer buffer))
           (dolist (entry drover-prompt-test-buffers)
             (when (buffer-live-p (cdr entry))
               (kill-buffer (cdr entry)))))))))

(defmacro drover-prompt-test-with-messages (&rest body)
  "Run BODY collecting the echo area messages in `messages', newest first."
  (declare (indent 0) (debug t))
  `(let ((messages nil))
     (cl-letf (((symbol-function 'message)
                (lambda (format-string &rest arguments)
                  (push (apply #'format-message format-string arguments) messages))))
       ,@body)))

(defmacro drover-prompt-test-with-warnings (&rest body)
  "Run BODY collecting the messages of `drover-warn' in `warnings', newest first."
  (declare (indent 0) (debug t))
  `(let ((warnings nil))
     (cl-letf (((symbol-function 'display-warning)
                (lambda (_type message &optional _level _buffer)
                  (push message warnings))))
       ,@body)))

(defun drover-prompt-test--session (id &rest properties)
  "Serve a fake session ID with PROPERTIES, of engine `fake', with a buffer.
Return the session."
  (let ((session (apply #'drover-test-session id (append properties (list :engine 'fake))))
        (buffer (generate-new-buffer (concat "session " id))))
    (setq drover-test-sessions (append drover-test-sessions (list session)))
    (push (cons id buffer) drover-prompt-test-buffers)
    session))

(defun drover-prompt-test--buffer-of (id)
  "Return the buffer of the fake session ID."
  (alist-get id drover-prompt-test-buffers nil nil #'equal))

(defmacro drover-prompt-test-choosing (answer &rest body)
  "Run BODY with `completing-read' answering ANSWER, a function of the collection.
ANSWER is called with the names offered, those the predicate keeps,
in the order offered; it returns the name chosen.  `offered' holds the
names of the latest call, and `metadata' its completion metadata."
  (declare (indent 1) (debug t))
  `(let ((offered nil)
         (metadata nil))
     (cl-letf (((symbol-function 'completing-read)
                (lambda (_prompt collection &optional predicate &rest _)
                  (setq offered (all-completions "" collection predicate)
                        metadata (completion-metadata "" collection predicate))
                  (funcall ,answer offered))))
       ;; Tests read them only when they need to.
       (ignore offered metadata)
       ,@body)))

;;;; Context and Markdown

(defun drover-prompt-test--context (text &optional language)
  "Return `drover-prompt-format-context' quoting all of TEXT in LANGUAGE."
  (with-temp-buffer
    (insert text)
    (drover-prompt-format-context (current-buffer) (buffer-string) (or language "org"))))

(ert-deftest drover-prompt-test-context-is-escaped-src-block ()
  "A quoted region is an Org src block whose own Org syntax is escaped."
  (should (string-match-p
           "line [0-9]+:\n#\\+begin_src org\n,\\* heading\n,#\\+end_src\nplain\n#\\+end_src\n\n\\'"
           (drover-prompt-test--context "* heading\n#+end_src\nplain\n"))))

(ert-deftest drover-prompt-test-context-round-trips-to-markdown ()
  "An escaped region reaches the agent verbatim inside a Markdown fence."
  (should (string-suffix-p
           "```org\n* heading\n#+end_src\n,* kept\nplain\n```\n\n"
           (drover-prompt-to-markdown
            (drover-prompt-test--context "* heading\n#+end_src\n,* kept\nplain")))))

(ert-deftest drover-prompt-test-context-of-file-and-terminal ()
  "A file is linked at its line; a terminal's padding and blank lines around a region go."
  (let ((file (make-temp-file "drover-context" nil ".txt" "one\ntwo\n")))
    (unwind-protect
        (with-current-buffer (find-file-noselect file)
          (forward-line 1)
          (should (equal (drover-prompt-format-context (current-buffer) nil "text")
                         (format "From [[file:%s::2][%s:2]]\n\n" file (buffer-name))))
          (kill-buffer))
      (delete-file file)))
  (with-temp-buffer
    (setq major-mode 'comint-mode)
    (should (string-suffix-p ":\n#+begin_src console\n$ ls\na\n#+end_src\n\n"
                             (drover-prompt-format-context (current-buffer) "\n  \n$ ls   \na \n  "
                                                           "console")))))

(ert-deftest drover-prompt-test-fence-outgrows-backticks ()
  "Backtick fences inside a block cannot close its Markdown fence."
  (should (equal (drover-prompt-to-markdown
                  "#+begin_src markdown\n```elisp\n(foo)\n```\n#+end_src")
                 "````markdown\n```elisp\n(foo)\n```\n````")))

(ert-deftest drover-prompt-test-prose-kept-verbatim ()
  "Text outside blocks, affiliated keywords and trailing blank lines are untouched."
  (should (equal (drover-prompt-to-markdown
                  (concat "See [[id:ABC][Task]] and *bold* /it/.\n\n"
                          "#+name: snippet\n#+begin_src elisp\n(foo)\n#+end_src\n\n\n"
                          "- item\n\nAfter."))
                 (concat "See [[id:ABC][Task]] and *bold* /it/.\n\n"
                         "#+name: snippet\n```elisp\n(foo)\n```\n\n\n"
                         "- item\n\nAfter."))))

(ert-deftest drover-prompt-test-example-block-and-list-indentation ()
  "Example blocks have no language; a block in a list item keeps its column."
  (should (equal (drover-prompt-to-markdown "#+begin_example\nout\n#+end_example\n")
                 "```\nout\n```\n"))
  (should (equal (drover-prompt-to-markdown
                  "- item\n  #+begin_src sh\n    ls\n\n    pwd\n  #+end_src\n- next")
                 "- item\n  ```sh\n  ls\n\n  pwd\n  ```\n- next")))

(ert-deftest drover-prompt-test-markdown-is-plain-text ()
  "Startup modes such as `org-indent-mode' leave no properties on the result."
  (let* ((org-startup-indented t)
         (markdown (drover-prompt-to-markdown "* h\ntext\n#+begin_src sh\nls\n#+end_src")))
    (should (equal-including-properties markdown (substring-no-properties markdown)))))

(ert-deftest drover-prompt-test-block-indentation-follows-org ()
  "Common indentation is removed unless the block preserves it."
  (should (equal (drover-prompt-to-markdown "#+begin_src python\n  x = 1\n#+end_src")
                 "```python\nx = 1\n```"))
  (should (equal (drover-prompt-to-markdown "#+begin_src python -i\n  x = 1\n#+end_src")
                 "```python\n  x = 1\n```")))

(ert-deftest drover-prompt-test-language ()
  "Modes take their language from `drover-prompt-mode-languages', else their name."
  (should (equal (drover-prompt-language 'python-ts-mode) "python"))
  (should (equal (drover-prompt-language 'emacs-lisp-mode) "emacs-lisp"))
  (should (equal (drover-prompt-language 'ghostel-mode) "console"))
  (let ((drover-prompt-mode-languages '((emacs-lisp-mode . "elisp"))))
    (should (equal (drover-prompt-language 'emacs-lisp-mode) "elisp"))))

;;;; The buffer and its mode

(ert-deftest drover-prompt-test-default-buffer ()
  "The default prompt buffer is created undisplayed from the current directory, then reused."
  (drover-prompt-test-with
    (let* ((default-directory temporary-file-directory)
           (buffer (drover-prompt-default-buffer)))
      (should (equal (buffer-name buffer) drover-prompt-buffer-name))
      (should-not (get-buffer-window buffer t))
      (with-current-buffer buffer
        (should (derived-mode-p 'org-mode))
        (should drover-prompt-mode)
        (should (equal header-line-format '(:eval (drover-prompt--header-line))))
        (should (equal default-directory
                       (file-name-as-directory (expand-file-name temporary-file-directory))))
        (should-not drover-prompt-directory-picked-p))
      (with-current-buffer buffer
        (setq drover-prompt-picked-agent "plan"))
      (let ((default-directory "/"))
        (should (eq (drover-prompt-default-buffer) buffer)))
      ;; Reused as it is.
      (with-current-buffer buffer
        (should (equal drover-prompt-picked-agent "plan"))
        (should (equal default-directory
                       (file-name-as-directory (expand-file-name temporary-file-directory))))))))

(ert-deftest drover-prompt-test-defines-drover-actions ()
  "The mode defines the prompt's actions, `clear' and `schedule' among them, which a hook may redefine."
  (drover-prompt-test-with
    (with-current-buffer (drover-prompt-buffer t)
      (should (eq (alist-get 'schedule drover-prompt-actions) #'drover-schedule-prompt)))
    (kill-buffer (drover-prompt-buffer))
    (let ((drover-prompt-mode-hook
           (list (lambda () (when drover-prompt-mode (drover-prompt-define 'schedule #'ignore))))))
      (with-current-buffer (drover-prompt-buffer t)
        (should (equal (sort (mapcar (lambda (entry) (symbol-name (car entry))) drover-prompt-actions)
                             #'string<)
                       '("agent" "attach" "clear" "copy-block" "directory" "effort" "model"
                         "open-link" "schedule" "send")))
        (should (eq (alist-get 'send drover-prompt-actions) #'drover-prompt-submit))
        (should (eq (alist-get 'clear drover-prompt-actions) #'drover-prompt-erase))
        (should (eq (alist-get 'effort drover-prompt-actions) #'drover-prompt-choose-effort))
        (should (eq (alist-get 'open-link drover-prompt-actions) #'org-open-at-point))
        (should (memq #'drover-prompt--remember-point post-command-hook))
        ;; Turned off, it takes back its own actions only.
        (drover-prompt-define 'attach #'ignore)
        (drover-prompt-mode -1)
        (should (equal (sort (mapcar #'car drover-prompt-actions)
                             (lambda (a b) (string< a b)))
                       '(attach schedule)))
        (should-not (memq #'drover-prompt--remember-point post-command-hook))))))

(ert-deftest drover-prompt-test-keys-are-actions ()
  "The prompt binds no key; the picker's options menu replaces the menu; the header names keys."
  (let (bound)
    (map-keymap (lambda (&rest _) (setq bound t)) drover-prompt-mode-map)
    (should-not bound))
  (should (eq (command-remapping #'drover-dispatch nil (list drover-prompt-target-map))
              #'drover-prompt-options-menu))
  (pcase-dolist (`(,key . ,command) '(("a" . drover-prompt-choose-agent)
                                      ("m" . drover-prompt-choose-model)
                                      ("e" . drover-prompt-choose-effort)
                                      ("d" . drover-prompt-choose-directory)))
    (should (eq (plist-get (cdr (transient-get-suffix 'drover-prompt-options-menu key)) :command)
                command)))
  (drover-prompt-test-with
    (with-current-buffer (drover-prompt-buffer t)
      (use-local-map (let ((map (make-sparse-keymap)))
                       (keymap-set map "C-'" #'drover-dispatch)
                       map))
      (let ((header (substring-no-properties (drover-prompt--header-line))))
        (should (string-prefix-p "C-' RET send  C-' S-<return> schedule  C-' i attach  |  C-' d cwd" header))
        (should (string-match-p "  a agent  m model  e effort\\'" header)))
      ;; A buffer not scheduling names no key for it.
      (drover-prompt-define 'schedule nil)
      (should-not (string-match-p "schedule" (drover-prompt--header-line)))
      (drover-prompt-define 'schedule #'ignore)
      (setq drover-prompt-picked-agent "plan")
      (let ((header (drover-prompt--header-line)))
        (should (string-match-p "C-' RET send  C-' S-<return> schedule  C-' i attach" header))
        (should (string-match-p "a agent \\[plan\\]" header))
        (should (eq (get-text-property (string-match "plan" header) 'face header)
                    'opencode-setting-chosen))))))

(ert-deftest drover-prompt-test-option-display ()
  "Chosen settings show in one face, inherited ones in another."
  (should (equal-including-properties (drover-prompt-option-display 'high t)
                                      (propertize "high" 'face 'opencode-setting-chosen)))
  (should (equal-including-properties (drover-prompt-option-display "plan" nil)
                                      (propertize "plan" 'face 'opencode-setting-inherited))))

;;;; Defaults and what the prompt runs

(defvar drover-prompt-test--pending nil
  "Requests of defaults of the recording engines, newest first.
Each is (ENGINE DIRECTORY CALLBACK).")

(defun drover-prompt-test--recording-defaults (engine)
  "Return a :defaults function of ENGINE that records its requests.
They go to `drover-prompt-test--pending'; it never answers, tests call
the callbacks recorded."
  (lambda (directory callback)
    (push (list engine directory callback) drover-prompt-test--pending)))

(defun drover-prompt-test--requests (engine)
  "Return the requests of defaults of ENGINE, newest first.
Each is (DIRECTORY CALLBACK)."
  (mapcar #'cdr (seq-filter (lambda (request) (eq (car request) engine)) drover-prompt-test--pending)))

(defun drover-prompt-test--answer (engine index defaults)
  "Answer DEFAULTS to the request of ENGINE at INDEX, newest first."
  (funcall (nth 1 (nth index (drover-prompt-test--requests engine))) defaults))

(ert-deftest drover-prompt-test-defaults-delivery ()
  "Slow engines are asked when the prompt starts, for its directory; their answer lands when it comes."
  (drover-prompt-test-with
    (let ((drover-prompt-test--pending nil))
      (drover-test-register-engine 'slow :defaults (drover-prompt-test--recording-defaults 'slow))
      (drover-test-register-engine 'quick :volatile t)
      (setq drover-test-engine-defaults '((quick :model "quick")))
      (let* ((default-directory temporary-file-directory)
             (buffer (drover-prompt-default-buffer)))
        (with-current-buffer buffer
          (should (equal (mapcar #'car (drover-prompt-test--requests 'slow)) (list default-directory)))
          ;; Volatile engines are read after the command, not at start.
          (should-not (assq :defaults drover-test-engine-calls))
          (should-not drover-prompt--defaults)
          (drover-prompt-test--answer 'slow 0 '(:agent "build"))
          (should (equal drover-prompt--defaults '((slow :agent "build"))))
          (drover-prompt--refresh-volatile)
          (should (equal drover-test-engine-calls `((:defaults quick ,default-directory))))
          (should (equal (alist-get 'quick drover-prompt--defaults) '(:model "quick"))))))))

(ert-deftest drover-prompt-test-defaults-discard-stale ()
  "A later request of the same engine, or a directory change, discards defaults still arriving."
  (drover-prompt-test-with
    (let ((drover-prompt-test--pending nil))
      (drover-test-register-engine 'slow :defaults (drover-prompt-test--recording-defaults 'slow))
      (drover-test-register-engine 'other :defaults (drover-prompt-test--recording-defaults 'other))
      (with-current-buffer (drover-prompt-buffer t)
        (drover-prompt--fetch-defaults)
        (drover-prompt--fetch-defaults)
        (drover-prompt-test--answer 'slow 1 '(:agent "stale"))
        (should-not drover-prompt--defaults)
        ;; Each engine counts its own requests.
        (drover-prompt-test--answer 'other 1 '(:agent "other stale"))
        (should-not drover-prompt--defaults)
        (drover-prompt-test--answer 'other 0 '(:agent "other"))
        (should (equal drover-prompt--defaults '((other :agent "other"))))
        (setq default-directory (file-name-as-directory
                                 (expand-file-name "moved" temporary-file-directory)))
        (drover-prompt-test--answer 'slow 0 '(:agent "moved"))
        (should-not (assq 'slow drover-prompt--defaults))
        (drover-prompt--fetch-defaults)
        (drover-prompt-test--answer 'slow 0 '(:agent "fresh"))
        (should (equal (alist-get 'slow drover-prompt--defaults) '(:agent "fresh")))
        ;; An answer after the prompt is gone is dropped.
        (drover-prompt--fetch-defaults))
      (kill-buffer (drover-prompt-buffer))
      (drover-prompt-test--answer 'slow 0 '(:agent "late")))))

(ert-deftest drover-prompt-test-defaults-require-features ()
  "Starting a prompt loads `drover-prompt-require-features', missing ones skipped."
  (let* ((directory (make-temp-file "drover-features" t))
         (load-path (cons directory load-path)))
    (unwind-protect
        (drover-prompt-test-with
          (with-temp-file (expand-file-name "drover-prompt-test-feature.el" directory)
            (insert ";;; -*- lexical-binding: t -*-\n(drover-test-register-engine 'loaded)\n(provide 'drover-prompt-test-feature)\n"))
          (let ((drover-prompt-require-features '(drover-prompt-test-missing drover-prompt-test-feature)))
            (with-current-buffer (drover-prompt-default-buffer)
              (should (featurep 'drover-prompt-test-feature))
              ;; Required first, its engine was asked for its defaults.
              (should (equal drover-test-engine-calls `((:defaults loaded ,default-directory)))))))
      (setq features (delq 'drover-prompt-test-feature features))
      (delete-directory directory t))))

(defconst drover-prompt-test--schedule-refresh (symbol-function 'drover-prompt--schedule-refresh)
  "The definition of `drover-prompt--schedule-refresh', which tests stub.")

(ert-deftest drover-prompt-test-refresh-after-command ()
  "Volatile defaults and the last engine are read once the command returns, in a live prompt."
  (drover-prompt-test-with
    (drover-test-register-engine 'quick :volatile t)
    (drover-test-register-engine 'other)
    (setq drover-test-engine-defaults '((quick :model "quick") (other :model "other"))
          drover-test-engine-last-used '((quick . 10.0) (other . 20.0)))
    (let ((timers nil))
      (cl-letf (((symbol-function 'drover-prompt--schedule-refresh) drover-prompt-test--schedule-refresh)
                ((symbol-function 'run-at-time)
                 (lambda (time repeat function &rest arguments)
                   (should (equal (list time repeat) '(0 nil)))
                   (push (lambda () (apply function arguments)) timers))))
        (let ((prompt (drover-prompt-default-buffer)))
          (with-current-buffer prompt
            (should (equal drover-prompt--defaults '((other :model "other"))))
            (should-not drover-prompt--last-engine))
          (should (= (length timers) 1))
          (funcall (pop timers))
          (with-current-buffer prompt
            (should (eq drover-prompt--last-engine 'other))
            (should (equal (alist-get 'quick drover-prompt--defaults) '(:model "quick"))))
          ;; Context added later refreshes again, unless the prompt is gone.
          (with-temp-buffer
            (insert "x")
            (drover-prompt-add-context))
          (should (= (length timers) 1))
          (kill-buffer prompt)
          (setq drover-test-engine-calls nil)
          (funcall (pop timers))
          (should-not drover-test-engine-calls))))))

(ert-deftest drover-prompt-test-header-follows-last-engine ()
  "The header shows what a new session of the engine used last would run."
  (drover-prompt-test-with
    (let ((opus (drover-test-model "claude-opus-5-5" :provider-name "Anthropic" :name "Opus 5.5"
                                   :engines '(claude)))
          (gpt (drover-test-model "gpt-a" :provider-id "openai" :name "GPT-A"
                                  :default-variant 'medium :engines '(open))))
      (drover-test-register-engine
       'claude :volatile t
       :resolve (lambda (defaults agent model variant)
                  (let ((model (or model (plist-get defaults :model))))
                    (list :agent (or agent (plist-get defaults :agent)) :model model
                          :variant (or variant (and model (plist-get defaults :effort)))))))
      (drover-test-register-engine
       'open
       :resolve (lambda (defaults agent model variant)
                  (let ((model (or model (plist-get defaults :default-model))))
                    (list :agent (or agent (plist-get defaults :agent)) :model model
                          :variant (or variant (and model (drover-model-default-variant model)))))))
      (setq drover-test-engine-defaults `((claude :model ,opus :effort xhigh))
            drover-test-engine-last-used '((claude . 20.0) (open . 10.0)))
      (with-current-buffer (drover-prompt-buffer t)
        (setq drover-prompt--defaults `((open :agent "plan" :default-model ,gpt)))
        (drover-prompt--refresh-volatile)
        (should (eq drover-prompt--last-engine 'claude))
        (let ((options (drover-prompt-effective-options)))
          (should (eq (plist-get options :model) opus))
          (should (eq (plist-get options :variant) 'xhigh))
          (should-not (plist-get options :agent)))
        ;; A model only another engine runs shows what that engine would run.
        (setq drover-prompt-picked-model gpt)
        (let ((options (drover-prompt-effective-options)))
          (should (eq (plist-get options :model) gpt))
          (should (plist-get options :model-explicit-p))
          (should (eq (plist-get options :variant) 'medium))
          (should-not (plist-get options :variant-explicit-p))
          (should (equal (plist-get options :agent) "plan")))
        ;; The pickers' Default entry still names the last engine's default.
        (should (eq (plist-get (drover-prompt-inherited-options) :model) opus))
        (setq drover-prompt-picked-model nil
              drover-prompt--last-engine 'open)
        (let ((options (drover-prompt-effective-options)))
          (should (eq (plist-get options :model) gpt))
          (should (equal (plist-get options :agent) "plan")))
        ;; Before any engine took a prompt, the default engine's.
        (setq drover-prompt--last-engine nil
              drover-prompt--defaults `((fake :agent "build")))
        (drover-test-register-engine
         'fake :resolve (lambda (defaults agent model variant)
                          (list :agent (or agent (plist-get defaults :agent)) :model model
                                :variant variant)))
        (should (equal (plist-get (drover-prompt-effective-options) :agent) "build"))))))

(ert-deftest drover-prompt-test-header-shows-start-agent ()
  "An engine's starting agent shows as inherited; a chosen agent as chosen."
  (drover-prompt-test-with
    (drover-test-register-engine 'claude :resolve (lambda (defaults agent model variant)
                                                    (list :agent (or agent (plist-get defaults :agent))
                                                          :model model :variant variant)))
    (with-current-buffer (drover-prompt-buffer t)
      (setq drover-prompt--last-engine 'claude
            drover-prompt--defaults '((claude :agent "plan")))
      (let ((options (drover-prompt-effective-options)))
        (should (equal (plist-get options :agent) "plan"))
        (should-not (plist-get options :agent-explicit-p)))
      (should (equal (plist-get (drover-prompt-inherited-options) :agent) "plan"))
      (setq drover-prompt-picked-agent "build")
      (let ((options (drover-prompt-effective-options)))
        (should (equal (plist-get options :agent) "build"))
        (should (plist-get options :agent-explicit-p)))
      (should (equal (substring-no-properties (drover-prompt--minibuffer-info (current-buffer)))
                     (format " [cwd: %s] [agent: build]"
                             (abbreviate-file-name default-directory))))
      (setq drover-prompt-picked-model (drover-test-model "opus" :provider-name "Anthropic"
                                                          :engines '(claude))
            drover-prompt-picked-variant 'high
            drover-prompt-picked-agent nil
            drover-prompt-directory-picked-p t)
      (let ((info (drover-prompt--minibuffer-info (current-buffer))))
        (should (string-suffix-p " [agent: plan] [model: Anthropic: opus/high]" info))
        (should (eq (get-text-property (string-match "Anthropic: opus" info) 'face info)
                    'opencode-setting-chosen))
        (should (eq (get-text-property (string-match "plan" info) 'face info)
                    'opencode-setting-inherited))
        (should (eq (get-text-property (string-match "\\[cwd: \\(.\\)" info) 'face info)
                    'minibuffer-prompt))
        (should (eq (get-text-property (match-beginning 1) 'face info) 'opencode-setting-chosen))))))

(ert-deftest drover-prompt-test-picker-entry-options ()
  "Picker entries show what a send runs, choices applied the way sending applies them."
  (drover-prompt-test-with
    (let* ((opus (drover-test-model "claude-opus-5-5" :provider-name "Anthropic" :engines '(claude)))
           (fable (drover-test-model "claude-fable-5-1" :provider-name "Anthropic" :engines '(claude)))
           (gpt (drover-test-model "gpt-a" :provider-id "openai" :default-variant 'medium
                                   :engines '(open plain)))
           (terminal-options (list :model opus :variant 'xhigh))
           (shell-options (list :model gpt :variant 'high))
           (terminal (drover-test-session "t" :engine 'claude))
           (shell (drover-test-session "s" :engine 'open))
           (plain (drover-test-session "p" :engine 'plain))
           (new (list :backend 'fake :engine 'claude))
           (prompt (drover-prompt-buffer t)))
      (drover-test-register-engine
       'claude :resolve (lambda (defaults agent model variant)
                          (list :agent agent :model (or model (plist-get defaults :model))
                                :variant variant)))
      (drover-test-register-engine 'open :model-resets-variant t)
      (drover-test-register-engine 'plain)
      ;; A terminal changes its model only.
      (setq drover-test-settable '(("t" :model)))
      (with-current-buffer prompt
        (setq drover-prompt--defaults `((claude :model ,opus))))
      (should (equal (drover-prompt--entry-options new nil prompt)
                     (list :model opus :model-explicit-p nil :variant nil :variant-explicit-p nil)))
      (should-not (drover-prompt--entry-options new nil nil))
      (should (equal (drover-prompt--entry-options terminal terminal-options prompt) terminal-options))
      ;; A terminal takes a chosen model and keeps its effort; a new
      ;; session starts with both.
      (with-current-buffer prompt
        (setq drover-prompt-picked-model fable
              drover-prompt-picked-variant 'low))
      (should (equal (drover-prompt--entry-options terminal terminal-options prompt)
                     (list :model fable :model-explicit-p t :variant 'xhigh)))
      (should (equal (drover-prompt--entry-options new nil prompt)
                     (list :model fable :model-explicit-p t :variant 'low :variant-explicit-p t)))
      ;; A model the session cannot run drops both choices.
      (should (equal (drover-prompt--entry-options shell shell-options prompt) shell-options))
      ;; A session takes a lone variant.
      (with-current-buffer prompt
        (setq drover-prompt-picked-model nil))
      (should (equal (drover-prompt--entry-options shell shell-options prompt)
                     (list :model gpt :variant 'low :variant-explicit-p t)))
      ;; A chosen model runs its default variant where the engine resets
      ;; it, and keeps the session's elsewhere.
      (with-current-buffer prompt
        (setq drover-prompt-picked-model gpt
              drover-prompt-picked-variant nil))
      (should (equal (drover-prompt--entry-options shell shell-options prompt)
                     (list :model gpt :model-explicit-p t :variant 'medium :variant-explicit-p nil)))
      (should (equal (drover-prompt--entry-options plain shell-options prompt)
                     (list :model gpt :model-explicit-p t :variant 'high :variant-explicit-p nil)))
      (should (equal (substring-no-properties
                      (drover-prompt--options-label (drover-prompt--entry-options shell shell-options prompt)))
                     "openai: gpt-a / medium"))
      (should-not (drover-prompt--options-label nil)))))

;;;; Choosing what the prompt runs

(ert-deftest drover-prompt-test-two-agents-switch ()
  "With exactly two agents, choosing one switches to the other; with more, it asks."
  (drover-prompt-test-with
    (drover-test-register-engine 'fake :resolve (lambda (defaults agent model variant)
                                                  (list :agent (or agent (plist-get defaults :agent))
                                                        :model model :variant variant)))
    (setq drover-test-engine-agents '((fake "build" "plan")))
    (with-current-buffer (drover-prompt-buffer t)
      (setq drover-prompt--defaults '((fake :agent "build")))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) (ert-fail "Two agents switch without asking"))))
        (drover-prompt-choose-agent)
        (should (equal drover-prompt-picked-agent "plan"))
        (drover-prompt-choose-agent)
        (should (equal drover-prompt-picked-agent "build")))
      (setq drover-test-engine-agents '((fake "build" "plan" "review")))
      (drover-prompt-test-choosing (lambda (names)
                                     (should (equal names '("Default (build)" "build" "plan" "review")))
                                     "review")
        (drover-prompt-choose-agent))
      (should (equal drover-prompt-picked-agent "review")))))

(ert-deftest drover-prompt-test-choose-model-and-effort ()
  "A chosen model drops the variant chosen before; the effort is of the model run."
  (drover-prompt-test-with
    (let ((high (drover-test-model "high" :variants '((low) (high)) :default-variant 'high))
          (bare (drover-test-model "bare")))
      (drover-test-register-engine 'fake)
      (drover-register-model-source 'fake (lambda (_directory) (list high bare)))
      (with-current-buffer (drover-prompt-buffer t)
        (setq drover-prompt-picked-variant 'low)
        (drover-prompt-test-choosing (lambda (names)
                                       (should (equal names '("Default" "high" "bare")))
                                       "bare")
          (drover-prompt-choose-model))
        (should (eq drover-prompt-picked-model bare))
        (should-not drover-prompt-picked-variant)
        (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) (ert-fail "No variant"))))
          (drover-prompt-test-with-messages
            (drover-prompt-choose-effort)
            (setq drover-prompt-picked-model nil)
            (drover-prompt-choose-effort)
            (should (equal messages '("Choose a model first" "No variants for bare")))))
        (setq drover-prompt-picked-model high)
        (drover-prompt-test-choosing (lambda (names)
                                       (should (equal names '("Default (high)" "low" "high")))
                                       "low")
          (drover-prompt-choose-effort))
        (should (eq drover-prompt-picked-variant 'low))))))

(ert-deftest drover-prompt-test-choice-redraws-its-header-only ()
  "A choice draws again the header of the prompt it is made for, not every mode line."
  (drover-prompt-test-with
    (let ((bare (drover-test-model "bare"))
          (forced nil))
      (drover-test-register-engine 'fake)
      (drover-register-model-source 'fake (lambda (_directory) (list bare)))
      (with-current-buffer (drover-prompt-buffer t)
        (cl-letf (((symbol-function 'force-mode-line-update)
                   (lambda (&optional all) (push (cons (current-buffer) all) forced))))
          (drover-prompt-test-choosing (lambda (_names) "bare")
            (with-temp-buffer
              (drover-prompt-choose-model))))
        (should (eq drover-prompt-picked-model bare))
        (should (member (cons (current-buffer) nil) forced))
        (should-not (rassq t forced))))))

(ert-deftest drover-prompt-test-choose-directory ()
  "A chosen directory stays chosen, and the engines' defaults are read again for it."
  (drover-prompt-test-with
    (let ((drover-prompt-test--pending nil)
          (directory (file-name-as-directory (make-temp-file "drover-directory" t))))
      (unwind-protect
          (progn
            (drover-test-register-engine 'slow :defaults (drover-prompt-test--recording-defaults 'slow))
            (drover-test-register-engine 'quick :volatile t)
            (with-current-buffer (drover-prompt-buffer t)
              (cl-letf (((symbol-function 'read-directory-name) (lambda (&rest _) directory)))
                (drover-prompt-choose-directory))
              (should (equal default-directory directory))
              (should drover-prompt-directory-picked-p)
              (should (equal (mapcar #'car (drover-prompt-test--requests 'slow)) (list directory)))
              (should (equal drover-test-engine-calls `((:defaults quick ,directory))))))
        (delete-directory directory t)))))

(ert-deftest drover-prompt-test-options-from-anywhere ()
  "Choosing an option outside the prompt chooses it for the prompt, created when missing."
  (drover-prompt-test-with
    (drover-test-register-engine 'fake)
    (setq drover-test-engine-agents '((fake "build" "plan" "review")))
    (with-temp-buffer
      (drover-prompt-test-choosing (lambda (_) "review")
        (drover-prompt-choose-agent)))
    (should (equal (buffer-local-value 'drover-prompt-picked-agent (drover-prompt-buffer)) "review"))))

;;;; Context into the prompt

(ert-deftest drover-prompt-test-picks-survive-empty-prompt ()
  "Picks made on an empty prompt survive the next context; an unpicked directory follows it."
  (drover-prompt-test-with
    (let ((prompt (let ((default-directory "/")) (drover-prompt-default-buffer)))
          (model (drover-test-model "gpt-5.5" :provider-id "openai" :name "GPT-5.5")))
      (with-current-buffer prompt
        (setq drover-prompt-picked-model model
              drover-prompt-picked-variant 'high))
      (with-temp-buffer
        (setq default-directory temporary-file-directory)
        (insert "context line")
        (drover-prompt-dwim))
      (with-current-buffer prompt
        (should (string-match-p "context line" (buffer-string)))
        (should (eq drover-prompt-picked-model model))
        (should (eq drover-prompt-picked-variant 'high))
        (should (equal default-directory
                       (file-name-as-directory (expand-file-name temporary-file-directory))))
        ;; A chosen directory stays.
        (erase-buffer)
        (setq default-directory "/" drover-prompt-directory-picked-p t))
      (with-temp-buffer
        (setq default-directory temporary-file-directory)
        (drover-prompt-toggle))
      (should (equal (buffer-local-value 'default-directory prompt) "/")))))

(ert-deftest drover-prompt-test-dwim-on-heading ()
  "A heading line is quoted as any other line; the task prompt has a key of its own."
  (let (planned added)
    (cl-letf (((symbol-function 'drover-prompt-org-task) (lambda () (setq planned t)))
              ((symbol-function 'drover-prompt-add-context) (lambda () (setq added t))))
      (with-temp-buffer
        (let ((org-mode-hook nil)
              (transient-mark-mode t))
          (org-mode)
          (insert "* Task\nbody\n")
          (goto-char (point-min))
          (drover-prompt-dwim)
          (should (and added (not planned)))
          (setq added nil)
          ;; The former option changes nothing.
          (with-suppressed-warnings ((obsolete drover-prompt-org-headings-start-tasks))
            (let ((drover-prompt-org-headings-start-tasks t))
              (drover-prompt-dwim)))
          (should (and added (not planned)))
          (setq added nil)
          (setq-local drover-prompt-mode t)
          (should-error (drover-prompt-dwim) :type 'user-error)
          (should-not (or added planned)))))
    ;; No prefix argument any more.
    (should (equal (help-function-arglist 'drover-prompt-dwim) nil))
    (should (get 'drover-prompt-org-headings-start-tasks 'byte-obsolete-variable))))

(ert-deftest drover-prompt-test-dwim-quotes-heading ()
  "The heading line goes into the prompt, linked and quoted."
  (drover-prompt-test-with
    (with-temp-buffer
      (let ((org-mode-hook nil))
        (org-mode)
        (insert "* TODO Ship it\nbody\n")
        (goto-char (point-min))
        (drover-prompt-dwim)
        (with-current-buffer (drover-prompt-buffer)
          (should (string-match-p "\\`From buffer .* line 1:\n#\\+begin_src org\n,\\* TODO Ship it\n#\\+end_src\n\n\\'"
                                  (buffer-string))))))))

(ert-deftest drover-prompt-test-context-at-last-point ()
  "Context added while the prompt is hidden goes where the user left point, in order."
  (drover-prompt-test-with
    (cl-letf (((symbol-function 'drover-prompt-format-context)
               (lambda (_buffer text _language) (concat "<" text ">"))))
      (let ((prompt (drover-prompt-default-buffer)))
        (with-current-buffer prompt
          (insert "first\nsecond")
          (goto-char (point-min))
          (end-of-line)
          (run-hooks 'post-command-hook)
          (goto-char (point-max)))
        (with-temp-buffer
          (insert "one")
          (drover-prompt-add-context)
          (erase-buffer)
          (insert "two")
          (drover-prompt-add-context))
        (with-current-buffer prompt
          (should (equal (buffer-string) "first\n\n<one>\n\n<two>\nsecond")))))))

(ert-deftest drover-prompt-test-context-into-shown-prompt ()
  "Context goes at point in the prompt's window when it shows."
  (drover-prompt-test-with
    (cl-letf (((symbol-function 'drover-prompt-format-context)
               (lambda (_buffer text _language) (concat "<" text ">"))))
      (let ((prompt (drover-prompt-default-buffer))
            (window (selected-window)))
        (with-current-buffer prompt
          (insert "first\nsecond"))
        (save-window-excursion
          (set-window-buffer window prompt)
          (set-window-point window 6)
          (with-temp-buffer
            (insert "one")
            (drover-prompt-add-context))
          (should (equal (with-current-buffer prompt (buffer-string))
                         "first\n\n<one>\nsecond")))))))

(ert-deftest drover-prompt-test-first-context ()
  "The first context begins the prompt in the source's directory, the region quoted."
  (drover-prompt-test-with
    (let ((transient-mark-mode t))
      (with-temp-buffer
        (setq default-directory "/")
        (emacs-lisp-mode)
        (insert "(one)\n(two)\n")
        (set-mark (point-min))
        (goto-char (point-max))
        (activate-mark)
        (drover-prompt-add-context)
        (with-current-buffer (drover-prompt-buffer)
          (should (equal default-directory "/"))
          (should (string-match-p "\\`From buffer .* line 3:\n#\\+begin_src emacs-lisp\n(one)\n(two)\n#\\+end_src\n\n\\'"
                                  (buffer-string)))
          (should (eobp)))))))

(ert-deftest drover-prompt-test-task-keeps-draft ()
  "Starting a task's prompt asks before replacing a draft, and keeps picks made on an empty prompt."
  (drover-prompt-test-with
    (cl-letf (((symbol-function 'org-id-get-create) (lambda (&rest _) "TASK-ID")))
      (let ((prompt (drover-prompt-default-buffer))
            (model (drover-test-model "gpt-5.5" :provider-id "openai" :name "GPT-5.5")))
        (with-current-buffer prompt
          (setq drover-prompt-picked-model model))
        (with-temp-buffer
          (let ((org-mode-hook nil))
            (org-mode)
            (insert "* TODO Ship it :tag:\nbody\n")
            (goto-char (point-max))
            (drover-prompt-org-task)
            (with-current-buffer prompt
              (should (equal (buffer-string)
                             "Work on [[id:TASK-ID][Ship it]]. Investigate fully and provide a plan."))
              (should (equal drover-prompt-picked-agent "plan"))
              (should (equal drover-prompt-title "Ship it"))
              (should (eq drover-prompt-picked-model model))
              (erase-buffer)
              (insert "my draft"))
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil)))
              (should-error (drover-prompt-org-task) :type 'user-error))
            (should (equal (with-current-buffer prompt (buffer-string)) "my draft"))
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
              (let ((drover-prompt-task-format "Plan %s.")
                    (drover-prompt-task-agent nil))
                (drover-prompt-org-task)))
            (with-current-buffer prompt
              (should (equal (buffer-string) "Plan [[id:TASK-ID][Ship it]]."))
              (should-not drover-prompt-picked-model)
              (should-not drover-prompt-picked-agent))))))))

(ert-deftest drover-prompt-test-task-needs-heading ()
  "A task prompt starts only from under an Org heading, outside the prompt."
  (drover-prompt-test-with
    (with-temp-buffer
      (should-error (drover-prompt-org-task) :type 'user-error)
      (let ((org-mode-hook nil))
        (org-mode)
        (insert "text before\n* Heading\n")
        (goto-char (point-min))
        (should-error (drover-prompt-org-task) :type 'user-error)
        ;; Narrowed to the text before the first heading, still none.
        (narrow-to-region (point-min) (line-end-position))
        (should-error (drover-prompt-org-task) :type 'user-error)
        (widen)
        (goto-char (point-max))
        (setq-local drover-prompt-mode t)
        (should-error (drover-prompt-org-task) :type 'user-error)))
    (should-not (drover-prompt-buffer))))

(ert-deftest drover-prompt-test-task-anywhere-in-entry ()
  "The task is the heading point is under, from its body, folded or narrowed to it."
  (drover-prompt-test-with
    (let ((ids nil))
      (cl-letf (((symbol-function 'org-id-get-create)
                 (lambda (&rest _)
                   (push (org-get-heading t t t t) ids)
                   (concat "ID-" (car ids))))
                ((symbol-function 'y-or-n-p) #'always))
        (with-temp-buffer
          (let ((org-mode-hook nil))
            (org-mode)
            (insert "* Parent\nparent body\n** Child\nchild body\nmore\n* Next\n")
            (cl-flet ((task-at (text)
                        (goto-char (point-min))
                        (search-forward text)
                        (drover-prompt-org-task)
                        (with-current-buffer (drover-prompt-buffer)
                          drover-prompt-title)))
              (should (equal (task-at "parent body") "Parent"))
              (should (equal (task-at "more") "Child"))
              (should (equal (task-at "* Next") "Next"))
              ;; Folded: point within the hidden body.
              (goto-char (point-min))
              (search-forward "** Child")
              (org-fold-subtree t)
              (search-forward "child body")
              (should (invisible-p (point)))
              (drover-prompt-org-task)
              (should (equal (buffer-local-value 'drover-prompt-title (drover-prompt-buffer)) "Child"))
              (org-fold-show-all)
              ;; Narrowed to the body, the heading out of reach.
              (goto-char (point-min))
              (search-forward "child body")
              (narrow-to-region (line-beginning-position) (line-end-position))
              (drover-prompt-org-task)
              (should (equal (buffer-substring (point-min) (point-max)) "child body"))
              (should (equal (buffer-local-value 'drover-prompt-title (drover-prompt-buffer)) "Child"))
              (should (equal (with-current-buffer (drover-prompt-buffer) (buffer-string))
                             "Work on [[id:ID-Child][Child]]. Investigate fully and provide a plan."))
              (widen))))))))

(ert-deftest drover-prompt-test-attach ()
  "A buffer is quoted whole, a project file linked; the prompt's own region is refused."
  (drover-prompt-test-with
    (let* ((directory (file-name-as-directory (make-temp-file "drover-attach-" t)))
           (file (expand-file-name "a.txt" directory))
           (source (generate-new-buffer "attach-source.el"))
           (clash (generate-new-buffer
                   (concat (file-name-nondirectory (directory-file-name directory)) "/a.txt")))
           (prompt (let ((default-directory directory)) (drover-prompt-default-buffer))))
      (unwind-protect
          (cl-letf (((symbol-function 'project-current)
                     (lambda (&rest _) (cons 'transient directory))))
            (with-temp-file file (insert "text"))
            (with-current-buffer source
              (emacs-lisp-mode)
              (insert "(foo)\n"))
            (let ((candidates (drover-prompt--attach-candidates prompt)))
              (should (member "attach-source.el" candidates))
              (should-not (member (buffer-name prompt) candidates))
              (should (member (concat (buffer-name clash) " (file)") candidates)))
            (with-current-buffer prompt
              (insert "Look:")
              (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "attach-source.el")))
                (drover-prompt-insert-attachment))
              (should (string-match-p "\\`Look:\n\nFrom buffer attach-source.el line 1:\n#\\+begin_src emacs-lisp\n(foo)\n#\\+end_src\n\n\\'"
                                      (buffer-string)))
              (cl-letf (((symbol-function 'completing-read)
                         (lambda (&rest _) (concat (buffer-name clash) " (file)"))))
                (drover-prompt-insert-attachment))
              (should (string-suffix-p (format "From [[file:%s][a.txt]]\n\n" (abbreviate-file-name file))
                                       (buffer-string)))
              (let ((transient-mark-mode t))
                (set-mark (point-min))
                (activate-mark)
                (should-error (drover-prompt-insert-attachment) :type 'user-error)))
            (with-temp-buffer
              (should-error (drover-prompt-insert-attachment) :type 'user-error)))
        (kill-buffer source)
        (kill-buffer clash)
        (delete-directory directory t)))))

(ert-deftest drover-prompt-test-copy-block ()
  "The src block at point is copied unescaped and without its common indentation."
  (with-temp-buffer
    (let ((org-mode-hook nil)
          (kill-ring nil))
      (org-mode)
      (insert "Text\n#+begin_src sh\n  ls\n  ,* x\n#+end_src\n")
      (goto-char (point-min))
      (should-error (drover-prompt-copy-src-block) :type 'user-error)
      (search-forward "ls")
      (drover-prompt-copy-src-block)
      (should (equal (car kill-ring) "ls\n* x\n")))))

;;;; Showing and hiding

(ert-deftest drover-prompt-test-toggle ()
  "The prompt shows in its side window, and hides from there or from elsewhere."
  (drover-test-with-backend nil
    (drover-test-with-engines
      (let ((drover-prompt-buffer-name "*drover prompt test*")
            (drover-prompt-require-features nil)
            (drover-prompt-default-engine 'fake)
            (org-mode-hook nil))
        (save-window-excursion
          (delete-other-windows)
          (unwind-protect
              (cl-letf (((symbol-function 'drover-prompt--schedule-refresh) #'ignore))
                (with-temp-buffer
                  (setq default-directory "/")
                  (let ((source (current-buffer)))
                    (drover-prompt-toggle)
                    (let* ((prompt (drover-prompt-buffer))
                           (window (get-buffer-window prompt)))
                      (should window)
                      (should (eq (selected-window) window))
                      (should (eq (window-parameter window 'window-side) 'bottom))
                      (should (equal (buffer-local-value 'default-directory prompt) "/"))
                      ;; From the prompt itself, it hides.
                      (with-current-buffer prompt
                        (drover-prompt-toggle))
                      (should-not (window-live-p window))
                      (should (buffer-live-p prompt))
                      ;; Current but hidden, it stays hidden.
                      (with-current-buffer prompt
                        (drover-prompt-toggle))
                      (should-not (get-buffer-window prompt t))
                      (with-current-buffer source
                        (drover-prompt-toggle))
                      (setq window (get-buffer-window prompt))
                      (should (eq (selected-window) window))
                      ;; Shown again, it is selected rather than shown twice.
                      (select-window (window-main-window))
                      (with-current-buffer source
                        (drover-prompt-dwim))
                      (should (eq (selected-window) window))
                      ;; From elsewhere too, it hides.
                      (select-window (window-main-window))
                      (with-current-buffer source
                        (drover-prompt-toggle))
                      (should-not (window-live-p window))))))
            (when-let* ((buffer (get-buffer drover-prompt-buffer-name)))
              (kill-buffer buffer))))))))

(ert-deftest drover-prompt-test-display-reuses-window ()
  "A prompt shown already is selected where it shows, not shown again."
  (drover-test-with-backend nil
    (drover-test-with-engines
      (let ((drover-prompt-buffer-name "*drover prompt test*")
            (drover-prompt-require-features nil)
            (org-mode-hook nil))
        (save-window-excursion
          (delete-other-windows)
          (unwind-protect
              (cl-letf (((symbol-function 'drover-prompt--schedule-refresh) #'ignore))
                (let* ((prompt (drover-prompt-default-buffer))
                       (shown (split-window))
                       (source (generate-new-buffer "drover source")))
                  (set-window-buffer shown prompt)
                  (set-window-buffer (selected-window) source)
                  (with-current-buffer source
                    (insert "context")
                    (drover-prompt-dwim))
                  (kill-buffer source)
                  (should (eq (selected-window) shown))
                  (should (equal (get-buffer-window-list prompt nil t) (list shown)))))
            (when-let* ((buffer (get-buffer drover-prompt-buffer-name)))
              (kill-buffer buffer))))))))

;;;; Content and what leaves the prompt

(ert-deftest drover-prompt-test-quote-another-host ()
  "Quoting another host's session leaves the prompt where it is: its directory is not here."
  (drover-test-with-backend (list (drover-test-session "r" :directory "/on/core/" :host "core")
                                  (drover-test-session "h" :directory "/work/h/"))
    (let ((added nil)
          (default-directory "/work/here/"))
      (cl-letf (((symbol-function 'drover-prompt--add)
                 (lambda (_text directory &rest _) (push directory added)))
                ((symbol-function 'drover-session-link) (lambda (_session) "[[link]]")))
        (drover-prompt-quote-sessions (list (drover-find 'fake "r")))
        (drover-prompt-quote-sessions (list (drover-find 'fake "h"))))
      (should (equal added '("/work/h/" "/work/here/"))))))
(ert-deftest drover-prompt-test-content ()
  "The content is the region when active, else the whole prompt, trimmed; nil when blank."
  (drover-prompt-test-with
    (should-not (drover-prompt-content))
    (with-current-buffer (drover-prompt-buffer t)
      (should-not (drover-prompt-content))
      (insert "  \n\n")
      (should-not (drover-prompt-content))
      (erase-buffer)
      (insert "keep\n#+begin_src sh\nls\n#+end_src\n")
      (let ((content (drover-prompt-content)))
        (should (equal (plist-get content :org) "keep\n#+begin_src sh\nls\n#+end_src"))
        (should (equal (plist-get content :text) "keep\n```sh\nls\n```"))
        (should-not (plist-get content :region))
        (should (= (plist-get content :start) (point-min)))
        (should (= (plist-get content :end) (point-max))))
      (let ((transient-mark-mode t))
        (goto-char (point-min))
        (set-mark (point))
        (forward-line 1)
        (activate-mark)
        (let ((content (drover-prompt-content (current-buffer))))
          (should (equal (plist-get content :org) "keep"))
          (should (plist-get content :region))
          (should (markerp (plist-get content :start)))
          (should (= (plist-get content :end) (point))))
        ;; An empty region is no region.
        (set-mark (point))
        (should-not (plist-get (drover-prompt-content) :region))))))

(ert-deftest drover-prompt-test-consume ()
  "A region leaves alone unless only blanks remain; the whole prompt leaves with its window."
  (drover-prompt-test-with
    (let ((transient-mark-mode t)
          (prompt (drover-prompt-buffer t)))
      (with-current-buffer prompt
        (insert "keep\nsend\n")
        (set-mark (point))
        (forward-line -1)
        (activate-mark)
        (drover-prompt-consume (drover-prompt-content))
        (should (equal (buffer-string) "keep\n"))
        (should-not mark-active)
        (goto-char (point-min))
        (set-mark (point))
        (forward-line 1)
        (activate-mark)
        (drover-prompt-consume (drover-prompt-content)))
      (should-not (buffer-live-p prompt))
      (setq prompt (drover-prompt-buffer t))
      (with-current-buffer prompt
        (insert "all"))
      (save-window-excursion
        (delete-other-windows)
        (let ((window (split-window)))
          (set-window-buffer window prompt)
          (drover-prompt-consume (drover-prompt-content))
          (should-not (buffer-live-p prompt))
          (should-not (window-live-p window))
          ;; The last window of a frame stays.
          (setq prompt (drover-prompt-buffer t))
          (with-current-buffer prompt
            (insert "all"))
          (delete-other-windows)
          (set-window-buffer (selected-window) prompt)
          (drover-prompt-consume (drover-prompt-content))
          (should-not (buffer-live-p prompt))
          (should (window-live-p (selected-window))))))))

(ert-deftest drover-prompt-test-erase ()
  "Clearing erases the text, region or not, and keeps the buffer, its window and the picks."
  (drover-prompt-test-with
    (let ((transient-mark-mode t)
          (prompt (drover-prompt-buffer t)))
      (save-window-excursion
        (set-window-buffer (selected-window) prompt)
        (with-current-buffer prompt
          (insert "some text")
          (setq drover-prompt-picked-agent "plan")
          (set-mark (point-min))
          (activate-mark)
          (drover-prompt-run 'clear)
          (should (equal (buffer-string) ""))
          (should-not mark-active)
          (should (equal drover-prompt-picked-agent "plan")))
        (should (eq (window-buffer (selected-window)) prompt))
        (should (buffer-live-p prompt))))))

;;;; Options for a running session

(ert-deftest drover-prompt-test-explicit-options ()
  "The prompt's choices, an empty agent as none; none without a prompt."
  (drover-prompt-test-with
    (should (equal (drover-prompt-explicit-options) '(:agent nil :model nil :variant nil)))
    (let ((model (drover-test-model "m")))
      (with-current-buffer (drover-prompt-buffer t)
        (setq drover-prompt-picked-agent ""
              drover-prompt-picked-model model
              drover-prompt-picked-variant 'high))
      (should (equal (drover-prompt-explicit-options) (list :agent nil :model model :variant 'high)))
      (with-current-buffer (drover-prompt-buffer)
        (setq drover-prompt-picked-agent "plan"))
      (should (equal (drover-prompt-explicit-options (drover-prompt-buffer))
                     (list :agent "plan" :model model :variant 'high))))))

(ert-deftest drover-prompt-test-options-for ()
  "A model the session's engine cannot run goes with its variant, with a warning when asked."
  (drover-prompt-test-with
    (let* ((runs (drover-test-model "runs"))
           (other (drover-test-model "other" :engines '(other)))
           (qualified (drover-test-model "bare" :engines '(opencode)))
           (session (drover-test-session "a" :engine 'fake)))
      (drover-prompt-test-with-warnings
        (should (equal (drover-prompt-options-for session (list :agent "plan" :model runs :variant 'high))
                       (list :agent "plan" :model runs :variant 'high)))
        (should (equal (drover-prompt-options-for session (list :agent "plan" :model other :variant 'high))
                       (list :agent "plan" :model nil :variant nil)))
        (should-not warnings)
        (should (equal (drover-prompt-options-for session (list :model other :variant 'high) t)
                       (list :agent nil :model nil :variant nil)))
        (should (equal warnings '("other cannot be applied to Session a; keeping its current model")))
        ;; OpenCode takes provider-qualified models only.
        (should-not (plist-get (drover-prompt-options-for (drover-test-session "o" :engine 'opencode)
                                                          (list :model qualified))
                               :model))
        (should (equal (drover-prompt-options-for session (list :variant 'low))
                       (list :agent nil :model nil :variant 'low)))))))

(ert-deftest drover-prompt-test-apply-options ()
  "Options are filtered, then given to the backend, which calls THEN; THEN runs once whatever happens."
  (drover-prompt-test-with
    (let ((other (drover-test-model "other" :engines '(other)))
          (session (drover-test-session "a" :engine 'fake))
          (ran 0))
      (drover-prompt-test-with-warnings
        (drover-prompt-apply-options session (list :agent "plan" :model other :variant 'high)
                                     (lambda () (cl-incf ran)))
        (should (= ran 1))
        (should (equal drover-test-calls '((:apply-options "a" (:agent "plan" :model nil :variant nil)))))
        (should (equal (length warnings) 1))
        ;; Without a session, or without the operation, THEN runs at once.
        (drover-prompt-apply-options nil (list :agent "plan" :model other) (lambda () (cl-incf ran)))
        (should (= ran 2))
        (drover-test-register-simple 'simple nil)
        (drover-prompt-apply-options (drover-session-create :backend 'simple :id "s") '(:agent "plan")
                                     (lambda () (cl-incf ran)))
        (should (= ran 3))
        ;; A backend failing after calling THEN does not call it twice.
        (drover-test-register-simple 'broken nil
                                     :apply-options (lambda (_session _options then)
                                                      (funcall then)
                                                      (error "Broken")))
        (setq warnings nil)
        (drover-prompt-apply-options (drover-session-create :backend 'broken :id "b" :title "B")
                                     '(:agent "plan") (lambda () (cl-incf ran)))
        (should (= ran 4))
        (should (equal warnings
                       '("Could not apply the options to B: Broken; sending the prompt anyway")))))))

;;;; The target picker

(ert-deftest drover-prompt-test-target-order ()
  "Running sessions that take prompts and have a buffer, the current one first, then most recent."
  (drover-prompt-test-with
    (let ((a (drover-prompt-test--session "a"))
          (b (drover-prompt-test--session "b")))
      (drover-prompt-test--session "c")
      ;; A session without a buffer, or whose backend takes no prompt, is no target.
      (setq drover-test-sessions (append drover-test-sessions (list (drover-test-session "nobuffer"))))
      (let ((other (generate-new-buffer "simple session")))
        (push (cons "simple" other) drover-prompt-test-buffers)
        (drover-test-register-simple 'simple (list (drover-session-create :backend 'simple :id "x"))
                                     :buffer (lambda (_) other)))
      (save-window-excursion
        (switch-to-buffer (drover-prompt-test--buffer-of "c"))
        (switch-to-buffer (drover-prompt-test--buffer-of "a"))
        (with-current-buffer (drover-prompt-test--buffer-of "b")
          (drover-prompt-test-choosing (lambda (names) (cadr names))
            (should (eq (drover-prompt-read-target "To: ") a))
            (should (equal offered '("session b" "session a" "session c")))
            (should (eq (completion-metadata-get metadata 'category) 'drover-target))
            (should (eq (completion-metadata-get metadata 'display-sort-function) #'identity)))
          (drover-prompt-test-choosing (lambda (names) (car names))
            (should (eq (drover-prompt-read-target "To: " :existing-only t) b)))
          ;; Two sessions in one buffer are told apart.
          (drover-test-register-simple 'twin (list (drover-session-create :backend 'twin :id "t"))
                                       :buffer (lambda (_) (drover-prompt-test--buffer-of "b"))
                                       :send #'ignore)
          (drover-prompt-test-choosing (lambda (names) (cadr names))
            (should (eq (drover-session-backend (drover-prompt-read-target "To: ")) 'fake))
            (should (equal (seq-take offered 2) '("session b" "session b <2>")))))))))

(ert-deftest drover-prompt-test-target-none ()
  "Without a target, the picker says so rather than asking."
  (drover-prompt-test-with
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) (ert-fail "Nothing to choose"))))
      (should-error (drover-prompt-read-target "To: ") :type 'user-error)
      (let ((drover-prompt-new-targets '((:backend fake :engine fake :feature drover :label "Fake"))))
        (should-error (drover-prompt-read-target "To: " :existing-only t) :type 'user-error)))))

(ert-deftest drover-prompt-test-new-targets ()
  "New sessions show when their feature is installed and their engine runs the model chosen."
  (drover-prompt-test-with
    (let ((drover-prompt-new-targets
           '((:backend fake :engine fake :feature drover :label "Fake session")
             (:backend fake :engine fake :feature drover-prompt-test-missing :label "Missing")
             (:backend fake :engine other :label "Other session")))
          (other (drover-test-model "other" :engines '(other))))
      (drover-prompt-test--session "a")
      (drover-prompt-test-choosing (lambda (names) (car (last names)))
        (should (equal (drover-prompt-read-target "To: ")
                       '(:backend fake :engine other :label "Other session")))
        (should (equal offered '("session a" "[New Fake session…]" "[New Other session…]")))
        (with-current-buffer (drover-prompt-buffer t)
          (setq drover-prompt-picked-model other))
        (drover-prompt-read-target "To: ")
        (should (equal offered '("session a" "[New Other session…]")))
        (drover-prompt-read-target "To: " :existing-only t)
        (should (equal offered '("session a")))))))

(ert-deftest drover-prompt-test-target-annotation ()
  "Candidates show their status, backend and engine, what they would run, directory and summary."
  (drover-prompt-test-with
    (let ((drover-prompt-new-targets '((:backend fake :engine other :label "Other session")))
          (opencode-session-status-icons '((waiting . "W") (idle . "I")))
          (model (drover-test-model "gpt" :provider-id "openai" :engines '(fake other))))
      (drover-test-register-engine 'fake :label "Fake")
      (drover-test-register-engine 'other :label "Other")
      (drover-prompt-test--session "a" :status 'waiting :directory "/tmp/"
                                   :requests '((:kind question :summary "Which one?")))
      (setq drover-test-options `(("a" :model ,model :variant high)))
      (let ((prompt (drover-prompt-buffer t)))
        (with-current-buffer prompt
          (setq default-directory "/"))
        (drover-prompt-test-choosing (lambda (names) (car names))
          (drover-prompt-read-target "To: ")
          (let ((annotate (completion-metadata-get metadata 'annotation-function))
                (pad (- (string-width "[New Other session…]") (string-width "session a"))))
            (should (equal (substring-no-properties (funcall annotate "session a"))
                           (concat (make-string pad ?\s)
                                   "  W [Fake]  openai: gpt / high  /tmp/  asks: Which one?")))
            (should (equal (substring-no-properties (funcall annotate "[New Other session…]"))
                           "    [Fake (Other)]  /"))
            ;; Choices made in the picker show at once.
            (with-current-buffer prompt
              (setq drover-prompt-picked-variant 'low))
            (should (string-match-p "openai: gpt / low  /tmp/"
                                    (funcall annotate "session a")))))))))

(ert-deftest drover-prompt-test-picker-refreshes-defaults ()
  "Opening the picker reads the volatile defaults again, unless it offers running sessions only."
  (drover-prompt-test-with
    (drover-test-register-engine 'quick :volatile t)
    (setq drover-test-engine-defaults '((quick :model "quick"))
          drover-test-engine-last-used '((quick . 10.0)))
    (drover-prompt-test--session "a")
    (let ((prompt (drover-prompt-buffer t)))
      (drover-prompt-test-choosing (lambda (names) (car names))
        (drover-prompt-read-target "To: " :existing-only t)
        (should-not drover-test-engine-calls)
        (drover-prompt-read-target "To: ")
        (should (equal drover-test-engine-calls
                       `((:defaults quick ,(buffer-local-value 'default-directory prompt)))))
        (with-current-buffer prompt
          (should (eq drover-prompt--last-engine 'quick))
          (should (equal drover-prompt--defaults '((quick :model "quick")))))))))

(ert-deftest drover-prompt-test-picker-option-menu ()
  "The picker offers the options menu for new sessions, or when asked for running ones."
  (drover-prompt-test-with
    (let ((drover-prompt-new-targets '((:backend fake :engine fake :feature drover :label "Fake")))
          (session (drover-prompt-test--session "a"))
          keymaps)
      (drover-prompt-buffer t)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _)
                   (with-temp-buffer
                     (use-local-map (make-sparse-keymap))
                     (run-hooks 'minibuffer-setup-hook)
                     (should drover-prompt--picker)
                     (push (command-remapping #'drover-dispatch nil (list (current-local-map)))
                           keymaps))
                   "session a")))
        (should (eq (drover-prompt-read-target "To: ") session))
        (should (eq (drover-prompt-read-target "To: " :existing-only t) session))
        (should (eq (drover-prompt-read-target "To: " :existing-only t :options-menu t) session)))
      (should (equal (nreverse keymaps)
                     '(drover-prompt-options-menu nil drover-prompt-options-menu))))))

(ert-deftest drover-prompt-test-picker-shows-choices ()
  "The picker's minibuffer shows what the prompt runs, and follows the choices made there."
  (drover-prompt-test-with
    (drover-test-register-engine 'fake)
    (setq drover-test-engine-agents '((fake "build" "plan" "review")))
    (let ((prompt (drover-prompt-buffer t))
          (minibuffer (generate-new-buffer " *drover test minibuffer*")))
      (unwind-protect
          (save-window-excursion
            (set-window-buffer (selected-window) minibuffer)
            (with-current-buffer prompt
              (setq default-directory "/"))
            (with-current-buffer minibuffer
              (drover-prompt--setup-picker nil)
              (should (equal (overlay-get drover-prompt--minibuffer-overlay 'after-string)
                             " [cwd: /]"))
              (set (make-local-variable 'vertico--input) "typed"))
            (cl-letf (((symbol-function 'active-minibuffer-window) #'selected-window))
              (drover-prompt-test-choosing (lambda (_) "plan")
                (with-current-buffer prompt
                  (drover-prompt-choose-agent))))
            (with-current-buffer minibuffer
              (should (equal (substring-no-properties
                              (overlay-get drover-prompt--minibuffer-overlay 'after-string))
                             " [cwd: /] [agent: plan]"))
              (should (eq (buffer-local-value 'vertico--input minibuffer) t))
              (run-hooks 'minibuffer-exit-hook)
              (should-not drover-prompt--minibuffer-overlay)))
        (kill-buffer minibuffer)))))

;;;; Sending

(defun drover-prompt-test--prompt (text)
  "Return the prompt buffer, created with TEXT."
  (with-current-buffer (drover-prompt-buffer t)
    (insert text)
    (current-buffer)))

(ert-deftest drover-prompt-test-send-region ()
  "A region goes to a running session as Markdown, after its checks and options; the rest stays."
  (drover-prompt-test-with
    (let* ((transient-mark-mode t)
           (model (drover-test-model "m"))
           (prompt (drover-prompt-test--prompt "keep\n#+begin_src sh\nls\n#+end_src\nkeep too")))
      (drover-prompt-test--session "a")
      (with-current-buffer prompt
        (setq drover-prompt-picked-model model)
        (goto-char (point-min))
        (forward-line 1)
        (set-mark (point))
        (forward-line 3)
        (activate-mark)
        (drover-prompt-test-choosing (lambda (_) "session a")
          (drover-prompt-run 'send))
        (should (equal (buffer-string) "keep\nkeep too"))
        (should-not mark-active))
      (should (equal (car kill-ring) "#+begin_src sh\nls\n#+end_src"))
      (should (equal (reverse drover-test-calls)
                     `((:check "a" "```sh\nls\n```" (:agent nil :model ,model :variant nil))
                       (:apply-options "a" (:agent nil :model ,model :variant nil))
                       (:send "a" "```sh\nls\n```" fake-token)))))))

(ert-deftest drover-prompt-test-send-whole ()
  "The whole prompt goes, and the prompt with its window, the options filtered for the session."
  (drover-prompt-test-with
    (let ((prompt (drover-prompt-test--prompt "  all of it\n"))
          (other (drover-test-model "other" :engines '(other))))
      (drover-prompt-test--session "a")
      (with-current-buffer prompt
        (setq drover-prompt-picked-model other
              drover-prompt-picked-variant 'high
              drover-prompt-picked-agent "plan"))
      (save-window-excursion
        (delete-other-windows)
        (let ((window (split-window)))
          (set-window-buffer window prompt)
          (drover-prompt-test-with-warnings
            (drover-prompt-test-choosing (lambda (_) "session a")
              (with-current-buffer prompt
                (drover-prompt-submit)))
            (should (equal warnings '("other cannot be applied to Session a; keeping its current model"))))
          (should-not (buffer-live-p prompt))
          (should-not (window-live-p window))))
      (should (equal (car kill-ring) "all of it"))
      (should (equal (reverse drover-test-calls)
                     '((:check "a" "all of it" (:agent "plan" :model nil :variant nil))
                       (:apply-options "a" (:agent "plan" :model nil :variant nil))
                       (:send "a" "all of it" fake-token)))))))

(ert-deftest drover-prompt-test-send-cancelled ()
  "A check cancelling the send sends nothing and keeps the prompt, region included."
  (drover-prompt-test-with
    (let ((transient-mark-mode t)
          (prompt (drover-prompt-test--prompt "one\ntwo")))
      (drover-prompt-test--session "a")
      (setq drover-test-check-result 'cancel)
      (with-current-buffer prompt
        (goto-char (point-min))
        (set-mark (point))
        (end-of-line)
        (activate-mark)
        (drover-prompt-test-with-messages
          (drover-prompt-test-choosing (lambda (_) "session a")
            (drover-prompt-submit))
          (should (equal messages '("Not sent to Session a; the prompt is in the kill ring"))))
        (should (equal (buffer-string) "one\ntwo"))
        (should mark-active))
      (should (equal (car kill-ring) "one"))
      (should (equal drover-test-calls '((:check "a" "one" (:agent nil :model nil :variant nil))))))))

(ert-deftest drover-prompt-test-send-empty ()
  "An empty prompt is not sent."
  (drover-prompt-test-with
    (drover-prompt-test--session "a")
    (with-current-buffer (drover-prompt-test--prompt " \n")
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) (ert-fail "Nothing to send"))))
        (should-error (drover-prompt-submit) :type 'user-error)))
    (should-not drover-test-calls)))

(defun drover-prompt-test--register (name title &rest properties)
  "Register backend NAME serving one session, TITLE, in a buffer named TITLE.
PROPERTIES are more of its properties.  Return the buffer."
  (let ((buffer (generate-new-buffer title)))
    (push (cons title buffer) drover-prompt-test-buffers)
    (apply #'drover-test-register-simple name
           (list (drover-session-create :backend name :id title :title title))
           :buffer (lambda (_) buffer)
           properties)
    buffer))

(ert-deftest drover-prompt-test-send-to-dead-session ()
  "A session whose buffer died while its options were applied is reported, not sent to."
  (drover-prompt-test-with
    (let* ((pending nil)
           (buffer (drover-prompt-test--register
                    'slow "Slow"
                    :apply-options (lambda (_session _options then) (setq pending then))
                    :send (lambda (&rest _) (ert-fail "Sent to a dead session")))))
      (with-current-buffer (drover-prompt-test--prompt "text")
        (drover-prompt-test-choosing (lambda (_) "Slow")
          (drover-prompt-submit)))
      (should-not (drover-prompt-buffer))
      (kill-buffer buffer)
      (drover-prompt-test-with-warnings
        (funcall pending)
        (should (equal warnings '("Slow died before the prompt was sent; it is in the kill ring")))))))

(ert-deftest drover-prompt-test-send-failure ()
  "A backend failing to send is reported."
  (drover-prompt-test-with
    (drover-prompt-test--register 'broken "Broken" :send (lambda (&rest _) (error "Gone")))
    (drover-prompt-test-with-warnings
      (with-current-buffer (drover-prompt-test--prompt "text")
        (drover-prompt-test-choosing (lambda (_) "Broken")
          (drover-prompt-submit)))
      (should (equal warnings '("Broken did not take the prompt: Gone; it is in the kill ring"))))
    (should-not (drover-prompt-buffer))))

(ert-deftest drover-prompt-test-send-new ()
  "A new session starts with the prompt's choices, title and engine defaults, then takes the text."
  (drover-prompt-test-with
    (let ((drover-prompt-new-targets '((:backend fake :engine fake :feature drover :label "Fake session")))
          (model (drover-test-model "m"))
          (prompt (drover-prompt-test--prompt "Do it")))
      (with-current-buffer prompt
        (setq default-directory "/"
              drover-prompt-picked-agent "plan"
              drover-prompt-picked-model model
              drover-prompt-title "Ship it"
              drover-prompt--defaults '((fake :x 1) (other :y 2))))
      (with-current-buffer prompt
        (drover-prompt-test-choosing (lambda (_) "[New Fake session…]")
          (drover-prompt-submit)))
      (should-not (buffer-live-p prompt))
      (should (equal (reverse drover-test-calls)
                     `((:create "/" fake (:agent "plan" :model ,model :variant nil
                                                 :title "Ship it" :defaults (:x 1)))
                       (:send "new-0" "Do it" nil)))))))

(ert-deftest drover-prompt-test-send-new-refused ()
  "A new session refused at once keeps the prompt; refused later, the prompt went already."
  (drover-prompt-test-with
    (let ((drover-prompt-new-targets '((:backend fake :engine fake :label "Fake session")))
          (prompt (drover-prompt-test--prompt "Do it")))
      (setq drover-test-create-error "no service")
      (drover-prompt-test-with-warnings
        (with-current-buffer prompt
          (drover-prompt-test-choosing (lambda (_) "[New Fake session…]")
            (drover-prompt-submit)))
        (should (equal warnings '("No new Fake session: no service; the prompt is in the kill ring"))))
      (should (equal (with-current-buffer prompt (buffer-string)) "Do it"))
      (let ((else nil))
        (drover-test-register-simple 'later nil
                                     :engines '(fake)
                                     :create (lambda (_directory _engine _options _then fail)
                                               (setq else fail)))
        (setq drover-prompt-new-targets '((:backend later :engine fake :label "Later session")))
        (with-current-buffer prompt
          (drover-prompt-test-choosing (lambda (_) "[New Later session…]")
            (drover-prompt-submit)))
        (should-not (buffer-live-p prompt))
        (drover-prompt-test-with-warnings
          (funcall else "timed out")
          (should (equal warnings
                         '("No new Later session: timed out; the prompt is in the kill ring"))))))))

(ert-deftest drover-prompt-test-send-new-without-send ()
  "A new session whose backend takes no prompt is reported."
  (drover-prompt-test-with
    (let ((drover-prompt-new-targets '((:backend mute :engine fake :label "Mute session"))))
      (drover-test-register-simple
       'mute nil :engines '(fake)
       :create (lambda (directory _engine _options then _else)
                 (funcall then (drover-session-create :backend 'mute :id "m" :title "Mute"
                                                      :directory directory))))
      (drover-prompt-test-with-warnings
        (with-current-buffer (drover-prompt-test--prompt "Do it")
          (drover-prompt-test-choosing (lambda (_) "[New Mute session…]")
            (drover-prompt-submit)))
        (should (equal warnings '("Mute takes no prompt; it is in the kill ring")))))))

(ert-deftest drover-prompt-test-send-new-unavailable ()
  "A new session of a backend not registered, or not running the engine, is refused."
  (drover-prompt-test-with
    (let ((drover-prompt-new-targets '((:backend nowhere :engine fake :label "Nowhere session")
                                       (:backend fake :engine other :label "Other session")))
          (prompt (drover-prompt-test--prompt "Do it")))
      (dolist (name '("[New Nowhere session…]" "[New Other session…]"))
        (with-current-buffer prompt
          (drover-prompt-test-choosing (lambda (_) name)
            (should-error (drover-prompt-submit) :type 'user-error))))
      (should (equal (with-current-buffer prompt (buffer-string)) "Do it"))
      (should-not drover-test-calls))))

(provide 'drover-prompt-test)
;;; drover-prompt-test.el ends here
