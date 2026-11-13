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
The backend reports it, as a backend does a new session.  Return the
session."
  (let ((session (apply #'drover-test-session id (append properties (list :engine 'fake))))
        (buffer (generate-new-buffer (concat "session " id))))
    (setq drover-test-sessions (append drover-test-sessions (list session)))
    (drover-backend-changed 'fake)
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
                         "open-link" "provider" "schedule" "send")))
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
                                      ("P" . drover-prompt-choose-provider)
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
        (should (string-match-p "  a agent  m model  P provider  e effort\\'" header)))
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
        (should (string-suffix-p " [agent: plan] [model: opus/high] [provider: Anthropic]" info))
        (should (eq (get-text-property (string-match "opus" info) 'face info)
                    'opencode-setting-chosen))
        (should (eq (get-text-property (string-match "plan" info) 'face info)
                    'opencode-setting-inherited))
        (should (eq (get-text-property (string-match "\\[cwd: \\(.\\)" info) 'face info)
                    'minibuffer-prompt))
        (should (eq (get-text-property (match-beginning 1) 'face info) 'opencode-setting-chosen))))))

(ert-deftest drover-prompt-test-menu-shows-options ()
  "The dispatch menu shows what the prompt runs beside the keys choosing it.
A choice shows as a value, highlighted; what the engine would run
without one, dimmed, as the header dims it.  From another buffer, the
keys choose for the prompt, and so show its values; until the prompt
exists, none."
  (require 'drover-keys-test)
  (drover-prompt-test-with
    (drover-test-register-engine 'claude :resolve (lambda (defaults agent model variant)
                                                    (list :agent (or agent (plist-get defaults :agent))
                                                          :model model :variant variant)))
    (let ((drover-default-prompt-buffer-function #'drover-prompt-default-buffer))
      (cl-flet ((value (key) (drover-keys-test-value key 'drover-dispatch))
                (face (key) (get-text-property 0 'face (drover-keys-test-value key 'drover-dispatch))))
        (with-temp-buffer
          (should (equal (value "a") ""))
          (should-not (get-buffer drover-prompt-buffer-name)))
        (with-current-buffer (drover-prompt-buffer t)
          (setq default-directory "/work/project/"
                drover-prompt--last-engine 'claude
                drover-prompt--defaults '((claude :agent "plan"))
                drover-prompt-picked-model (drover-test-model "claude-opus-5-5" :name "Claude Opus 5.5"
                                                              :engines '(claude))
                drover-prompt-picked-variant 'high)
          (should (equal (value "a") "plan"))
          (should (eq (face "a") 'transient-inactive-value))
          (should (equal (value "m") "Opus 5.5"))
          (should (eq (face "m") 'transient-value))
          (should (equal (value "e") "high"))
          (should (equal (value "d") "/work/project"))
          (should (eq (face "d") 'transient-inactive-value))
          (should (member " a Agent plan" (drover-keys-test-render 'drover-dispatch)))
          (should (equal (drover--dispatch-options-description) "Prompt runs")))
        ;; Elsewhere, the prompt's, which the keys choose for; updated as
        ;; it changes.
        (with-temp-buffer
          (should (equal (value "m") "Opus 5.5"))
          (with-current-buffer drover-prompt-buffer-name
            (setq drover-prompt-picked-agent "build"))
          (should (equal (value "a") "build"))
          (should (eq (face "a") 'transient-value)))
        ;; A directory shows its end.
        (with-current-buffer drover-prompt-buffer-name
          (setq default-directory "/work/a/very/long/path/to/the/project/of/the/prompt/"
                drover-prompt-directory-picked-p t)
          (should (equal (value "d") "…/project/of/the/prompt"))
          (should (eq (face "d") 'transient-value)))))))

(ert-deftest drover-prompt-test-menu-shows-session-options ()
  "In a session's buffer, the menu shows what the session runs.
The keys choose for the session there, as in a list on a session whose
buffer is open: its agent, model, effort and directory show, the model
and effort its backend tells first, an effort by default dimmed."
  (require 'drover-keys-test)
  (drover-prompt-test-with
    (let* ((session (drover-prompt-test--session "s" :agent "build" :model "Opus 5.5" :effort "high"
                                                 :provider-name "Anthropic" :effort-default t :directory "/work/s/"))
           (buffer (drover-prompt-test--buffer-of "s")))
      (with-current-buffer buffer
        (setq-local drover-session-at-point-function (lambda () session))
        (dolist (action '(agent model provider effort directory))
          (drover-prompt-define action #'ignore)))
      (cl-flet ((value (key) (drover-keys-test-value key 'drover-dispatch))
                (face (key) (get-text-property 0 'face (drover-keys-test-value key 'drover-dispatch))))
        (with-current-buffer buffer
          (should (equal (mapcar #'value '("a" "m" "e" "d")) '("build" "Opus 5.5" "high" "/work/s")))
          (should (equal (value "P") "Anthropic"))
          (should (eq (face "a") 'transient-value))
          (should (eq (face "e") 'transient-inactive-value))
          (should (equal (drover--dispatch-options-description) "Session runs"))
          ;; What the backend tells it runs now comes first.
          (setf (alist-get 'fake drover--backends)
                (append (list :options (lambda (_)
                                         (list :model (drover-test-model "gpt-a" :name "GPT A"
                                                                         :provider-name "OpenAI" :default-variant 'medium)
                                               :variant 'xhigh)))
                        (alist-get 'fake drover--backends)))
          (should (equal (value "m") "GPT A"))
          (should (equal (value "P") "OpenAI"))
          (should (equal (value "e") "xhigh"))
          (should (eq (face "e") 'transient-value)))
        ;; From a list, the session at point's, its buffer being open.
        (with-temp-buffer
          (setq-local drover-view-p t
                      drover-session-at-point-function (lambda () session))
          (should (equal (value "a") "build"))
          (should (equal (value "P") "OpenAI"))
          (should (equal (drover--dispatch-options-description) "Session runs")))))))

(ert-deftest drover-prompt-test-menu-option-stays-open ()
  "Choosing the agent from the menu leaves it open, the new agent shown."
  (require 'drover-keys-test)
  (drover-prompt-test-with
    (save-window-excursion
      (let ((prompt (drover-prompt-buffer t)))
        (switch-to-buffer prompt)
        (drover-prompt-define 'agent (lambda ()
                                       (interactive)
                                       (setq drover-prompt-picked-agent "review")))
        (use-local-map (let ((map (make-sparse-keymap)))
                         (keymap-set map "C-'" #'drover-dispatch)
                         map))
        (unwind-protect
            (progn
              (execute-kbd-macro (kbd "C-' a"))
              (should (eq (oref transient--prefix command) 'drover-dispatch))
              (should (string-search " a Agent review"
                                     (with-current-buffer " *transient*" (buffer-string))))
              (execute-kbd-macro (kbd "C-g"))
              (should-not transient--prefix))
          (when transient--prefix
            (execute-kbd-macro (kbd "C-g")))
          (use-local-map nil))))))

(ert-deftest drover-prompt-test-menu-send-target ()
  "Send names where the prompt goes, as the functions of a hook tell."
  (require 'drover-keys-test)
  (drover-prompt-test-with
    (with-current-buffer (drover-prompt-buffer t)
      (should (member " RET Send" (drover-keys-test-render 'drover-dispatch)))
      (let ((drover-dispatch-send-target-functions
             (list (lambda () (error "Broken"))
                   (lambda () nil)
                   (lambda () (and (eq (current-buffer) (get-buffer drover-prompt-buffer-name))
                                   (concat "Fix the login\nredirect" (make-string 500000 ?x))))
                   (lambda () "Never asked"))))
        (let ((line (seq-find (lambda (line) (string-prefix-p " RET Send" line))
                              (drover-keys-test-render 'drover-dispatch))))
          (should (string-prefix-p " RET Send Fix the login redirect" line))
          (should (< (length line) 40)))))))

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
      (should (plist-get (drover-prompt--entry-options terminal terminal-options prompt) :failure))
      (should (equal (drover-prompt--entry-options new nil prompt)
                     (list :model fable :model-explicit-p t :variant 'low :variant-explicit-p t)))
      ;; A destination cannot silently drop explicit choices.
      (should (plist-get (drover-prompt--entry-options shell shell-options prompt) :failure))
      ;; A session takes a lone variant.
      (with-current-buffer prompt
        (setq drover-prompt-picked-model nil))
      (should (equal (drover-prompt--entry-options shell shell-options prompt)
                     (list :model gpt :model-explicit-p nil :variant 'low :variant-explicit-p t)))
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

(ert-deftest drover-prompt-test-resolve-models-when-chosen ()
  "A destination's models are read only when a model, provider or effort is chosen.
What is resolved without a choice is what the models would resolve."
  (drover-prompt-test-with
    (let* ((opus (drover-test-model "claude-opus-5-5" :provider-name "Anthropic" :engines '(claude)
                                    :variants '((high) (low))))
           (fable (drover-test-model "claude-fable-5-1" :provider-name "Anthropic" :engines '(claude)))
           (current (list :model opus :variant 'high))
           (terminal (drover-test-session "t" :engine 'claude))
           (prompt (drover-prompt-buffer t))
           (settable (and (drover-supports-p terminal :apply-options)
                          (if (drover-supports-p terminal :settable)
                              (drover-backend-call terminal :settable)
                            '(:agent :model :variant))))
           (asked 0))
      (drover-test-register-engine 'claude)
      (drover-register-model-source 'claude (lambda (_directory) (cl-incf asked) (list opus fable)))
      (dolist (options (list (drover-prompt-explicit-options prompt)
                             (list :agent "plan" :model nil :variant nil)
                             (list :agent nil :model nil :variant nil :selection t)))
        (should (equal (drover-prompt--resolve terminal current options prompt)
                       (drover-selection-resolve 'claude options current (list opus fable) settable))))
      (should (= asked 0))
      (dolist (options (list (list :agent nil :model fable :variant nil)
                             (list :agent nil :model nil :variant 'low)
                             (list :agent nil :model nil :variant nil :provider "anthropic")))
        (setq asked 0)
        (drover-prompt--resolve terminal current options prompt)
        (should (= asked 1))))))

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
  "Model choices preserve effort and only offer models supporting that effort."
  (drover-prompt-test-with
    (let ((high (drover-test-model "high" :variants '((low) (high)) :default-variant 'high))
          (bare (drover-test-model "bare")))
      (drover-test-register-engine 'fake)
      (drover-register-model-source 'fake (lambda (_directory) (list high bare)))
      (with-current-buffer (drover-prompt-buffer t)
        (setq drover-prompt-picked-variant 'low)
        (drover-prompt-test-choosing (lambda (names)
                                       (should (equal names '("Default" "high")))
                                       "high")
          (drover-prompt-choose-model))
        (should (equal (drover-model-id drover-prompt-picked-model) (drover-model-id high)))
        (should (eq drover-prompt-picked-variant 'low))
        (setq drover-prompt-picked-model bare drover-prompt-picked-variant nil)
        (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) (ert-fail "No variant"))))
          (drover-prompt-test-with-messages
            (drover-prompt-choose-effort)
            (setq drover-prompt-picked-model nil)
            (drover-prompt-choose-effort)
            (should (equal messages '("Choose a model first" "No variants for bare")))))
        (setq drover-prompt-picked-model high)
        (drover-prompt-test-choosing (lambda (names)
                                       (should (equal names '("Default" "low" "high")))
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
        (should (equal (drover-model-id drover-prompt-picked-model) (drover-model-id bare)))
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
          (should (equal default-directory (expand-file-name "/")))
          (should (string-match-p "\\`From buffer .* line 3:\n#\\+begin_src emacs-lisp\n(one)\n(two)\n#\\+end_src\n\n\\'"
                                  (buffer-string)))
          (should (eobp)))))))

(ert-deftest drover-prompt-test-session-buffers-quote-lines ()
  "Outside drover's own lists, the line or the region is quoted, whatever session is at point.
A session buffer tells its session at point, and so does a list of
another front end, its session keys on: neither quotes a session."
  (drover-prompt-test-with
    (let ((session (drover-prompt-test--session "s"))
          (transient-mark-mode t)
          (quoted nil))
      (cl-letf (((symbol-function 'drover-prompt-quote-sessions)
                 (lambda (sessions) (push (mapcar #'drover-session-id sessions) quoted))))
        (dolist (list '(nil t))
          (with-temp-buffer
            (setq default-directory "/")
            (setq-local drover-session-at-point-function (lambda () session))
            (setq-local drover-selected-sessions-function (lambda () (list session)))
            (when list
              (drover-session-keys-mode))
            (insert "first line\nthe line at point\nlast line\n")
            (goto-char (point-min))
            (forward-line 1)
            (forward-char 4)
            (drover-prompt-add-context)
            (with-current-buffer (drover-prompt-buffer)
              (should (string-match-p "\\`From buffer .* line 2:\n#\\+begin_src fundamental\nthe line at point\n#\\+end_src\n\n\\'"
                                      (buffer-string)))
              (erase-buffer))
            (set-mark (point-min))
            (goto-char (line-end-position))
            (activate-mark)
            (drover-prompt-add-context)
            (with-current-buffer (drover-prompt-buffer)
              (should (string-match-p "\n#\\+begin_src fundamental\nfirst line\nthe line at point\n#\\+end_src\n\n\\'"
                                      (buffer-string)))
              (erase-buffer))))
        (should-not quoted)
        ;; In drover's own list, the session at point is quoted.
        (with-temp-buffer
          (setq-local drover-view-p t
                      drover-session-at-point-function (lambda () session))
          (insert "Session s\n")
          (drover-prompt-add-context))
        (should (equal quoted '(("s"))))))))

(ert-deftest drover-prompt-test-session-own-buffer-names-session ()
  "The line of a session's own buffer is quoted from the session, by its link, else its title.
The buffer's name, which reads as a subject, made agents title the new
conversation after the one quoted; and its line numbers are those of a
rendering the agent cannot open."
  (drover-prompt-test-with
    (require 'ol)
    (let* ((session (drover-prompt-test--session "s" :title "Quoted\nsubject"))
           (buffer (drover-prompt-test--buffer-of "s"))
           (org-link-parameters (copy-tree org-link-parameters))
           (linked nil))
      (org-link-set-parameters
       "test-session"
       :store (lambda (&optional _interactive)
                (when-let* ((linked)
                            (at-point (drover-session-at-point t)))
                  (org-link-store-props :type "test-session"
                                        :link (concat "test-session:" (drover-session-id at-point))))))
      (with-current-buffer buffer
        (setq default-directory "/")
        (setq-local drover-session-at-point-function (lambda () session))
        (insert "first line\nthe line at point\n")
        (goto-char (point-min))
        (forward-line 1)
        (cl-letf (((symbol-function 'org-store-link-functions)
                   (lambda ()
                     (list (org-link-get-parameter "test-session" :store)))))
          (drover-prompt-add-context)
          (should (equal (with-current-buffer (drover-prompt-buffer)
                           (prog1 (buffer-string) (erase-buffer)))
                         "From agent session \"Quoted subject\":\n#+begin_src fundamental\nthe line at point\n#+end_src\n\n"))
          (setq linked t)
          (drover-prompt-add-context)
          (should (equal (with-current-buffer (drover-prompt-buffer) (buffer-string))
                         "From agent session [[test-session:s][Quoted subject]]:\n#+begin_src fundamental\nthe line at point\n#+end_src\n\n")))))))

(defun drover-prompt-test--quoted-from (positions)
  "Return what the context at each of POSITIONS quotes, as (POSITION . TEXT).
TEXT is the content of the code block the prompt got, unescaped, or
the whole prompt when it holds no block.  The prompt is emptied after
each."
  (mapcar (lambda (position)
            (goto-char position)
            (drover-prompt-add-context)
            (with-current-buffer (drover-prompt-buffer)
              (prog1 (cons position
                           (if (string-match "\n#\\+begin_src [^\n]*\n\\(\\(?:.\\|\n\\)*\\)\n#\\+end_src\n\n\\'"
                                             (buffer-string))
                               (org-unescape-code-in-string (match-string 1 (buffer-string)))
                             (buffer-string)))
                (erase-buffer))))
          positions))

(defun drover-prompt-test--line-positions (line)
  "Return the positions on line number LINE that quoting is tried from.
Its start, its second and third characters, which are in and after a
prompt such as \"> \", its middle and its end."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (let ((start (pos-bol))
          (end (pos-eol)))
      (list start (min (1+ start) end) (min (+ start 2) end) (/ (+ start end) 2) end))))

(defun drover-prompt-test--quotes-whole (line text)
  "Check that the context anywhere on LINE, a line number, quotes TEXT."
  (let ((positions (drover-prompt-test--line-positions line)))
    (should (equal (drover-prompt-test--quoted-from positions)
                   (mapcar (lambda (position) (cons position text)) positions)))))

(ert-deftest drover-prompt-test-quotes-whole-line-across-fields ()
  "The line at point is quoted whole, though fields divide it.
Comint marks its prompts and output with the `field' property, which
`line-beginning-position' and `line-end-position' stop at: from the
start of a sent input's line, the quote held the prompt alone.  Here
Comint writes the output and takes the input itself, as opencode.el
writes its transcripts.  Plain, Org and terminal buffers, without
fields, quote their lines whole too."
  (drover-prompt-test-with
    (with-temp-buffer
      (setq default-directory "/")
      (comint-mode)
      (let ((process (make-pipe-process :name "drover-prompt-test" :buffer (current-buffer)
                                        :noquery t))
            (comint-input-sender #'ignore))
        (unwind-protect
            (progn
              (comint-output-filter process "Welcome\n> ")
              (goto-char (point-max))
              (insert "Please investigate: https://example.com/x")
              (comint-send-input)
              (comint-output-filter process "An answer\n> ")
              ;; The fields the quote must cross.
              (should (eq (get-text-property 9 'field) 'output))
              (should-not (get-text-property 11 'field))
              (should (eq (get-text-property (save-excursion (goto-char 9) (pos-eol)) 'field)
                          'boundary))
              (drover-prompt-test--quotes-whole 2 "> Please investigate: https://example.com/x")
              (drover-prompt-test--quotes-whole 3 "An answer"))
          (delete-process process))))
    (with-temp-buffer
      (setq default-directory "/")
      (insert "first\nthe line at point\nlast\n")
      (drover-prompt-test--quotes-whole 2 "the line at point"))
    (with-temp-buffer
      (setq default-directory "/")
      (org-mode)
      (insert "* TODO Ship it\n- an item, see [[https://example.com][there]]\n")
      (drover-prompt-test--quotes-whole 1 "* TODO Ship it")
      (drover-prompt-test--quotes-whole 2 "- an item, see [[https://example.com][there]]"))
    (dolist (mode '(vterm-mode eat-mode))
      (with-temp-buffer
        (setq default-directory "/"
              major-mode mode)
        ;; Terminals pad lines to their width.
        (insert "$ ls -la          \nfile          \n$           \n")
        (drover-prompt-test--quotes-whole 1 "$ ls -la")
        (drover-prompt-test--quotes-whole 2 "file")))))

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
                      (should (equal (buffer-local-value 'default-directory prompt) (expand-file-name "/")))
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
  "Clearing erases the text, region or not, closes the prompt's window, and keeps the buffer and picks.
The window closes as `drover-prompt-toggle' hides it; the last window
of a frame stays."
  (drover-prompt-test-with
    (let ((transient-mark-mode t)
          (prompt (drover-prompt-buffer t))
          (model (drover-test-model "m")))
      (save-window-excursion
        (delete-other-windows)
        (let ((other (selected-window))
              (window (split-window)))
          (set-window-buffer window prompt)
          (select-window window)
          (with-current-buffer prompt
            (insert "some text")
            (setq drover-prompt-picked-agent "plan"
                  drover-prompt-picked-model model
                  drover-prompt-picked-variant 'high
                  default-directory "/"
                  drover-prompt-directory-picked-p t)
            (set-mark (point-min))
            (activate-mark)
            (drover-prompt-run 'clear))
          (should-not (window-live-p window))
          (should (eq (selected-window) other))
          (should (buffer-live-p prompt))
          (with-current-buffer prompt
            (should (equal (buffer-string) ""))
            (should-not mark-active)
            (should (equal drover-prompt-picked-agent "plan"))
            (should (eq drover-prompt-picked-model model))
            (should (eq drover-prompt-picked-variant 'high))
            (should (equal default-directory "/"))
            (should drover-prompt-directory-picked-p))
          ;; Shown again, the choices still hold, the directory chosen too.
          (with-temp-buffer
            (setq default-directory temporary-file-directory)
            (drover-prompt-toggle))
          (should (equal (buffer-local-value 'default-directory prompt) "/"))
          (should (equal (buffer-local-value 'drover-prompt-picked-agent prompt) "plan"))
          ;; The last window of a frame stays.
          (delete-other-windows)
          (set-window-buffer (selected-window) prompt)
          (with-current-buffer prompt
            (insert "more text")
            (drover-prompt-run 'clear)
            (should (equal (buffer-string) "")))
          (should (eq (window-buffer (selected-window)) prompt)))))))

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
  "An incompatible model is refused rather than silently dropping the choice."
  (drover-prompt-test-with
    (let* ((runs (drover-test-model "runs"))
           (other (drover-test-model "other" :engines '(other)))
           (qualified (drover-test-model "bare" :engines '(opencode)))
           (session (drover-test-session "a" :engine 'fake)))
      (drover-prompt-test-with-warnings
        (should (equal (drover-prompt-options-for session (list :agent "plan" :model runs :variant 'high))
                       (list :agent "plan" :model runs :variant 'high)))
        (should-error (drover-prompt-options-for session (list :agent "plan" :model other :variant 'high))
                      :type 'user-error)
        (should-not warnings)
        (should-error (drover-prompt-options-for session (list :model other :variant 'high) t)
                      :type 'user-error)
        (should-not warnings)
        ;; OpenCode takes provider-qualified models only.
        (should-error (drover-prompt-options-for (drover-test-session "o" :engine 'opencode)
                                                 (list :model qualified)) :type 'user-error)
        (setq drover-test-options (list (list "a" :model runs)))
        (should (equal (drover-prompt-options-for session (list :variant 'low))
                       (list :agent nil :model nil :variant 'low)))))))

(ert-deftest drover-prompt-test-apply-options ()
  "Only successfully applied choices reach THEN, at most once."
  (drover-prompt-test-with
    (let ((other (drover-test-model "other" :engines '(other)))
          (session (drover-test-session "a" :engine 'fake))
          (ran 0))
      (drover-prompt-test-with-warnings
        (drover-prompt-apply-options session (list :agent "plan" :model other :variant 'high)
                                     (lambda () (cl-incf ran)))
        (should (= ran 0))
        (should-not drover-test-calls)
        (should (equal (length warnings) 1))
        ;; Without a session, or without the operation, THEN runs at once.
        (drover-prompt-apply-options nil (list :agent "plan" :model other) (lambda () (cl-incf ran)))
        (should (= ran 1))
        (drover-test-register-simple 'simple nil)
        (drover-prompt-apply-options (drover-session-create :backend 'simple :id "s") '(:agent "plan")
                                     (lambda () (cl-incf ran)))
        (should (= ran 1))
        ;; A backend failing after calling THEN does not call it twice.
        (drover-test-register-simple 'broken nil
                                     :apply-options (lambda (_session _options then &optional _else)
                                                      (funcall then)
                                                      (error "Broken")))
        (setq warnings nil)
        (drover-prompt-apply-options (drover-session-create :backend 'broken :id "b" :title "B")
                                     '(:agent "plan") (lambda () (cl-incf ran)))
        (should (= ran 2))
        (should-not warnings)))))

;;;; The target picker

(ert-deftest drover-prompt-test-target-order ()
  "Running sessions that take prompts and have a buffer, the current one first, then most recent."
  (drover-prompt-test-with
    (let ((a (drover-prompt-test--session "a"))
          (b (drover-prompt-test--session "b")))
      (drover-prompt-test--session "c")
      ;; A session without a buffer, or whose backend takes no prompt, is no target.
      (setq drover-test-sessions (append drover-test-sessions (list (drover-test-session "nobuffer"))))
      (drover-backend-changed 'fake)
      (let ((other (generate-new-buffer "simple session")))
        (push (cons "simple" other) drover-prompt-test-buffers)
        (drover-test-register-simple 'simple (list (drover-session-create :backend 'simple :id "x"))
                                     :buffer (lambda (_) other)))
      (save-window-excursion
        (switch-to-buffer (drover-prompt-test--buffer-of "c"))
        (switch-to-buffer (drover-prompt-test--buffer-of "a"))
        (with-current-buffer (drover-prompt-test--buffer-of "b")
          (drover-prompt-test-choosing (lambda (names) (cadr names))
            (should (equal (drover-prompt-read-target "To: ") a))
            (should (equal offered '("session b" "session a" "session c")))
            (should (eq (completion-metadata-get metadata 'category) 'drover-target))
            (should (eq (completion-metadata-get metadata 'display-sort-function) #'identity)))
          (drover-prompt-test-choosing (lambda (names) (car names))
            (should (equal (drover-prompt-read-target "To: " :existing-only t) b)))
          ;; Two sessions in one buffer are told apart, in either order.
          (drover-test-register-simple 'twin (list (drover-session-create :backend 'twin :id "t"))
                                       :buffer (lambda (_) (drover-prompt-test--buffer-of "b"))
                                       :send #'ignore)
          (let ((backends (mapcar (lambda (pick)
                                    (drover-prompt-test-choosing pick
                                      (prog1 (drover-session-backend (drover-prompt-read-target "To: "))
                                        (should (equal (seq-take offered 2) '("session b" "session b <2>"))))))
                                  (list #'car #'cadr))))
            (should (equal (sort backends #'string<) '(fake twin)))))))))

(ert-deftest drover-prompt-test-target-order-shown ()
  "A session comes as the buffers showing it: its own, those belonging to it, another host's too; lists aside."
  (drover-prompt-test-with
    (let ((drover-host-label-function nil)
          (a (drover-prompt-test--session "a"))
          ;; Another host's, shown by its mirror, which tells it.
          (m (drover-prompt-test--session "m" :host "core" :since 10.0))
          ;; Another host's, shown by a buffer belonging to it, as by its view.
          (v (drover-test-session "v" :host "core" :since 20.0))
          ;; Other hosts' shown nowhere: last, the latest active first.
          (o (drover-test-session "o" :host "core" :since 30.0))
          (n (drover-test-session "n" :host "core" :since 40.0))
          (view (generate-new-buffer "view of v"))
          (review (generate-new-buffer "review of a"))
          (listing (generate-new-buffer "list")))
      (drover-prompt-test--session "b")
      (setq drover-test-sessions (append drover-test-sessions (list v o n)))
      (drover-backend-changed 'fake)
      (push (cons "view of v" view) drover-prompt-test-buffers)
      (push (cons "review of a" review) drover-prompt-test-buffers)
      (push (cons "list" listing) drover-prompt-test-buffers)
      (with-current-buffer (drover-prompt-test--buffer-of "m")
        (setq-local drover-session-at-point-function (lambda () m)))
      (with-current-buffer view
        (setq-local drover-session-at-point-function (lambda () v)))
      (with-current-buffer review
        (setq-local drover-session-at-point-function (lambda () a)))
      ;; A list's session at point is not the list's: it ranks none.
      (with-current-buffer listing
        (setq-local drover-view-p t)
        (setq-local drover-session-at-point-function (lambda () n)))
      (cl-flet ((order ()
                  (mapcar (lambda (entry) (drover-session-id (car entry)))
                          (drover-prompt--target-sessions))))
        (save-window-excursion
          (dolist (buffer (list (drover-prompt-test--buffer-of "a") (drover-prompt-test--buffer-of "b")
                                view (drover-prompt-test--buffer-of "m") review listing))
            (switch-to-buffer buffer))
          (with-temp-buffer
            ;; a by its review, used after its own buffer.
            (should (equal (order) '("a" "m" "v" "b" "n" "o")))
            ;; Other hosts' sessions without buffers, named after their titles and hosts.
            (drover-prompt-test-choosing (lambda (names) (car names))
              (should (equal (drover-prompt-read-target "To: ") a))
              (should (equal offered '("session a" "Session m @core" "Session v @core" "session b"
                                       "Session n @core" "Session o @core"))))
            ;; Those shown here are offered first, however active the others.
            (let ((drover-prompt-remote-limit 2))
              (should (equal (order) '("a" "m" "v" "b"))))
            (let ((drover-prompt-remote-limit 1))
              (should (equal (order) '("a" "m" "b"))))
            (let ((drover-prompt-remote-limit 0))
              (should (equal (order) '("a" "b")))))
          ;; In the mirror, its session comes first.
          (with-current-buffer (drover-prompt-test--buffer-of "m")
            (should (equal (order) '("m" "a" "v" "b" "n" "o")))))))))

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
        (should (equal offered '("[New Other session…]")))
        (should-error (drover-prompt-read-target "To: " :existing-only t) :type 'user-error)
        (should-not offered)))))

(ert-deftest drover-prompt-test-other-hosts ()
  "Other hosts' new sessions follow this host's; a host chosen with the directory shows its agents, models and defaults, and starts there."
  (drover-prompt-test-with
    (let* ((drover-prompt-new-targets '((:backend fake :engine fake :feature drover :label "Fake session")))
           (fast (drover-test-model "fast" :name "Fast"))
           (deep (drover-test-model "deep" :name "Deep"))
           (local (drover-test-model "local" :name "Local"))
           (started nil)
           (opened nil)
           (drover-prompt-remote-functions
            (list :targets (lambda (&optional _wait)
                             (list (list :host "core" :host-label "Core" :backend 'fake :engine 'fake
                                         :label "Fake session")))
                  :hosts (lambda () '(("core" . "Core")))
                  :label (lambda (_host) "Core")
                  :catalog (lambda (_host _local directory &optional _wait)
                             (and directory
                                  (list :engines (list (list 'fake :agents '("build" "plan") :models (list fast deep)
                                                             :default (list :agent "build" :model fast :variant 'low))))))
                  :default-directory (lambda (_host _local &optional _wait) "C:/Users/val/work/")
                  :read-directory (lambda (_host _prompt default) default)
                  :start (lambda (host target directory text options _later)
                           (setq started (list host (plist-get target :engine) directory text
                                               (plist-get options :model) (plist-get options :agent)))
                           (list :key '(fake . "n1") :title "Started"))
                  :open (lambda (host result hidden)
                          (setq opened (list host (plist-get result :key) hidden))
                          (drover-test-session "n1")))))
      (drover-prompt-test-choosing (lambda (names) (car (last names)))
        (should (equal (plist-get (drover-prompt-read-target "To: ") :host) "core"))
        (should (equal offered '("[New Fake session…]" "[New Fake session on Core…]"))))
      (with-current-buffer (drover-prompt-buffer t)
        (insert "Do it there")
        ;; A model chosen here that host does not offer hides its new sessions.
        (setq drover-prompt-picked-model local)
        (drover-prompt-test-choosing (lambda (names) (car names))
          (drover-prompt-read-target "To: ")
          (should (equal offered '("[New Fake session…]"))))
        (setq drover-prompt-picked-model nil)
        ;; The directory under a prefix argument: the host, then one of its.
        (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Core")))
          (drover-prompt-choose-directory t))
        (should (equal drover-prompt-host "core"))
        (should (equal drover-prompt-host-directory "C:/Users/val/work/"))
        (should (string-search "Core: C:/Users/val/work/" (drover-prompt--header-line)))
        (should (equal (plist-get (drover-prompt-effective-options) :agent) "build"))
        (should (eq (plist-get (drover-prompt-effective-options) :model) fast))
        (should (eq (plist-get (drover-prompt-effective-options) :variant) 'low))
        ;; Its models and agents offered, not this host's.
        (drover-prompt-test-choosing (lambda (names) (seq-find (lambda (name) (string-search "Deep" name)) names))
          (drover-prompt-choose-model)
          (should (equal offered '("Default (Fast)" "Fast" "Deep"))))
        (should (equal (drover-model-id drover-prompt-picked-model) (drover-model-id deep)))
        ;; Two agents: the other one, without asking.
        (drover-prompt-choose-agent)
        (should (equal drover-prompt-picked-agent "plan"))
        ;; Sent there: started, the prompt taken, the session opened.
        (drover-prompt-test-choosing (lambda (names) (car (last names)))
          (drover-prompt-submit)))
      (should (equal started (list "core" 'fake "C:/Users/val/work/" "Do it there" deep "plan")))
      (should (equal opened '("core" (fake . "n1") nil)))
      (should-not (get-buffer drover-prompt-buffer-name))
      ;; Started in no window here: not opened.
      (let ((drover-prompt-display-new nil))
        (with-current-buffer (drover-prompt-buffer t)
          (insert "Quietly")
          (drover-prompt-test-choosing (lambda (names) (car (last names)))
            (drover-prompt-submit))))
      (should (equal (nth 3 started) "Quietly"))
      (should (equal opened '("core" (fake . "n1") t)))
      ;; Not started there: the prompt stays.
      (with-current-buffer (drover-prompt-buffer t)
        (insert "Again")
        (setq drover-prompt-host "core" drover-prompt-host-directory "C:/Users/val/work/"))
      (setq drover-prompt-remote-functions
            (plist-put (copy-sequence drover-prompt-remote-functions) :start
                       (lambda (&rest _) (user-error "No new Fake session on Core: refused"))))
      (drover-prompt-test-choosing (lambda (names) (car (last names)))
        (with-current-buffer (drover-prompt-buffer)
          (should-error (drover-prompt-submit) :type 'user-error)))
      (should (equal (with-current-buffer (drover-prompt-buffer) (buffer-string)) "Again")))))

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
        (should (equal (drover-prompt-read-target "To: ") session))
        (should (equal (drover-prompt-read-target "To: " :existing-only t) session))
        (should (equal (drover-prompt-read-target "To: " :existing-only t :options-menu t) session)))
      (should (equal (nreverse keymaps)
                     '(drover-prompt-options-menu nil drover-prompt-options-menu))))))

(defun drover-prompt-test--give-archived (fetch)
  "Give the fake backend :archived, calling FETCH with each callback."
  (apply #'drover-register-backend 'fake :archived fetch (alist-get 'fake drover--backends)))

(defmacro drover-prompt-test-picking (answer &rest body)
  "Run BODY with `completing-read' as a minibuffer would read, answering ANSWER.
Each read runs the setup hook in a buffer of its own, standing for the
minibuffer, and ANSWER, a function of no argument, there, which returns
the name chosen; the exit hook runs then.  In ANSWER, `(offered)'
returns the names offered, those the predicate keeps, in their order,
and `picker-metadata' the completion metadata."
  (declare (indent 1) (debug t))
  `(let ((picker-collection nil)
         (picker-predicate nil))
     (cl-flet ((offered () (all-completions "" picker-collection picker-predicate))
               (picker-metadata () (completion-metadata "" picker-collection picker-predicate)))
       (ignore #'offered #'picker-metadata)
       (cl-letf (((symbol-function 'completing-read)
                  (lambda (_prompt collection &optional predicate &rest _)
                    (setq picker-collection collection
                          picker-predicate predicate)
                    (with-temp-buffer
                      (use-local-map (make-sparse-keymap))
                      (run-hooks 'minibuffer-setup-hook)
                      (prog1 (funcall ,answer)
                        (run-hooks 'minibuffer-exit-hook))))))
         ,@body))))

(ert-deftest drover-prompt-test-picker-archived ()
  "C-z offers the archived sessions taking prompts, last, and hides them again; a new picker hides them."
  (drover-prompt-test-with
    (let ((drover-prompt-new-targets '((:backend fake :engine fake :feature drover :label "Fake")))
          (opencode-archived-icon "A")
          (fetched 0)
          (rounds nil)
          (status nil))
      (drover-prompt-test--session "a")
      (drover-prompt-test--give-archived
       (lambda (callback)
         (cl-incf fetched)
         (funcall callback (list (drover-test-session "older" :archived 50.0)
                                 (drover-test-session "newer" :archived 100.0)
                                 ;; Subagents are not offered.
                                 (drover-test-session "kid" :archived 100.0 :parent "newer")
                                 ;; Nor is a session offered already.
                                 (drover-test-session "a" :archived 10.0)))))
      (drover-prompt-test-picking
          (lambda ()
            (should (eq (key-binding (kbd "C-z")) #'drover-prompt-toggle-archived))
            (push (offered) rounds)
            (drover-prompt-toggle-archived)
            (push (offered) rounds)
            (push (overlay-get drover-prompt--minibuffer-overlay 'after-string) status)
            (let ((annotate (completion-metadata-get (picker-metadata) 'annotation-function)))
              (should (string-match-p " A \\[Fake\\]" (substring-no-properties
                                                       (funcall annotate "Session newer")))))
            (should (memq 'drover-dim (ensure-list (get-text-property 0 'face (car (last (offered)))))))
            (drover-prompt-toggle-archived)
            (push (offered) rounds)
            (push (and drover-prompt--minibuffer-overlay
                       (overlay-get drover-prompt--minibuffer-overlay 'after-string))
                  status)
            (drover-prompt-toggle-archived)
            "Session newer")
        (should (equal (drover-session-id (drover-prompt-read-target "To: " :archived t)) "newer"))
        (should (equal (nreverse rounds)
                       '(("session a" "[New Fake…]")
                         ("session a" "[New Fake…]" "Session newer" "Session older")
                         ("session a" "[New Fake…]"))))
        (should (equal (mapcar (lambda (text) (and text (substring-no-properties text))) (nreverse status))
                       '(" [archived: shown]" nil)))
        ;; Fetched once: kept, and fresh enough when shown again.
        (should (= fetched 1))
        ;; Gone with its minibuffer, nothing follows the sessions changing.
        (should-not drover-prompt--archived-pickers)
        (should-not (memq #'drover-prompt--archived-changed drover-changed-hook)))
      ;; A new picker hides them.
      (setq rounds nil)
      (drover-prompt-test-picking
          (lambda () (push (offered) rounds) "session a")
        (drover-prompt-read-target "To: " :archived t)
        (should (equal rounds '(("session a" "[New Fake…]")))))
      ;; A picker not asked to offers none, and has no key for them.
      (drover-prompt-test-picking
          (lambda ()
            (should-not (eq (key-binding (kbd "C-z")) #'drover-prompt-toggle-archived))
            (should-error (drover-prompt-toggle-archived) :type 'user-error)
            "session a")
        (drover-prompt-read-target "To: ")))))

(ert-deftest drover-prompt-test-picker-archived-arriving ()
  "Archived sessions arriving while the picker shows them join it; the minibuffer says they are fetched."
  (drover-prompt-test-with
    (let ((callbacks nil)
          (rounds nil)
          (status nil))
      (drover-prompt-test--session "a")
      (drover-prompt-test--give-archived (lambda (callback) (push callback callbacks)))
      (drover-prompt-test-picking
          (lambda ()
            (drover-prompt-toggle-archived)
            (push (offered) rounds)
            (push (substring-no-properties (overlay-get drover-prompt--minibuffer-overlay 'after-string)) status)
            (should (memq #'drover-prompt--archived-changed drover-changed-hook))
            (funcall (car callbacks) (list (drover-test-session "gone" :archived 5.0)))
            ;; As `drover-changed-hook' runs.
            (drover-prompt--archived-changed)
            (push (offered) rounds)
            (push (substring-no-properties (overlay-get drover-prompt--minibuffer-overlay 'after-string)) status)
            "Session gone")
        (should (equal (drover-session-id (drover-prompt-read-target "To: " :archived t)) "gone"))
        (should (equal (nreverse rounds) '(("session a") ("session a" "Session gone"))))
        (should (equal (nreverse status) '(" [archived: fetching…]" " [archived: shown]")))))))

(ert-deftest drover-prompt-test-send-to-archived ()
  "Sent to an archived session without a buffer, the prompt goes through its backend, which opens it."
  (drover-prompt-test-with
    (drover-prompt-test--give-archived
     (lambda (callback) (funcall callback (list (drover-test-session "gone" :archived 5.0)))))
    (drover-prompt-test--session "a")
    (with-current-buffer (drover-prompt-buffer t)
      (insert "Go on")
      (drover-prompt-test-picking
          (lambda () (drover-prompt-toggle-archived) "Session gone")
        (drover-prompt-submit)))
    (should (member '(:send "gone" "Go on" fake-token) drover-test-calls))
    (should (string-empty-p (with-current-buffer (drover-prompt-buffer t) (buffer-string))))))

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

(ert-deftest drover-prompt-test-build-and-go-waits-for-agent ()
  "Build checks its target agent, waits for the switch and sends once, preserving drafts.
Once sent, the session has its turn (see `drover-handed-off-functions')."
  (require 'drover-keys)
  ;; The sends of the tests before tell their hooks once they ran: not this one's.
  (accept-process-output nil 0.05)
  (drover-prompt-test-with
    (drover-test-register-engine 'opencode :build-agent "build")
    (let* ((session (drover-prompt-test--session "a" :engine 'opencode :agent "plan"))
           (prompt (drover-prompt-test--prompt "unfinished draft"))
           (pending nil)
           (handed nil)
           (drover-handed-off-functions
            (list (lambda (session buffers how origin)
                    (push (list (drover-session-id session) (mapcar #'buffer-name buffers) how origin)
                          handed)))))
      (with-current-buffer prompt
        (setq drover-prompt-picked-agent "plan"))
      (with-current-buffer (drover-prompt-test--buffer-of "a")
        (insert "session draft")
        (setq-local drover-session-at-point-function (lambda () session)))
      (setf (plist-get (alist-get 'fake drover--backends) :apply-options)
            (lambda (target options then &optional _else)
              (push (list :apply-options (drover-session-id target) options) drover-test-calls)
              (setq pending then)))
      (with-current-buffer (drover-prompt-test--buffer-of "a")
        (call-interactively #'drover-build-and-go))
      (should (equal (reverse drover-test-calls)
                     '((:check "a" "Go ahead" (:agent "build" :model nil :variant nil))
                       (:apply-options "a" (:agent "build" :model nil :variant nil)))))
      (should pending)
      (accept-process-output nil 0.05)
      (should-not handed)
      (funcall pending)
      (funcall pending)
      (should (equal (seq-filter (lambda (call) (eq (car call) :send)) drover-test-calls)
                     '((:send "a" "Go ahead" fake-token))))
      (accept-process-output nil 0.05)
      (should (equal handed '(("a" ("session a") build drover))))
      (with-current-buffer prompt
        (should (equal (buffer-string) "unfinished draft"))
        (should (equal drover-prompt-picked-agent "plan")))
      (with-current-buffer (drover-prompt-test--buffer-of "a")
        (should (equal (buffer-string) "session draft"))))))

(ert-deftest drover-prompt-test-build-and-go-cancelled ()
  "A cancelled send neither switches the agent nor sends Go ahead."
  (require 'drover-keys)
  (drover-prompt-test-with
    (drover-test-register-engine 'opencode :build-agent "build")
    (let ((session (drover-prompt-test--session "a" :engine 'opencode :agent "plan")))
      (setq drover-test-check-result 'cancel)
      (should-not (drover-build-and-go session))
      (should (equal drover-test-calls
                     '((:check "a" "Go ahead" (:agent "build" :model nil :variant nil))))))))

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
  "The whole prompt goes with its explicit options, closing its window."
  (drover-prompt-test-with
    (let ((prompt (drover-prompt-test--prompt "  all of it\n"))
          (other (drover-test-model "other")))
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
            (should-not warnings))
          (should-not (buffer-live-p prompt))
          (should-not (window-live-p window))))
      (should (equal (car kill-ring) "all of it"))
      (should (equal (reverse drover-test-calls)
                     `((:check "a" "all of it" (:agent "plan" :model ,other :variant high))
                       (:apply-options "a" (:agent "plan" :model ,other :variant high))
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

(ert-deftest drover-prompt-test-send-discarded ()
  "A check discarding the send sends nothing and removes the prompt's text, kept in the kill ring."
  (drover-prompt-test-with
    (let ((transient-mark-mode t)
          (prompt (drover-prompt-test--prompt "one\ntwo")))
      (drover-prompt-test--session "a")
      (setq drover-test-check-result 'discard)
      (with-current-buffer prompt
        (goto-char (point-min))
        (set-mark (point))
        (end-of-line)
        (activate-mark)
        (drover-prompt-test-with-messages
          (drover-prompt-test-choosing (lambda (_) "session a")
            (drover-prompt-submit))
          (should (equal messages '("Discarded; the prompt is in the kill ring"))))
        (should (equal (buffer-string) "\ntwo")))
      (should (equal (car kill-ring) "one"))
      (should (equal drover-test-calls '((:check "a" "one" (:agent nil :model nil :variant nil)))))
      ;; The whole prompt discarded closes it.
      (with-current-buffer prompt
        (drover-prompt-test-with-messages
          (drover-prompt-test-choosing (lambda (_) "session a")
            (drover-prompt-submit))))
      (should-not (buffer-live-p prompt))
      (should (equal (car kill-ring) "two"))
      (should-not (assq :send drover-test-calls))
      ;; Text of the caller's making is only not sent.
      (drover-prompt-test-with-messages
        (should-not (drover-prompt-send-to (drover-find 'fake "a") "review")))
      (should-not (assq :send drover-test-calls)))))

(ert-deftest drover-prompt-test-send-waits ()
  "A prompt sent to wait on a request the session awaits goes, without handing it its turn."
  ;; The sends of the tests before tell their hooks once they ran: not this one's.
  (accept-process-output nil 0.05)
  (require 'drover-keys)
  (drover-prompt-test-with
    (drover-test-register-engine 'opencode :build-agent "build")
    (let* ((handed nil)
           (drover-handed-off-functions
            (list (lambda (session _buffers how _origin) (push (list (drover-session-id session) how) handed))))
           (session (drover-prompt-test--session "a" :engine 'opencode :agent "plan"))
           (prompt (drover-prompt-test--prompt "wait for it")))
      (setq drover-test-check-result 'waiting-token)
      (with-current-buffer prompt
        (drover-prompt-test-choosing (lambda (_) "session a")
          (drover-prompt-submit)))
      (should-not (buffer-live-p prompt))
      (should (member '(:send "a" "wait for it" waiting-token) drover-test-calls))
      (drover-build-and-go session)
      (should (member '(:send "a" "Go ahead" waiting-token) drover-test-calls))
      (accept-process-output nil 0.05)
      (should-not handed)
      ;; A prompt that does not wait hands it its turn.
      (setq drover-test-check-result 'fake-token)
      (with-current-buffer (drover-prompt-test--prompt "now")
        (drover-prompt-test-choosing (lambda (_) "session a")
          (drover-prompt-submit)))
      (accept-process-output nil 0.05)
      (should (equal handed '(("a" prompt))))
      (drover-build-and-go session)
      (accept-process-output nil 0.05)
      (should (equal handed '(("a" build) ("a" prompt)))))))

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
                    :apply-options (lambda (_session _options then &optional _else) (setq pending then))
                    :send (lambda (&rest _) (ert-fail "Sent to a dead session")))))
      (with-current-buffer (drover-prompt-test--prompt "text")
        (drover-prompt-test-choosing (lambda (_) "Slow")
          (drover-prompt-submit)))
      (should (drover-prompt-buffer))
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
    (should (drover-prompt-buffer))))

(ert-deftest drover-prompt-test-handed-off ()
  "The prompt's send hands the session its turn, told once its command ended.
For a running session and a new one, through `drover-handed-off-functions';
a failed send, and the other sends, tell nothing."
  ;; The sends of the tests before tell their hooks once they ran: not this one's.
  (accept-process-output nil 0.05)
  (drover-prompt-test-with
    (let* ((sent nil)
           (drover-handed-off-functions
            (list (lambda (session buffers how origin)
                    (push (list (drover-session-id session) (mapcar #'buffer-name buffers) how origin)
                          sent))))
           (drover-prompt-new-targets '((:backend fake :engine fake :feature drover :label "Fake session"))))
      (drover-prompt-test--register 'shown "Shown" :send #'ignore)
      (with-current-buffer (drover-prompt-test--prompt "text")
        (drover-prompt-test-choosing (lambda (_) "Shown")
          (drover-prompt-submit)))
      ;; Once the command ended, its windows settled.
      (should-not sent)
      (accept-process-output nil 0.05)
      (should (equal sent '(("Shown" ("Shown") prompt drover))))
      (setq sent nil)
      (let ((prompt (drover-prompt-test--prompt "Do it")))
        (with-current-buffer prompt
          (setq default-directory "/")
          (drover-prompt-test-choosing (lambda (_) "[New Fake session…]")
            (drover-prompt-submit))))
      (accept-process-output nil 0.05)
      (should (equal sent '(("new-0" nil start drover))))
      (setq sent nil)
      (drover-prompt-test--register 'broken "Broken" :send (lambda (&rest _) (error "Gone")))
      (drover-prompt-test-with-warnings
        (with-current-buffer (drover-prompt-test--prompt "text")
          (drover-prompt-test-choosing (lambda (_) "Broken")
            (drover-prompt-submit))))
      (drover-prompt-send-to (drover-find 'shown "Shown") "again")
      (accept-process-output nil 0.05)
      (should-not sent))))

(ert-deftest drover-prompt-test-display-new ()
  "A session the prompt starts shows in no window when `drover-prompt-display-new' is nil.
It starts hidden, and takes its prompt in no window when started later,
as an OpenCode session is; the windows stay as they were.  Shown by
default."
  ;; The sends of the tests before tell their hooks once they ran: not this one's.
  (accept-process-output nil 0.05)
  (drover-prompt-test-with
    (let* ((drover-prompt-new-targets '((:backend fake :engine fake :feature drover :label "Fake session")))
           (drover-prompt-display-new nil)
           (other (generate-new-buffer "*drover prompt test other*"))
           (pending nil)
           (displayed nil)
           (sent nil)
           (drover-handed-off-functions
            (list (lambda (session _buffers how _origin) (push (list (drover-session-id session) how) sent)))))
      (push (cons "other" other) drover-prompt-test-buffers)
      (apply #'drover-register-backend 'fake
             :create (lambda (directory engine options then else)
                       (push (list :create (display-buffer other)) displayed)
                       (setq pending (lambda () (drover-test--create directory engine options then else))))
             :send (lambda (session text _token)
                     (push (list :send (display-buffer other)) displayed)
                     (push (list :send (drover-session-id session) text) drover-test-calls))
             (alist-get 'fake drover--backends))
      (save-window-excursion
        (delete-other-windows)
        (let ((windows (window-list)))
          (with-current-buffer (drover-prompt-test--prompt "Do it")
            (setq default-directory "/")
            (drover-prompt-test-choosing (lambda (_) "[New Fake session…]")
              (drover-prompt-submit)))
          (funcall pending)
          (accept-process-output nil 0.05)
          (should (equal displayed '((:send nil) (:create nil))))
          (should (equal (window-list) windows))
          (should-not (get-buffer-window other t))))
      (should (plist-get (nth 3 (assq :create drover-test-calls)) :hidden))
      (should (equal (assq :send drover-test-calls) '(:send "new-0" "Do it")))
      (should (equal sent '(("new-0" start))))
      (setq drover-test-calls nil)
      (let ((drover-prompt-display-new t))
        (with-current-buffer (drover-prompt-test--prompt "Shown")
          (setq default-directory "/")
          (drover-prompt-test-choosing (lambda (_) "[New Fake session…]")
            (save-window-excursion
              (drover-prompt-submit)
              (funcall pending)))))
      (should-not (plist-member (nth 3 (assq :create drover-test-calls)) :hidden)))))

(ert-deftest drover-prompt-test-send-restores-archived ()
  "A session drover archived, its buffer still open, is offered, and restored before it is sent to.
One that cannot be restored is sent nothing, the prompt kept, and the
user told why on one line."
  (drover-prompt-test-with
    (drover-test-with-state
      (let* ((session (drover-session-create :backend 'kept :id "k" :title "Kept"
                                             :engine 'claude-code :engine-id "conv-k"))
             (buffer (drover-prompt-test--register
                      'kept "Kept"
                      :send (lambda (session text _token)
                              (push (list :send (drover-session-id session) text
                                          (drover-session-archived-p session))
                                    drover-test-calls)))))
        ;; The backend serves this record, with its engine.
        (drover-test-register-simple 'kept (list session) :buffer (lambda (_) buffer)
                                     :send (drover-backend-property 'kept :send))
        (drover-archive-session session)
        (should (buffer-live-p buffer))
        (should-not (drover-sessions))
        (with-current-buffer (drover-prompt-test--prompt "Go on")
          (drover-prompt-test-choosing (lambda (names)
                                         (should (member "Kept" names))
                                         "Kept")
            (drover-prompt-submit)))
        ;; Restored first, then sent.
        (should (equal drover-test-calls '((:send "k" "Go on" nil))))
        (should-not (drover-session-archived-p session))
        (should (equal (mapcar #'drover-session-id (drover-sessions)) '("k")))
        (should-not (drover-prompt-buffer))
        ;; Archived again, and no backend would list it restored.
        (setq drover-test-calls nil)
        (drover-archive-session session)
        (cl-letf (((symbol-function 'drover--followed-p) #'ignore))
          (with-current-buffer (drover-prompt-test--prompt "Once more")
            (let ((err (drover-prompt-test-choosing (lambda (_) "Kept")
                         (should-error (drover-prompt-submit) :type 'user-error))))
              (should (equal (cadr err)
                             (concat "Could not restore Kept from the archive: no front end follows its "
                                     "conversation, and its engine keeps no record of it that drover lists"))))
            (should (equal (buffer-string) "Once more"))))
        (should-not drover-test-calls)
        (should (drover-session-archived-p session))))))

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
  "A new session refused synchronously or asynchronously keeps the prompt."
  (drover-prompt-test-with
    (let ((drover-prompt-new-targets '((:backend fake :engine fake :label "Fake session")))
          (prompt (drover-prompt-test--prompt "Do it")))
      (setq drover-test-create-error "no service")
      (drover-prompt-test-with-warnings
        (with-current-buffer prompt
          (drover-prompt-test-choosing (lambda (_) "[New Fake session…]")
            (drover-prompt-submit)))
        (should (equal warnings '("No new Fake session: no service; the prompt stays"))))
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
        (should (buffer-live-p prompt))
        (drover-prompt-test-with-warnings
          (funcall else "timed out")
          (should (equal warnings
                         '("No new Later session: timed out; the prompt stays"))))))))

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

;;;; Editing a prompt for a session

(defmacro drover-prompt-test-without-picker (&rest body)
  "Run BODY failing the test should the target picker open."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'completing-read)
              (lambda (&rest _) (ert-fail "The picker opened"))))
     ,@body))

(ert-deftest drover-prompt-test-edit-for-empty-prompt ()
  "An earlier prompt fills an empty prompt, which goes to its session without the picker."
  (drover-prompt-test-with
    (let ((session (drover-prompt-test--session "a" :directory "/tmp/a/")))
      (drover-prompt-test--session "b")
      (drover-prompt-test-with-messages
        (with-temp-buffer
          (setq default-directory "/")
          (drover-prompt-edit-for session "first line\nsecond line"))
        (should (string-match-p "\\`\\(.* \\)?sends it to Session a\\'\\|\\`it goes to Session a\\'"
                                (car messages)))
        (should-not (string-match-p "Added after" (car messages))))
      (let ((prompt (drover-prompt-buffer)))
        (with-current-buffer prompt
          (should (equal (buffer-string) "first line\nsecond line"))
          (should (= (point) (point-max)))
          (should (equal default-directory (expand-file-name "/tmp/a/")))
          (should (equal (plist-get drover-prompt--chosen-session :key) '(fake . "a")))
          (should (equal (plist-get drover-prompt--chosen-session :title) "Session a"))
          (should (eq (plist-get drover-prompt--chosen-session :session) session))
          (let ((header (drover-prompt--header-line)))
            (should (string-match-p "|  to \\[Session a\\]" header))
            (should (eq (get-text-property (string-match "Session a" header) 'face header)
                        'opencode-setting-chosen))
            (should (eq (lookup-key (get-text-property (string-match "Session a" header) 'keymap header)
                                    [header-line mouse-1])
                        #'drover-prompt-forget-target)))
          (drover-prompt-test-without-picker
            (drover-prompt-submit)))
        (should-not (buffer-live-p prompt)))
      (should (equal (reverse drover-test-calls)
                     '((:check "a" "first line\nsecond line" (:agent nil :model nil :variant nil))
                       (:apply-options "a" (:agent nil :model nil :variant nil))
                       (:send "a" "first line\nsecond line" fake-token)))))))

(ert-deftest drover-prompt-test-edit-for-shows-session-options ()
  "A prompt set to go to a running session shows what that session runs.
Not what a new session would run: the session keeps its own model, the
one its backend tells it runs now, unless a model is chosen."
  (drover-prompt-test-with
    (let* ((sonnet (drover-test-model "sonnet" :name "Sonnet" :default-variant 'medium))
           (opus (drover-test-model "opus" :name "Opus" :default-variant 'high))
           (haiku (drover-test-model "haiku" :name "Haiku"))
           (session (drover-prompt-test--session "a")))
      (drover-test-register-engine
       'fake :resolve (lambda (_defaults agent model variant)
                        (list :agent (or agent "plan") :model (or model opus)
                              :variant (or variant (drover-model-default-variant (or model opus))))))
      (setq drover-test-options `(("a" :model ,sonnet :variant low)))
      (with-current-buffer (drover-prompt-buffer t)
        (should (eq (plist-get (drover-prompt-effective-options) :model) opus)))
      (drover-prompt-edit-for session "again")
      (with-current-buffer (drover-prompt-buffer)
        (let ((options (drover-prompt-effective-options)))
          (should (eq (plist-get options :model) sonnet))
          (should (eq (plist-get options :variant) 'low))
          (should-not (plist-get options :model-explicit-p))
          (should-not (plist-get options :variant-explicit-p))
          (should-not (plist-get options :agent)))
        (should (string-match-p "model \\[Sonnet\\]" (drover-prompt--header-line)))
        (should (eq (plist-get (drover-prompt-inherited-options) :model) sonnet))
        ;; The backend is asked again: a switch made since shows.
        (setq drover-test-options `(("a" :model ,haiku :variant nil)))
        (should (eq (plist-get (drover-prompt-effective-options) :model) haiku))
        ;; A model chosen applies to the session.
        (setq drover-prompt-picked-model opus)
        (let ((options (drover-prompt-effective-options)))
          (should (eq (plist-get options :model) opus))
          (should (plist-get options :model-explicit-p)))
        ;; Forgotten, the prompt shows a new session's options again.
        (setq drover-prompt-picked-model nil
              drover-prompt--chosen-session nil)
        (should (eq (plist-get (drover-prompt-effective-options) :model) opus))
        (should (eq (plist-get (drover-prompt-inherited-options) :model) opus))))))

(ert-deftest drover-prompt-test-edit-for-keeps-draft ()
  "An earlier prompt follows a draft after a blank line, which the echo area tells."
  (drover-prompt-test-with
    (let ((session (drover-prompt-test--session "a"))
          (prompt (drover-prompt-test--prompt "my draft")))
      (with-current-buffer prompt
        (setq drover-prompt-picked-agent "plan"))
      (drover-prompt-test-with-messages
        (drover-prompt-edit-for session "again")
        (should (string-prefix-p "Added after the prompt being written; " (car messages))))
      (with-current-buffer prompt
        (should (equal (buffer-string) "my draft\n\nagain"))
        (should (equal drover-prompt-picked-agent "plan"))
        (should (equal (plist-get drover-prompt--chosen-session :key) '(fake . "a")))))))

(ert-deftest drover-prompt-test-edit-for-target-cleared ()
  "The session chosen goes once a region is sent, or the prompt cleared, or forgotten."
  (drover-prompt-test-with
    (let ((transient-mark-mode t)
          (session (drover-prompt-test--session "a")))
      (drover-prompt-test--session "b")
      (drover-prompt-edit-for session "send this\nkeep this")
      (with-current-buffer (drover-prompt-buffer)
        (goto-char (point-min))
        (set-mark (line-end-position))
        (activate-mark)
        (drover-prompt-test-without-picker
          (drover-prompt-submit))
        (should (equal (buffer-string) "\nkeep this"))
        (should-not drover-prompt--chosen-session)
        (should-not (string-match-p "|  to " (drover-prompt--header-line)))
        ;; Now the picker asks.
        (drover-prompt-test-choosing (lambda (_) "session b")
          (drover-prompt-submit)))
      (should (equal (mapcar (lambda (call) (list (car call) (nth 1 call)))
                             (seq-filter (lambda (call) (eq (car call) :send)) (reverse drover-test-calls)))
                     '((:send "a") (:send "b"))))
      ;; Clearing the prompt forgets it.
      (drover-prompt-edit-for session "text")
      (with-current-buffer (drover-prompt-buffer)
        (drover-prompt-run 'clear)
        (should-not drover-prompt--chosen-session))
      ;; So does the command, the text staying.
      (drover-prompt-edit-for session "text")
      (with-current-buffer (drover-prompt-buffer)
        (drover-prompt-forget-target)
        (should-not drover-prompt--chosen-session)
        (should (equal (buffer-string) "text"))))))

(ert-deftest drover-prompt-test-edit-for-gone-session ()
  "A session gone since it was chosen is forgotten, and the picker asks."
  (drover-prompt-test-with
    (let ((session (drover-prompt-test--session "a")))
      (drover-prompt-test--session "b")
      (drover-prompt-edit-for session "text")
      (kill-buffer (drover-prompt-test--buffer-of "a"))
      (drover-prompt-test-with-messages
        (with-current-buffer (drover-prompt-buffer)
          (drover-prompt-test-choosing (lambda (offered) (car offered))
            (drover-prompt-submit)))
        (should (member "Session a no longer takes prompts; choose where the prompt goes" messages)))
      (should (equal (seq-keep (lambda (call) (and (eq (car call) :send) (nth 1 call)))
                               drover-test-calls)
                     '("b"))))))

(ert-deftest drover-prompt-test-edit-for-read-target ()
  "The session chosen answers any reading of where the prompt goes, scheduling's included."
  (drover-prompt-test-with
    (let ((session (drover-prompt-test--session "a")))
      (drover-prompt-test--session "b")
      (drover-prompt-edit-for session "text")
      (drover-prompt-test-without-picker
        (should (equal (drover-session-key (drover-prompt-read-target "Schedule: " :existing-only t))
                       '(fake . "a")))
        (should (equal (drover-session-key (drover-prompt-read-target "Send: ")) '(fake . "a")))))))

(ert-deftest drover-prompt-test-edit-for-menu-send ()
  "Send in the menu names the session chosen, until it is forgotten."
  (require 'drover-keys-test)
  (drover-prompt-test-with
    (let ((session (drover-prompt-test--session "a")))
      (drover-prompt-edit-for session "text")
      (with-current-buffer (drover-prompt-buffer)
        (should (seq-find (lambda (line) (string-prefix-p " RET Send Session a" line))
                          (drover-keys-test-render 'drover-dispatch)))
        (drover-prompt-forget-target)
        (should (member " RET Send" (drover-keys-test-render 'drover-dispatch))))
      (with-temp-buffer
        (setq-local drover-prompt--chosen-session (list :key '(fake . "a") :title "Session a"))
        (should-not (drover-prompt--menu-send-target))))))

;;;; Sending the prompt from anywhere

(defun drover-prompt-test--send-entries ()
  "Return the entries of Send and Open the dispatch menu shows here, as drawn."
  (seq-filter (lambda (line) (string-match-p "\\` \\(M-\\)?RET \\(Send\\|Open\\)" line))
              (drover-keys-test-render 'drover-dispatch)))

(defun drover-prompt-test--sends ()
  "Return the sessions sent to, as identifiers, oldest first."
  (reverse (seq-keep (lambda (call) (and (eq (car call) :send) (nth 1 call))) drover-test-calls)))

(ert-deftest drover-prompt-test-send-from-anywhere ()
  "RET of the menu sends the prompt from any buffer, as from the prompt, unshown.
To the session chosen for it, which Send names, else to one the picker
asks for, with the options chosen in the prompt.  A prompt of blanks is
nothing to send: Send does not apply, nor RET from elsewhere."
  (require 'drover-keys-test)
  (drover-prompt-test-with
    (let ((session (drover-prompt-test--session "a")))
      (drover-prompt-test--session "b")
      (drover-prompt-edit-for session "Fix the build")
      (with-current-buffer (drover-prompt-buffer)
        (setq drover-prompt-picked-agent "plan"))
      (with-temp-buffer
        (setq default-directory "/")
        (should (equal (drover-prompt-test--send-entries) '(" RET Send Session a")))
        (should (member "RET" (drover-keys-test-apt 'drover-dispatch)))
        (drover-prompt-test-without-picker
          (call-interactively #'drover-prompt-send))
        (should (equal (drover-prompt-test--sends) '("a")))
        (should (member '(:apply-options "a" (:agent "plan" :model nil :variant nil)) drover-test-calls))
        ;; Sent, the prompt is done with: nothing more to send.
        (should-not (drover-prompt-buffer))
        (should (equal (drover-prompt-test--send-entries) '(" RET Send")))
        (should-not (member "RET" (drover-keys-test-apt 'drover-dispatch)))
        (should-error (call-interactively #'drover-prompt-send) :type 'user-error)
        ;; Blanks are nothing either.
        (with-current-buffer (drover-prompt-buffer t)
          (insert "  \n\t\n"))
        (should-not (member "RET" (drover-keys-test-apt 'drover-dispatch)))
        (should-error (call-interactively #'drover-prompt-send) :type 'user-error)
        ;; No session chosen: the picker asks, as in the prompt.
        (with-current-buffer (drover-prompt-buffer)
          (insert "Second prompt"))
        (should (equal (drover-prompt-test--send-entries) '(" RET Send")))
        (should (member "RET" (drover-keys-test-apt 'drover-dispatch)))
        (drover-prompt-test-choosing (lambda (offered)
                                       (should (member "session b" offered))
                                       "session b")
          (call-interactively #'drover-prompt-send))
        (should (equal (drover-prompt-test--sends) '("a" "b")))
        (should-not (drover-prompt-buffer))))))

(ert-deftest drover-prompt-test-send-beside-a-list ()
  "With a prompt to send, RET sends it from a buffer beside a list, and M-RET from the list.
From another buffer, the menu acting on a list in sight, RET sends; in
the list, RET opens the session at point, as there, and M-RET sends.
The menu never shows the two RET at once."
  (require 'drover-keys-test)
  (drover-prompt-test-with
    (let ((session (drover-prompt-test--session "a"))
          (list (generate-new-buffer " list")))
      (unwind-protect
          (save-window-excursion
            (with-current-buffer list
              (setq-local drover-view-p t
                          drover-session-at-point-function (lambda () session)))
            (delete-other-windows)
            (set-window-buffer (split-window-right) list)
            (with-temp-buffer
              (set-window-buffer (selected-window) (current-buffer))
              ;; Nothing to send: RET opens the list's session at point.
              (should (equal (drover-prompt-test--send-entries) '(" RET Open" " M-RET Send")))
              (drover-prompt-edit-for session "Fix the build")
              (should (equal (drover-prompt-test--send-entries) '(" RET Send Session a")))
              ;; In the list itself.
              (with-current-buffer list
                (should (equal (drover-prompt-test--send-entries) '(" RET Open" " M-RET Send Session a")))
                (should (member "M-RET" (drover-keys-test-apt 'drover-dispatch)))
                (drover-prompt-test-without-picker
                  (call-interactively #'drover-prompt-send))
                (should (equal (drover-prompt-test--sends) '("a")))
                (should (equal (drover-prompt-test--send-entries) '(" RET Open" " M-RET Send")))
                (should-not (member "M-RET" (drover-keys-test-apt 'drover-dispatch))))))
        (kill-buffer list)))))

(provide 'drover-prompt-test)
;;; drover-prompt-test.el ends here
