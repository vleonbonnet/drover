;;; drover-claude-code-ide-test.el --- Tests of the claude-code-ide backend  -*- lexical-binding: t; -*-

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

;; Terminals are buffers whose major mode is set by hand, with a pipe
;; process standing for the CLI: neither Ghostel nor vterm is loaded,
;; and no Claude Code process starts.  The functions of the terminals,
;; of claude-code-ide and of the `claude-code' engine the backend calls
;; are stubbed; timers are recorded and fired by the tests.
;;
;; Loading claude-code-ide loads the backend, which follows terminals
;; through global hooks and an advice: this file undoes that at once, and
;; every test installs the backend and uninstalls it again, so later
;; tests of other files find no trace of it.

;;; Code:

(require 'drover-test-helpers)
;; claude-code-ide first: drover loads the backend once it is loaded.
(require 'claude-code-ide)
(require 'drover-claude-code-ide)

(drover-claude-code-ide--uninstall)
(when (timerp drover--change-timer)
  (cancel-timer drover--change-timer)
  (setq drover--change-timer nil))

(defvar ghostel-program-status)
(defvar ghostel-program-status-functions)
(defvar ghostel-exit-functions)
(defvar ghostel-mode-hook)
(defvar ghostel--pid)
(defvar ghostel-buffer-name-function)
(defvar vterm-exit-functions)
(defvar vterm-mode-hook)
(defvar vterm-buffer-name-string)
(defvar vterm-copy-mode)
(defvar eat-mode-hook)
(defvar eat-terminal)

;;;; Harness

(defvar drover-claude-code-ide-test-mcp-sessions nil
  "Sessions the stubbed `claude-code-ide-mcp--active-sessions' returns.")

(defvar drover-claude-code-ide-test-buffers nil
  "Terminals made by the test running, killed after it.")

(defvar drover-claude-code-ide-test-pids nil
  "Conversations the stubbed CLI registry records, as an alist (PID . ID).")

(defvar drover-claude-code-ide-test-newest nil
  "Newest transcripts, as an alist (DIRECTORY . ID).")

(defvar drover-claude-code-ide-test-calls nil
  "Calls of stubbed functions, newest first, as (FUNCTION . ARGUMENTS).")

(defvar drover-claude-code-ide-test-changes nil
  "Calls of `drover-backend-changed', newest first.")

(defvar drover-claude-code-ide-test-warnings nil
  "Messages of `drover-warn', newest first.")

(defvar drover-claude-code-ide-test-models-changed nil
  "Engines `drover-models-changed' was told of, newest first.")

(defvar drover-claude-code-ide-test-focused nil
  "Buffers in front of the user.")

(defvar drover-claude-code-ide-test-timers nil
  "Timers started, newest first, as (DELAY REPEAT TIMER).")

(defvar drover-claude-code-ide-test-cancelled nil
  "Timers cancelled or fired once.")

(defvar drover-claude-code-ide-test-clock 1000.0
  "The time `float-time' returns, in tests that stub it.")

(defun drover-claude-code-ide-test--run-at-time (delay repeat function &rest arguments)
  "Record a timer calling FUNCTION with ARGUMENTS after DELAY seconds.
It repeats every REPEAT seconds, when non-nil."
  (let ((timer (timer-create)))
    (timer-set-function timer function arguments)
    (push (list delay repeat timer) drover-claude-code-ide-test-timers)
    timer))

(defun drover-claude-code-ide-test-pending ()
  "Return the timers neither cancelled nor fired once, oldest first."
  (seq-remove (lambda (entry) (memq (nth 2 entry) drover-claude-code-ide-test-cancelled))
              (reverse drover-claude-code-ide-test-timers)))

(defun drover-claude-code-ide-test-fire ()
  "Run every pending timer once, oldest first; a timer that does not repeat stops."
  (dolist (entry (drover-claude-code-ide-test-pending))
    (let ((timer (nth 2 entry)))
      (unless (nth 1 entry)
        (push timer drover-claude-code-ide-test-cancelled))
      (apply (timer--function timer) (timer--args timer)))))

(defun drover-claude-code-ide-test--recorder (function)
  "Return a stub of FUNCTION recording its calls."
  (lambda (&rest arguments)
    (push (cons function arguments) drover-claude-code-ide-test-calls)
    nil))

(defun drover-claude-code-ide-test-called (function)
  "Return the arguments of the calls of stubbed FUNCTION, oldest first."
  (mapcar #'cdr (reverse (seq-filter (lambda (call) (eq (car call) function))
                                     drover-claude-code-ide-test-calls))))

(defmacro drover-claude-code-ide-test-isolated (&rest body)
  "Run BODY with drover's registries and the backend's global state isolated.
Stubs stand for claude-code-ide's sessions, the CLI's records, timers,
focus, warnings and change reports, of sessions and of models.
Terminals BODY makes are killed."
  (declare (indent 0) (debug t))
  `(let ((drover--backends nil)
         (drover--model-sources drover--model-sources)
         (drover-claude-code-ide-test-models-changed nil)
         (drover--started nil)
         (drover--running nil)
         (drover--change-timer nil)
         (drover--changed-keys nil)
         (drover--changed-backends nil)
         (drover--changed-all nil)
         (drover--changed-applied nil)
         (drover--index nil)
         (drover-changed-hook nil)
         (drover--closing nil)
         (drover-show-history nil)
         (drover--history nil)
         (drover-show-archived nil)
         (drover--archived nil)
         (drover-claude-code-start-flags-function nil)
         (drover-claude-code-project-directory-function nil)
         (drover-claude-code-ide--watchers nil)
         (drover--resumers nil)
         (drover--resumer-properties nil)
         (drover-claude-code-transcripts--users nil)
         (drover-claude-code-transcripts--index nil)
         (drover-claude-code-transcripts--files (make-hash-table :test #'equal))
         (drover-codex-rollouts--users nil)
         (drover-codex-rollouts--index nil)
         (drover-codex-rollouts--files (make-hash-table :test #'equal))
         (window-selection-change-functions window-selection-change-functions)
         (window-buffer-change-functions window-buffer-change-functions)
         (after-focus-change-function after-focus-change-function)
         (after-load-functions after-load-functions)
         (claude-code-ide-cli-extra-flags "")
         (drover-claude-code-ide-test-mcp-sessions nil)
         (drover-claude-code-ide-test-buffers nil)
         (drover-claude-code-ide-test-pids nil)
         (drover-claude-code-ide-test-newest nil)
         (drover-claude-code-ide-test-calls nil)
         (drover-claude-code-ide-test-changes nil)
         (drover-claude-code-ide-test-warnings nil)
         (drover-claude-code-ide-test-focused nil)
         (drover-claude-code-ide-test-timers nil)
         (drover-claude-code-ide-test-cancelled nil))
     (cl-letf (((symbol-function 'claude-code-ide-mcp--active-sessions)
                (lambda () drover-claude-code-ide-test-mcp-sessions))
               ((symbol-function 'drover-claude-code-pid-session-id)
                (lambda (pid) (alist-get pid drover-claude-code-ide-test-pids)))
               ((symbol-function 'drover-claude-code-newest-session-id)
                (lambda (directory)
                  (alist-get directory drover-claude-code-ide-test-newest nil nil #'equal)))
               ((symbol-function 'drover-git-branch)
                (lambda (directory) (and directory "main")))
               ((symbol-function 'drover-claude-code-ide--focused-p)
                (lambda (buffer) (memq buffer drover-claude-code-ide-test-focused)))
               ((symbol-function 'drover-claude-code-ide--focused-buffers)
                (lambda () drover-claude-code-ide-test-focused))
               ((symbol-function 'drover-backend-changed)
                (lambda (&rest arguments) (push arguments drover-claude-code-ide-test-changes)))
               ;; Asking the models would read the CLI's own catalog.
               ((symbol-function 'drover-models-changed)
                (lambda (engine) (push engine drover-claude-code-ide-test-models-changed)))
               ((symbol-function 'drover-warn)
                (lambda (format-string &rest arguments)
                  (push (apply #'format-message format-string arguments)
                        drover-claude-code-ide-test-warnings)))
               ((symbol-function 'run-at-time) #'drover-claude-code-ide-test--run-at-time)
               ((symbol-function 'run-with-timer) #'drover-claude-code-ide-test--run-at-time)
               ((symbol-function 'cancel-timer)
                (lambda (timer) (push timer drover-claude-code-ide-test-cancelled))))
       (unwind-protect
           (progn ,@body)
         (dolist (buffer drover-claude-code-ide-test-buffers)
           (when (buffer-live-p buffer)
             (kill-buffer buffer)))))))

(defmacro drover-claude-code-ide-test-with (&rest body)
  "Run BODY with the backend installed.
It is isolated as in `drover-claude-code-ide-test-isolated'."
  (declare (indent 0) (debug t))
  `(drover-claude-code-ide-test-isolated
     (unwind-protect
         (progn (drover-claude-code-ide--install)
                (setq drover-claude-code-ide-test-changes nil)
                ,@body)
       (drover-claude-code-ide--uninstall))))

(defun drover-claude-code-ide-test-terminal (name &rest properties)
  "Return a new terminal buffer NAME with a live process.
PROPERTIES may give its :mode, `ghostel-mode' by default, and its
:directory, \"/work/drover/\" by default.  With :mcp-session non-nil,
claude-code-ide tracks it, with that project directory when a string."
  (let ((buffer (generate-new-buffer name))
        (mcp (plist-get properties :mcp-session)))
    (push buffer drover-claude-code-ide-test-buffers)
    (with-current-buffer buffer
      (setq major-mode (or (plist-get properties :mode) 'ghostel-mode))
      (setq default-directory (or (plist-get properties :directory) "/work/drover/"))
      (let ((process (make-pipe-process :name name :buffer buffer :noquery t)))
        (when mcp
          (let ((session (make-claude-code-ide-mcp-session
                          :session-id name :buffer buffer :process process
                          :project-dir (if (stringp mcp) mcp default-directory))))
            (setq-local claude-code-ide--session session)
            (setq drover-claude-code-ide-test-mcp-sessions
                  (append drover-claude-code-ide-test-mcp-sessions (list session)))))))
    buffer))

(defun drover-claude-code-ide-test-session (buffer)
  "Return the drover session of terminal BUFFER, as listed."
  (seq-find (lambda (session) (eq (drover-session-buffer session) buffer))
            (drover-claude-code-ide--sessions)))

(defun drover-claude-code-ide-test-status (buffer records)
  "Give terminal BUFFER the program status RECORDS, as Ghostel would."
  (with-current-buffer buffer
    (setq-local ghostel-program-status records)
    (drover-claude-code-ide--status-changed "" nil nil)))

(defun drover-claude-code-ide-test-ready (buffer)
  "Make the interface in terminal BUFFER read input."
  (with-current-buffer buffer
    (goto-char (point-max))
    (insert "────\n>" (string #xa0) "\n────\n")))

(defun drover-claude-code-ide-test-model (id &rest engines)
  "Return a model ID that ENGINES run, `claude-code' by default."
  (drover-model-create :id id :engines (or engines '(claude-code))))

;;;; Start flags

(ert-deftest drover-claude-code-ide-test-start-flags-agent ()
  "Plan and build start in their permission modes; other agents take --agent."
  (let ((drover-claude-code-agent-modes '(("plan" . "plan") ("build" . "auto"))))
    (should (equal (drover-claude-code-ide--start-flags '(:agent "plan") "") "--permission-mode plan"))
    (should (equal (drover-claude-code-ide--start-flags '(:agent "build") "") "--permission-mode auto"))
    (should (equal (drover-claude-code-ide--start-flags '(:agent "reviewer") "") "--agent reviewer"))
    (should (equal (drover-claude-code-ide--start-flags '(:agent "") "") ""))
    (should (equal (drover-claude-code-ide--start-flags nil "") ""))
    (should (equal (drover-claude-code-ide--start-flags nil nil) ""))
    (should (equal (drover-claude-code-ide--start-flags '(:agent "my agent") "")
                   (concat "--agent " (shell-quote-argument "my agent"))))))

(ert-deftest drover-claude-code-ide-test-start-flags-order ()
  "Model, name and mode come before the base flags, the effort after them."
  (let ((drover-claude-code-agent-modes '(("plan" . "plan")))
        (model (drover-claude-code-ide-test-model "opus[1m]")))
    (should (equal (drover-claude-code-ide--start-flags
                    (list :agent "plan" :title "Fix the bug" :model model :variant 'high)
                    "--verbose --effort low")
                   (concat "--model " (shell-quote-argument "opus[1m]")
                           " --name " (shell-quote-argument "Fix the bug")
                           " --permission-mode plan --verbose --effort low --effort high")))
    (should (equal (drover-claude-code-ide--start-flags (list :title "") "--verbose") "--verbose"))))

(ert-deftest drover-claude-code-ide-test-start-flags-model-and-effort ()
  "Only Claude Code models and Claude Code effort levels reach the flags."
  (let ((opencode (drover-claude-code-ide-test-model "gpt-5" 'opencode))
        (efforts (drover-model-create :id "sonnet" :engines '(claude-code)
                                      :variants '((ultra :effort "max") (fast)))))
    (should (equal (drover-claude-code-ide--start-flags (list :model opencode) "") ""))
    (should (equal (drover-claude-code-ide--start-flags (list :model efforts :variant 'ultra) "")
                   "--model sonnet --effort max"))
    (should (equal (drover-claude-code-ide--start-flags (list :model efforts :variant 'fast) "")
                   "--model sonnet"))
    ;; A variant alone names its effort, as the variant of no model.
    (should (equal (drover-claude-code-ide--start-flags (list :variant 'xhigh) "") "--effort xhigh"))))

;;;; Starting sessions

(defmacro drover-claude-code-ide-test-starting (flags &rest body)
  "Run BODY with `claude-code-ide--start-session' stubbed, the base FLAGS set.
The stub records (DIRECTORY FLAGS PREFIX) in `observed', and makes a
session of `buffer', a temporary buffer, active."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let* ((buffer (current-buffer))
            (session (make-claude-code-ide-mcp-session :buffer buffer))
            (old (make-claude-code-ide-mcp-session :buffer (current-buffer)))
            (started nil)
            (observed nil)
            (claude-code-ide-cli-extra-flags ,flags)
            (current-prefix-arg '(4)))
       (cl-letf (((symbol-function 'claude-code-ide-mcp--active-sessions)
                  (lambda () (if started (list session old) (list old))))
                 ((symbol-function 'claude-code-ide--start-session)
                  (lambda (&rest arguments)
                    (should-not arguments)
                    (setq observed (list default-directory claude-code-ide-cli-extra-flags
                                         current-prefix-arg)
                          started t))))
         ,@body))))

(ert-deftest drover-claude-code-ide-test-start-names-the-session ()
  "A new session gets an explicit --session-id, recorded on its terminal."
  (drover-claude-code-ide-test-starting "--model haiku"
    (should (eq (drover-claude-code-ide-start "/tmp/project") buffer))
    (should (equal (car observed) "/tmp/project/"))
    (should (string-match "\\`--model haiku --session-id \\([^ ]+\\)\\'" (nth 1 observed)))
    (let ((id (match-string 1 (nth 1 observed))))
      (should (string-match-p "\\`[[:xdigit:]]\\{8\\}-[[:xdigit:]]\\{4\\}-4[[:xdigit:]]\\{3\\}-[89ab][[:xdigit:]]\\{3\\}-[[:xdigit:]]\\{12\\}\\'"
                              id))
      (should (equal drover-claude-code-ide-session-id id)))
    ;; A new instance never asks for its name.
    (should-not (nth 2 observed))
    (should (equal claude-code-ide-cli-extra-flags "--model haiku"))))

(ert-deftest drover-claude-code-ide-test-start-keeps-session-flags ()
  "Flags that resume, continue or name a session get no --session-id."
  (dolist (flags '("--resume abc" "--verbose -c" "--continue" "-r" "--session-id=abc"))
    (drover-claude-code-ide-test-starting "--verbose"
      (should (eq (drover-claude-code-ide-start "/tmp/project" flags) buffer))
      (should (equal (nth 1 observed) flags))
      (should-not drover-claude-code-ide-session-id)))
  (drover-claude-code-ide-test-starting "--verbose"
    (drover-claude-code-ide-start "/tmp/project" "")
    (should (string-prefix-p "--session-id " (nth 1 observed)))))

(ert-deftest drover-claude-code-ide-test-start-without-new-session ()
  "A start that registers no session returns nil."
  (drover-claude-code-ide-test-starting ""
    (cl-letf (((symbol-function 'claude-code-ide--start-session) #'ignore))
      (should-not (drover-claude-code-ide-start "/tmp/project"))
      (should-not observed))))

(ert-deftest drover-claude-code-ide-test-resume ()
  "Resuming passes the exact identifier, quoted, and records it on the terminal."
  (drover-claude-code-ide-test-starting "--verbose"
    (let ((id "33333333-3333-4333-8333-333333333333"))
      (should (eq (drover-claude-code-ide-resume id "/tmp/project/") buffer))
      (should (equal observed (list "/tmp/project/" (concat "--verbose --resume " id) nil)))
      (should (equal drover-claude-code-ide-session-id id))))
  (drover-claude-code-ide-test-starting "--verbose"
    (drover-claude-code-ide-resume "a b" "/tmp/project/")
    (should (equal (nth 1 observed) (concat "--verbose --resume " (shell-quote-argument "a b"))))))

(ert-deftest drover-claude-code-ide-test-resume-in-its-directory ()
  "A conversation resumes in the directory it ran in, not the root of a project around it."
  (let* ((root (file-name-as-directory (make-temp-file "drover-project" t)))
         (directory (file-name-as-directory (expand-file-name "sub" root))))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" root) t)
          (make-directory directory t)
          (drover-claude-code-ide-test-starting ""
            (let ((working nil))
              (cl-letf (((symbol-function 'claude-code-ide--start-session)
                         (lambda (&rest _)
                           (setq working (claude-code-ide--get-working-directory)
                                 started t))))
                (drover-claude-code-ide-resume "33333333-3333-4333-8333-333333333333" directory))
              (should (equal working directory))
              ;; Started otherwise, it starts at the root, as claude-code-ide does.
              (cl-letf (((symbol-function 'claude-code-ide--start-session)
                         (lambda (&rest _)
                           (setq working (claude-code-ide--get-working-directory)
                                 started t))))
                (drover-claude-code-ide-start directory))
              (should (equal (file-truename working) (file-truename root))))))
      (delete-directory root t))))

(ert-deftest drover-claude-code-ide-test-fork ()
  "Forking resumes the conversation under a new identifier, given at start."
  (drover-claude-code-ide-test-starting "--verbose"
    (let ((id "33333333-3333-4333-8333-333333333333"))
      (should (eq (drover-claude-code-ide-fork id "/tmp/project/") buffer))
      (should (equal (car observed) "/tmp/project/"))
      (should (string-match (concat "\\`--verbose --resume " id " --fork-session --session-id \\([^ ]+\\)\\'")
                            (nth 1 observed)))
      (let ((fork (match-string 1 (nth 1 observed))))
        (should-not (equal fork id))
        (should (string-match-p drover-claude-code--uuid-regexp fork))
        (should (equal drover-claude-code-ide-session-id fork)))))
  ;; Through the backend, the session's own conversation and directory.
  (drover-claude-code-ide-test-with
    (let ((terminal (drover-claude-code-ide-test-terminal "*claude* fork"))
          (forked nil))
      (cl-letf (((symbol-function 'drover-claude-code-ide-fork)
                 (lambda (id directory) (push (list id directory) forked) terminal)))
        (let ((session (drover-claude-code-ide-test-session terminal)))
          ;; Its conversation not known yet, it cannot fork.
          (should-not (drover-supports-p session :fork))
          (should-error (drover-fork session) :type 'user-error)
          (with-current-buffer terminal
            (setq-local drover-claude-code-ide-session-id "conv"))
          (setq session (drover-claude-code-ide-test-session terminal))
          (should (drover-supports-p session :fork))
          (drover-fork session)
          (should (equal forked '(("conv" "/work/drover/")))))))))

(ert-deftest drover-claude-code-ide-test-compact ()
  "Compacting types /compact in the terminal, once its turn ended and it reads input."
  (drover-claude-code-ide-test-with
    (drover-claude-code-ide-test-typing
      (let ((terminal (drover-claude-code-ide-test-terminal "*claude* compact")))
        (should (drover-supports-p (drover-claude-code-ide-test-session terminal) :compact))
        ;; Not reading input yet.
        (should-error (drover-compact (drover-claude-code-ide-test-session terminal)) :type 'user-error)
        (drover-claude-code-ide-test-ready terminal)
        ;; In the middle of a turn.
        (drover-claude-code-ide-test-status terminal '(("" :state working)))
        (should-error (drover-compact (drover-claude-code-ide-test-session terminal)) :type 'user-error)
        (should-not (drover-claude-code-ide-test-called 'ghostel-send-string))
        (drover-claude-code-ide-test-status terminal '(("" :state done)))
        (drover-compact (drover-claude-code-ide-test-session terminal))
        (should (equal (drover-claude-code-ide-test-called 'ghostel-send-string) '(("/compact"))))
        (should (equal (drover-claude-code-ide-test-called 'ghostel-send-key) '(("return"))))))))

(ert-deftest drover-claude-code-ide-test-delete ()
  "Deleting closes the terminal, then deletes the conversation's files."
  (drover-test-with-homes
    (drover-claude-code-ide-test-with
      (let* ((id "44444444-4444-4444-8444-444444444444")
             (project (file-name-as-directory (file-truename (expand-file-name "project" home))))
             (transcript (drover-claude-code-transcript id project))
             (beside (expand-file-name id (file-name-directory transcript)))
             (delete-by-moving-to-trash nil)
             (terminal (drover-claude-code-ide-test-terminal "*claude* delete" :directory project)))
        (make-directory project t)
        (let ((session (drover-claude-code-ide-test-session terminal)))
          (should-not (drover-supports-p session :delete))
          (with-current-buffer terminal
            (setq-local drover-claude-code-ide-session-id id))
          (setq session (drover-claude-code-ide-test-session terminal))
          (should (drover-supports-p session :delete))
          ;; No transcript: nothing goes, the terminal stays.
          (should-error (drover-delete session) :type 'user-error)
          (should (buffer-live-p terminal))
          (drover-test-write transcript "{}\n")
          (drover-test-write (expand-file-name "tool-results/a.txt" beside) "out")
          (drover-delete session)
          (should-not (buffer-live-p terminal))
          (should-not (file-exists-p transcript))
          (should-not (file-exists-p beside)))))))

(ert-deftest drover-claude-code-ide-test-archive-closes-terminal ()
  "Archiving closes an idle terminal at once, a working one once its turn ended, asking nothing."
  (drover-test-with-homes
    (drover-claude-code-ide-test-with
      (drover-test-with-state
        (let ((idle (drover-claude-code-ide-test-terminal "*claude* idle"))
              (working (drover-claude-code-ide-test-terminal "*claude* working"))
              (messages nil))
          (pcase-dolist (`(,terminal . ,id) `((,idle . "66666666-6666-4666-8666-666666666666")
                                              (,working . "77777777-7777-4777-8777-777777777777")))
            (with-current-buffer terminal
              (setq-local drover-claude-code-ide-session-id id)
              (drover-claude-code-ide--setup-terminal)
              ;; As a terminal running its program asks before it goes.
              (add-hook 'kill-buffer-query-functions (lambda () (error "Asked")) nil t)))
          (drover-claude-code-ide-test-status working '(("" :state working)))
          (cl-letf (((symbol-function 'message)
                     (lambda (format &rest arguments)
                       (push (apply #'format-message format arguments) messages))))
            (drover-archive (drover-claude-code-ide-test-session idle))
            (should-not (buffer-live-p idle))
            (should (equal messages '("Archived idle")))
            (drover-archive (drover-claude-code-ide-test-session working))
            (should (buffer-live-p working))
            (should (equal (car messages) "Archived working; its buffer closes once its turn ends"))
            ;; Waiting for an answer, the turn goes on.
            (drover-claude-code-ide-test-status working '(("" :state blocked :kind permission :title "Bash")))
            (run-hooks 'drover-changed-hook)
            (should (buffer-live-p working))
            (drover-claude-code-ide-test-status working '(("" :state done)))
            (run-hooks 'drover-changed-hook)
            (should-not (buffer-live-p working))
            (should (equal (car messages) "Closed working, archived, now that its turn ended")))
          ;; Their terminals gone, drover keeps them among the archived sessions.
          (let ((drover-show-archived t))
            (should (= (length (drover-archived-sessions)) 2))))))))

(ert-deftest drover-claude-code-ide-test-archived-without-terminal ()
  "A session whose terminal is gone, as drover keeps one archived, offers what needs none."
  (drover-test-with-homes
    (drover-claude-code-ide-test-with
      (let* ((id "55555555-5555-4555-8555-555555555555")
             (project (file-name-as-directory (file-truename (expand-file-name "project" home))))
             (kept (drover-session-create :backend 'claude-code-ide :id id :engine 'claude-code
                                          :engine-id id :directory project :archived 1.0))
             (recap nil))
        (dolist (operation '(:open :respond :interrupt :mark-seen :preview :rename :compact))
          (should-not (drover-supports-p kept operation)))
        (dolist (operation '(:fork :delete :recap))
          (should (drover-supports-p kept operation)))
        ;; Its recap is read from its conversation.
        (drover-test-write (drover-claude-code-transcript id project)
                           (concat (json-encode '((type . "user") (message (content . "Go")))) "\n"))
        (drover-recap kept (lambda (value) (setq recap value)))
        (should (equal (plist-get recap :prompt) "Go"))
        (drover-delete kept)
        (should-not (file-exists-p (drover-claude-code-transcript id project)))))))

(ert-deftest drover-claude-code-ide-test-create ()
  "A new session starts with the flags of its options, and is handed over."
  (drover-claude-code-ide-test-with
    (let* ((claude-code-ide-cli-extra-flags "--verbose")
           (drover-claude-code-agent-modes '(("plan" . "plan")))
           (terminal (drover-claude-code-ide-test-terminal "*claude-code[drover]*" :mcp-session t))
           started created failed)
      (cl-letf (((symbol-function 'drover-claude-code-ide-start)
                 (lambda (directory flags) (push (list directory flags) started) terminal)))
        (drover-claude-code-ide--create "/work/drover/" 'claude-code
                                        (list :agent "plan" :title "Task" :defaults '(:model nil))
                                        (lambda (session) (push session created))
                                        (lambda (message) (push message failed)))
        (should (equal started '(("/work/drover/" "--name Task --permission-mode plan --verbose"))))
        (should-not failed)
        (should (= (length created) 1))
        (should (eq (drover-session-backend (car created)) 'claude-code-ide))
        (should (eq (drover-session-engine (car created)) 'claude-code))
        (should (eq (drover-session-buffer (car created)) terminal))))))

(ert-deftest drover-claude-code-ide-test-create-failures ()
  "A start that fails, finds no terminal or is for another engine goes to ELSE."
  (drover-claude-code-ide-test-with
    (let (created failed started)
      (cl-flet ((create (engine)
                  (drover-claude-code-ide--create "/work/drover/" engine nil
                                                  (lambda (session) (push session created))
                                                  (lambda (message) (push message failed)))))
        (cl-letf (((symbol-function 'drover-claude-code-ide-start)
                   (lambda (&rest _) (setq started t) nil)))
          (create 'claude-code)
          (should (equal failed '("Could not locate new Claude Code buffer")))
          (setq started nil)
          (create 'codex)
          (should-not started)
          (should (string-match-p "codex" (car failed))))
        (cl-letf (((symbol-function 'drover-claude-code-ide-start)
                   (lambda (&rest _) (error "Claude Code CLI not available"))))
          (create 'claude-code)
          (should (equal (car failed) "Claude Code CLI not available")))
        (should (= (length failed) 3))
        (should-not created)))))

(ert-deftest drover-claude-code-ide-test-new ()
  "A new session from drover's commands starts with the default flags."
  (drover-claude-code-ide-test-with
    (let (started)
      (cl-letf (((symbol-function 'drover-claude-code-ide-start)
                 (lambda (&rest arguments) (push arguments started) nil)))
        (should-error (drover-claude-code-ide--new "/work/drover/") :type 'user-error)
        (should (equal started '(("/work/drover/"))))))))

;;;; Learning the session of a new terminal

(ert-deftest drover-claude-code-ide-test-track-named-terminal ()
  "A terminal started with --session-id knows its session at once."
  (drover-claude-code-ide-test-with
    (let* ((terminal (drover-claude-code-ide-test-terminal "*claude* drover"))
           (id "44444444-4444-4444-8444-444444444444")
           (claude-code-ide-cli-extra-flags (concat "--verbose --session-id " id))
           (result (cons terminal 'process)))
      (should (eq (drover-claude-code-ide--track-terminal
                   (lambda (&rest arguments)
                     (should (equal arguments '("*claude* drover" "/work/drover/" 1 nil nil "s")))
                     result)
                   "*claude* drover" "/work/drover/" 1 nil nil "s")
                  result))
      (should (equal (buffer-local-value 'drover-claude-code-ide-session-id terminal) id))
      (should (numberp (buffer-local-value 'drover-claude-code-ide--created terminal)))
      (should-not drover-claude-code-ide-test-timers)
      (should drover-claude-code-ide-test-changes)
      ;; The CLI starting, its models are the catalog it caches.
      (should (equal drover-claude-code-ide-test-models-changed '(claude-code))))))

(ert-deftest drover-claude-code-ide-test-capture-new-transcript ()
  "An unnamed terminal learns the one transcript created after it started."
  (drover-claude-code-ide-test-with
    (let* ((projects (make-temp-file "drover-claude-projects" t))
           (bucket (expand-file-name "new-project" projects))
           (terminal (drover-claude-code-ide-test-terminal "*claude* new")))
      (unwind-protect
          (cl-letf (((symbol-function 'drover-claude-code-projects-directory) (lambda () projects))
                    ((symbol-function 'drover-claude-code-encode-directory)
                     (lambda (directory)
                       (should (equal directory "/work/new/"))
                       "new-project")))
            (drover-claude-code-ide--track-terminal
             (lambda (&rest _) (cons terminal 'process)) "*claude* new" "/work/new/")
            (should (equal (mapcar (lambda (entry) (list (car entry) (cadr entry)))
                                   (drover-claude-code-ide-test-pending))
                           '((2 2))))
            (should (= (length drover-claude-code-ide--watchers) 1))
            ;; The project's first session creates its directory of transcripts.
            (drover-claude-code-ide-test-fire)
            (should-not (buffer-local-value 'drover-claude-code-ide-session-id terminal))
            (make-directory bucket)
            (drover-test-write (expand-file-name "fixture-session.jsonl" bucket) "{}\n")
            (setq drover-claude-code-ide-test-changes nil
                  drover-claude-code-ide-test-models-changed nil)
            (drover-claude-code-ide-test-fire)
            (should (equal (buffer-local-value 'drover-claude-code-ide-session-id terminal)
                           "fixture-session"))
            (should drover-claude-code-ide-test-changes)
            (should (equal drover-claude-code-ide-test-models-changed '(claude-code)))
            (should-not (drover-claude-code-ide-test-pending))
            (should-not drover-claude-code-ide--watchers))
        (delete-directory projects t)))))

(ert-deftest drover-claude-code-ide-test-capture-never-guesses ()
  "Two new transcripts name no session; the record of the process does."
  (drover-claude-code-ide-test-with
    (let* ((projects (make-temp-file "drover-claude-projects" t))
           (bucket (expand-file-name "project" projects))
           (terminal (drover-claude-code-ide-test-terminal "*claude* project")))
      (unwind-protect
          (cl-letf (((symbol-function 'drover-claude-code-projects-directory) (lambda () projects))
                    ((symbol-function 'drover-claude-code-encode-directory) (lambda (_) "project")))
            (drover-test-write (expand-file-name "old.jsonl" bucket) "{}\n")
            (drover-claude-code-ide--capture-session-id terminal "/work/project/")
            (drover-test-write (expand-file-name "one.jsonl" bucket) "{}\n")
            (drover-test-write (expand-file-name "two.jsonl" bucket) "{}\n")
            (drover-claude-code-ide-test-fire)
            (should-not (buffer-local-value 'drover-claude-code-ide-session-id terminal))
            (should (drover-claude-code-ide-test-pending))
            (with-current-buffer terminal
              (setq-local ghostel--pid 4242))
            (push '(4242 . "two") drover-claude-code-ide-test-pids)
            (drover-claude-code-ide-test-fire)
            (should (equal (buffer-local-value 'drover-claude-code-ide-session-id terminal) "two"))
            (should-not (drover-claude-code-ide-test-pending)))
        (delete-directory projects t)))))

(ert-deftest drover-claude-code-ide-test-capture-stops ()
  "The search stops when the terminal exits or dies, or past its deadline."
  (drover-claude-code-ide-test-with
    (let ((terminal (drover-claude-code-ide-test-terminal "*claude* stop")))
      (should (drover-claude-code-ide--capture-step terminal "/nowhere/" nil (- (float-time) 1)))
      (should-not (drover-claude-code-ide--capture-step terminal "/nowhere/" nil (+ (float-time) 60)))
      (delete-process (get-buffer-process terminal))
      (should (drover-claude-code-ide--capture-step terminal "/nowhere/" nil (+ (float-time) 60)))
      (kill-buffer terminal)
      (should (drover-claude-code-ide--capture-step terminal "/nowhere/" nil (+ (float-time) 60))))))

(ert-deftest drover-claude-code-ide-test-track-survives-errors ()
  "A failure to follow a new terminal is reported; the terminal is returned."
  (drover-claude-code-ide-test-with
    (let ((terminal (drover-claude-code-ide-test-terminal "*claude* error")))
      (cl-letf (((symbol-function 'drover-claude-code-encode-directory)
                 (lambda (_) (error "Not implemented"))))
        (should (equal (drover-claude-code-ide--track-terminal
                        (lambda (&rest _) (cons terminal 'process)) "*claude* error" "/work/")
                       (cons terminal 'process)))
        (should (string-match-p "Not implemented" (car drover-claude-code-ide-test-warnings)))))))

(ert-deftest drover-claude-code-ide-test-uninstall-stops-watching ()
  "Uninstalling cancels the searches in progress."
  (drover-claude-code-ide-test-isolated
    (drover-claude-code-ide--install)
    (let ((terminal (drover-claude-code-ide-test-terminal "*claude* watched")))
      (cl-letf (((symbol-function 'drover-claude-code-encode-directory) (lambda (_) "watched"))
                ((symbol-function 'drover-claude-code-projects-directory) (lambda () "/nowhere/")))
        (drover-claude-code-ide--capture-session-id terminal "/work/"))
      (should (drover-claude-code-ide-test-pending))
      (drover-claude-code-ide--uninstall)
      (should-not (drover-claude-code-ide-test-pending))
      (should-not drover-claude-code-ide--watchers))))

;;;; Session identifiers

(ert-deftest drover-claude-code-ide-test-live-session-id ()
  "A continued or switched session follows the CLI's record of the process."
  (drover-claude-code-ide-test-isolated
    (let ((terminal (drover-claude-code-ide-test-terminal "*claude* live"))
          (first "11111111-1111-4111-8111-111111111111")
          (second "22222222-2222-4222-8222-222222222222"))
      (with-current-buffer terminal
        (setq-local ghostel--pid 12345)
        (should-not (drover-claude-code-ide-live-session-id))
        (dolist (id (list first second))
          (setq drover-claude-code-ide-test-pids (list (cons 12345 id)))
          (should (equal (drover-claude-code-ide-live-session-id) id)))
        ;; A record of another process names no session of this one.
        (setq drover-claude-code-ide-test-pids (list (cons 99999 first)))
        (should (equal (drover-claude-code-ide-live-session-id) second)))
      (should (equal (drover-claude-code-ide-live-session-id terminal) second))
      ;; An exited program has no record to read.
      (setq drover-claude-code-ide-test-pids (list (cons 12345 first)))
      (delete-process (get-buffer-process terminal))
      (should (equal (drover-claude-code-ide-live-session-id terminal) second)))))

(ert-deftest drover-claude-code-ide-test-live-session-id-of-process ()
  "Without Ghostel's PID, the buffer process's own is read."
  (drover-claude-code-ide-test-isolated
    (let ((terminal (drover-claude-code-ide-test-terminal "*claude* vterm" :mode 'vterm-mode)))
      (cl-letf (((symbol-function 'process-id) (lambda (_) 777)))
        (setq drover-claude-code-ide-test-pids '((777 . "vterm-session")))
        (should (equal (drover-claude-code-ide-live-session-id terminal) "vterm-session"))))))

(ert-deftest drover-claude-code-ide-test-current-session-id ()
  "Without a known session, a Claude terminal guesses its directory's newest."
  (drover-claude-code-ide-test-isolated
    (let ((orphan (drover-claude-code-ide-test-terminal "*claude* drover"))
          (tracked (drover-claude-code-ide-test-terminal "*claude-code[app]*" :mcp-session "/work/app/"))
          (shell (drover-claude-code-ide-test-terminal "*ghostel*")))
      (setq drover-claude-code-ide-test-newest '(("/work/drover/" . "newest") ("/work/app/" . "app")))
      (should (equal (drover-claude-code-ide-current-session-id orphan) "newest"))
      (should (equal (drover-claude-code-ide-current-session-id tracked) "app"))
      (should-not (drover-claude-code-ide-current-session-id shell))
      (with-current-buffer orphan
        (setq-local drover-claude-code-ide-session-id "known")
        (should (equal (drover-claude-code-ide-current-session-id) "known"))))))

;;;; Sessions

(ert-deftest drover-claude-code-ide-test-sessions ()
  "Tracked sessions and Claude terminals with a live process are listed."
  (drover-claude-code-ide-test-with
    (let* ((tracked (drover-claude-code-ide-test-terminal "*claude-code[app]*" :mcp-session "/work/app"))
           (orphan (drover-claude-code-ide-test-terminal "*claude* ~/notes" :mode 'vterm-mode
                                                         :directory "/work/notes/"))
           (shell (drover-claude-code-ide-test-terminal "*ghostel*"))
           (exited (drover-claude-code-ide-test-terminal "*claude* exited"))
           (dead (drover-claude-code-ide-test-terminal "*claude-code[dead]*" :mcp-session t))
           (text (generate-new-buffer "*claude* text")))
      (push text drover-claude-code-ide-test-buffers)
      (delete-process (get-buffer-process exited))
      (delete-process (get-buffer-process dead))
      (with-current-buffer tracked
        (setq-local drover-claude-code-ide-session-id "tracked-id"))
      (setq drover-claude-code-ide-test-newest '(("/work/notes/" . "notes-guess")))
      (let ((sessions (drover-claude-code-ide--sessions)))
        (should (equal (mapcar #'drover-session-buffer sessions) (list tracked orphan)))
        (should-not (memq shell (mapcar #'drover-session-buffer sessions)))
        (pcase-let ((`(,a ,b) sessions))
          (should (eq (drover-session-backend a) 'claude-code-ide))
          (should (eq (drover-session-engine a) 'claude-code))
          (should (eq (drover-session-engine b) 'claude-code))
          (should (equal (drover-session-engine-id a) "tracked-id"))
          (should-not (drover-session-engine-id b))
          (should (equal (drover-session-title a) "*claude-code[app]*"))
          (should (equal (drover-session-title b) "~/notes"))
          (should (equal (drover-session-directory a) "/work/app/"))
          (should (equal (drover-session-directory b) "/work/notes/"))
          (should (equal (drover-session-branch a) "main"))
          (should (eq (drover-session-status b) 'idle))
          (should-not (equal (drover-session-id a) (drover-session-id b)))
          ;; Tokens stay while the buffers live, whatever their names.
          (with-current-buffer orphan
            (rename-buffer "*claude* renamed"))
          (should (equal (mapcar #'drover-session-id (drover-claude-code-ide--sessions))
                         (list (drover-session-id a) (drover-session-id b))))
          (should (eq (drover-session-buffer (drover-find 'claude-code-ide (drover-session-id b)))
                      orphan))
          (should (eq (drover-session-buffer (drover-find 'claude-code-ide (drover-session-id a)))
                      tracked)))))))

(ert-deftest drover-claude-code-ide-test-engine-id-never-guessed ()
  "A terminal's engine identifier stays unknown until the CLI records it.
drover merges sessions of different backends by engine identifier, so a
guess could hide another session of the directory."
  (drover-claude-code-ide-test-with
    (let ((terminal (drover-claude-code-ide-test-terminal "*claude* guess")))
      (setq drover-claude-code-ide-test-newest '(("/work/drover/" . "newest")))
      (should-not (drover-session-engine-id (drover-claude-code-ide-test-session terminal)))
      ;; Links still guess, as they did.
      (should (equal (drover-claude-code-ide-current-session-id terminal) "newest"))
      (with-current-buffer terminal
        (setq-local ghostel--pid 1))
      (setq drover-claude-code-ide-test-pids '((1 . "live")))
      (should (equal (drover-session-engine-id (drover-claude-code-ide-test-session terminal)) "live")))))

(ert-deftest drover-claude-code-ide-test-project ()
  "A session's project is its directory's, found once per directory."
  (drover-claude-code-ide-test-with
    (let* ((root (file-name-as-directory (make-temp-file "drover-project" t)))
           (terminal (drover-claude-code-ide-test-terminal "*claude* project" :directory root))
           (lookups 0)
           (project-current (symbol-function 'project-current)))
      (unwind-protect
          (progn
            (make-directory (expand-file-name ".git" root))
            (cl-letf (((symbol-function 'project-current)
                       (lambda (&rest arguments)
                         (cl-incf lookups)
                         (apply project-current arguments))))
              (should (equal (drover-session-project (drover-claude-code-ide-test-session terminal))
                             (file-name-nondirectory (directory-file-name root))))
              (drover-claude-code-ide-test-session terminal)
              (should (= lookups 1))
              (with-current-buffer terminal
                (setq default-directory "/work/elsewhere/"))
              (should-not (drover-session-project (drover-claude-code-ide-test-session terminal)))
              (should (= lookups 2))))
        (delete-directory root t)))))

(ert-deftest drover-claude-code-ide-test-session-at-point ()
  "A Claude terminal tells its session; another terminal none."
  (drover-claude-code-ide-test-with
    (let ((claude (drover-claude-code-ide-test-terminal "*claude* at point"))
          (shell (drover-claude-code-ide-test-terminal "*ghostel* shell")))
      (dolist (buffer (list claude shell))
        (with-current-buffer buffer
          (drover-claude-code-ide--setup-terminal)))
      (let ((session (drover-buffer-session claude)))
        (should (eq (drover-session-backend session) 'claude-code-ide))
        (should (equal (drover-session-id session)
                       (drover-session-id (drover-claude-code-ide-test-session claude)))))
      (should-not (drover-buffer-session shell)))))

(ert-deftest drover-claude-code-ide-test-killing-reports ()
  "A Claude terminal going away is reported."
  (drover-claude-code-ide-test-with
    (let ((claude (drover-claude-code-ide-test-terminal "*claude* killed")))
      (with-current-buffer claude
        (drover-claude-code-ide--setup-terminal))
      (setq drover-claude-code-ide-test-changes nil)
      (kill-buffer claude)
      (should (equal drover-claude-code-ide-test-changes '((claude-code-ide)))))))

;;;; Statuses

(ert-deftest drover-claude-code-ide-test-statuses ()
  "Ghostel's program status gives the session's status and when it began."
  (drover-claude-code-ide-test-with
    (let ((terminal (drover-claude-code-ide-test-terminal "*claude* status"))
          (drover-claude-code-ide-test-clock 100.0))
      (cl-letf (((symbol-function 'float-time) (lambda (&rest _) drover-claude-code-ide-test-clock)))
        (cl-flet ((session () (drover-claude-code-ide-test-session terminal)))
          (should (eq (drover-session-status (session)) 'idle))
          (should-not (drover-session-since (session)))
          (drover-claude-code-ide-test-status terminal '(("" :state working :progress 40)))
          (should (eq (drover-session-status (session)) 'working))
          (should (equal (drover-session-detail (session)) "40%"))
          (should (= (drover-session-since (session)) 100.0))
          (should (equal drover-claude-code-ide-test-changes '((claude-code-ide))))
          ;; Progress does not start the state again.
          (setq drover-claude-code-ide-test-clock 110.0)
          (drover-claude-code-ide-test-status terminal '(("" :state working :progress 60 :msg "Reading")))
          (should (equal (drover-session-detail (session)) "Reading"))
          (should (= (drover-session-since (session)) 100.0))
          (should (= (drover-session-updated (session)) 110.0))
          (setq drover-claude-code-ide-test-clock 120.0)
          (drover-claude-code-ide-test-status terminal '(("" :state blocked :kind permission :title "Bash")))
          (should (eq (drover-session-status (session)) 'waiting))
          (should (equal (drover-session-requests (session)) '((:kind permission :summary "Bash"))))
          (should (= (drover-session-since (session)) 120.0))
          (drover-claude-code-ide-test-status
           terminal '(("" :state blocked :kind question :msg "Which file?" :title "Claude")))
          (should (equal (drover-session-requests (session)) '((:kind question :summary "Which file?"))))
          (drover-claude-code-ide-test-status terminal '(("" :state blocked :kind auth)))
          (should (equal (drover-session-requests (session)) '((:kind permission :summary "auth"))))
          (drover-claude-code-ide-test-status terminal '(("" :state blocked)))
          (should (equal (drover-session-requests (session)) '((:kind permission :summary "permission"))))
          (setq drover-claude-code-ide-test-clock 130.0)
          (drover-claude-code-ide-test-status terminal '(("" :state done)))
          (should (eq (drover-session-status (session)) 'done))
          (should (= (drover-session-since (session)) 130.0))
          (drover-claude-code-ide-test-status terminal '(("" :state error :msg "Overloaded")))
          (should (eq (drover-session-status (session)) 'failed))
          (should (equal (drover-session-detail (session)) "Overloaded"))
          ;; Ghostel drops ended turns on input.
          (drover-claude-code-ide-test-status terminal nil)
          (should (eq (drover-session-status (session)) 'idle))
          (should-not (drover-session-detail (session))))))))

(ert-deftest drover-claude-code-ide-test-statuses-of-records-beyond-the-root ()
  "A request or an error of any record wins over the root, as in Ghostel's mode line."
  (drover-claude-code-ide-test-with
    (let ((terminal (drover-claude-code-ide-test-terminal "*claude* records")))
      (drover-claude-code-ide-test-status
       terminal '(("task/2" :state blocked :kind question :msg "Second")
                  ("task/1" :state error :msg "Broken")
                  ("task/0" :state blocked :msg "First")
                  ("" :state working)))
      (let ((session (drover-claude-code-ide-test-session terminal)))
        (should (eq (drover-session-status session) 'waiting))
        (should (equal (drover-session-requests session)
                       '((:kind permission :summary "First") (:kind question :summary "Second")))))
      (drover-claude-code-ide-test-status terminal '(("task/1" :state error :title "Agent") ("" :state working)))
      (let ((session (drover-claude-code-ide-test-session terminal)))
        (should (eq (drover-session-status session) 'failed))
        (should (equal (drover-session-detail session) "Agent")))
      (drover-claude-code-ide-test-status terminal '(("task/1" :state working) ("" :state done)))
      (should (eq (drover-session-status (drover-claude-code-ide-test-session terminal)) 'done)))))

(ert-deftest drover-claude-code-ide-test-turns-seen ()
  "A turn ending in front of the user, or looked at since, is seen."
  (drover-claude-code-ide-test-with
    (let ((terminal (drover-claude-code-ide-test-terminal "*claude* seen")))
      (cl-flet ((status () (drover-session-status (drover-claude-code-ide-test-session terminal))))
        (setq drover-claude-code-ide-test-focused (list terminal))
        (drover-claude-code-ide-test-status terminal '(("" :state done)))
        (should (eq (status) 'idle))
        (setq drover-claude-code-ide-test-focused nil)
        (drover-claude-code-ide-test-status terminal '(("" :state working)))
        (drover-claude-code-ide-test-status terminal '(("" :state done)))
        (should (eq (status) 'done))
        ;; Records of other programs leave the turn unseen.
        (drover-claude-code-ide-test-status terminal '(("other" :state working) ("" :state done)))
        (should (eq (status) 'done))
        (setq drover-claude-code-ide-test-changes nil)
        (setq drover-claude-code-ide-test-focused (list terminal))
        (drover-claude-code-ide--note-focus (selected-frame))
        (should (eq (status) 'idle))
        (should (equal drover-claude-code-ide-test-changes '((claude-code-ide))))
        ;; Seeing it again changes nothing.
        (drover-claude-code-ide--note-focus)
        (should (= (length drover-claude-code-ide-test-changes) 1))
        (setq drover-claude-code-ide-test-focused nil)
        (drover-claude-code-ide-test-status terminal '(("" :state error :msg "Failed")))
        (should (eq (status) 'failed))
        (drover-mark-seen (drover-claude-code-ide-test-session terminal))
        (should (eq (status) 'idle))
        ;; The next turn is unseen again.
        (drover-claude-code-ide-test-status terminal '(("" :state working)))
        (drover-claude-code-ide-test-status terminal '(("" :state done)))
        (should (eq (status) 'done))))))

(ert-deftest drover-claude-code-ide-test-status-of-other-terminals ()
  "Status changes of terminals that are not Claude's are not reported."
  (drover-claude-code-ide-test-with
    (let ((shell (drover-claude-code-ide-test-terminal "*ghostel*")))
      (drover-claude-code-ide-test-status shell '(("" :state working)))
      (should-not drover-claude-code-ide-test-changes))))

(ert-deftest drover-claude-code-ide-test-vterm-idles ()
  "A vterm terminal has no program status, and idles."
  (drover-claude-code-ide-test-with
    (let ((terminal (drover-claude-code-ide-test-terminal "*claude* vterm" :mode 'vterm-mode)))
      (let ((session (drover-claude-code-ide-test-session terminal)))
        (should (eq (drover-session-status session) 'idle))
        (should-not (drover-session-since session))
        (should-not (drover-session-requests session))))))

(ert-deftest drover-claude-code-ide-test-focused-buffers ()
  "The terminals in front of the user are those of focused frames' selected windows."
  (cl-letf (((symbol-function 'frame-focus-state) (lambda (&rest _) t)))
    (should (equal (drover-claude-code-ide--focused-buffers) (list (window-buffer (selected-window)))))
    (should (drover-claude-code-ide--focused-p (window-buffer (selected-window)))))
  (cl-letf (((symbol-function 'frame-focus-state) (lambda (&rest _) nil)))
    (should-not (drover-claude-code-ide--focused-buffers))
    (should-not (drover-claude-code-ide--focused-p (window-buffer (selected-window))))))

;;;; Sending

(ert-deftest drover-claude-code-ide-test-ready-p ()
  "The interface reads input once its caret shows; quotes do not count."
  (drover-claude-code-ide-test-isolated
    (let ((terminal (drover-claude-code-ide-test-terminal "*claude* ready")))
      (with-current-buffer terminal
        (insert "Welcome\n> a quote\n")
        (should-not (drover-claude-code-ide-ready-p terminal))
        (insert ">" (string #xa0) "\n")
        (should (eq (drover-claude-code-ide-ready-p terminal) t))
        (erase-buffer)
        (insert "❯ \n")
        (should (drover-claude-code-ide-ready-p terminal)))
      (kill-buffer terminal)
      (should-not (drover-claude-code-ide-ready-p terminal)))))

(defmacro drover-claude-code-ide-test-typing (&rest body)
  "Run BODY with the input functions of Ghostel, vterm and eat recorded."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'ghostel-paste-string) (drover-claude-code-ide-test--recorder 'ghostel-paste-string))
             ((symbol-function 'ghostel-send-string) (drover-claude-code-ide-test--recorder 'ghostel-send-string))
             ((symbol-function 'ghostel-send-key) (drover-claude-code-ide-test--recorder 'ghostel-send-key))
             ((symbol-function 'vterm-send-string) (drover-claude-code-ide-test--recorder 'vterm-send-string))
             ((symbol-function 'vterm-send-return) (drover-claude-code-ide-test--recorder 'vterm-send-return))
             ((symbol-function 'vterm-send-escape) (drover-claude-code-ide-test--recorder 'vterm-send-escape))
             ((symbol-function 'vterm-copy-mode)
              (lambda (arg)
                (push (list 'vterm-copy-mode arg) drover-claude-code-ide-test-calls)
                (setq-local vterm-copy-mode nil)))
             ((symbol-function 'eat-term-send-string) (drover-claude-code-ide-test--recorder 'eat-term-send-string)))
     ,@body))

(ert-deftest drover-claude-code-ide-test-send-to-ghostel ()
  "A ready Ghostel terminal gets the prompt pasted, then submitted."
  (drover-claude-code-ide-test-with
    (drover-claude-code-ide-test-typing
      (let ((terminal (drover-claude-code-ide-test-terminal "*claude* ghostel")))
        (drover-claude-code-ide-test-ready terminal)
        (drover-claude-code-ide--send (drover-claude-code-ide-test-session terminal) "Line 1\nLine 2" nil)
        (should (equal (drover-claude-code-ide-test-called 'ghostel-paste-string) '(("Line 1\nLine 2"))))
        (should-not (drover-claude-code-ide-test-called 'ghostel-send-key))
        (should (equal (mapcar #'car (drover-claude-code-ide-test-pending))
                       (list drover-claude-code-ide--submit-delay)))
        (should (buffer-local-value 'drover-claude-code-ide--submitting terminal))
        (drover-claude-code-ide-test-fire)
        (should (equal (drover-claude-code-ide-test-called 'ghostel-send-key) '(("return"))))
        (should-not (buffer-local-value 'drover-claude-code-ide--submitting terminal))
        (should-not drover-claude-code-ide-test-warnings)))))

(ert-deftest drover-claude-code-ide-test-send-to-vterm-and-eat ()
  "A vterm terminal leaves copy mode; vterm and eat get a bracketed paste."
  (drover-claude-code-ide-test-with
    (drover-claude-code-ide-test-typing
      (let ((vterm (drover-claude-code-ide-test-terminal "*claude* vterm" :mode 'vterm-mode))
            (eat (drover-claude-code-ide-test-terminal "*claude* eat" :mode 'eat-mode)))
        (with-current-buffer vterm
          (setq-local vterm-copy-mode t))
        (with-current-buffer eat
          (setq-local eat-terminal 'eat-terminal))
        (dolist (terminal (list vterm eat))
          (drover-claude-code-ide-test-ready terminal)
          (drover-claude-code-ide-send-text terminal "Prompt"))
        (drover-claude-code-ide-test-fire)
        (should (equal (reverse drover-claude-code-ide-test-calls)
                       '((vterm-copy-mode -1)
                         (vterm-send-string "\e[200~Prompt\e[201~")
                         (eat-term-send-string eat-terminal "\e[200~Prompt\e[201~")
                         (vterm-send-return)
                         (eat-term-send-string eat-terminal "\r"))))))))

(ert-deftest drover-claude-code-ide-test-send-waits-until-ready ()
  "A starting terminal gets the prompt once it reads input."
  (drover-claude-code-ide-test-with
    (drover-claude-code-ide-test-typing
      (let ((terminal (drover-claude-code-ide-test-terminal "*claude* starting")))
        (drover-claude-code-ide-send-text terminal "Prompt")
        (should-not drover-claude-code-ide-test-calls)
        (should (equal (mapcar (lambda (entry) (list (car entry) (cadr entry)))
                               (drover-claude-code-ide-test-pending))
                       '((0.25 0.25))))
        (drover-claude-code-ide-test-fire)
        (should-not drover-claude-code-ide-test-calls)
        (drover-claude-code-ide-test-ready terminal)
        (drover-claude-code-ide-test-fire)
        (should (equal (drover-claude-code-ide-test-called 'ghostel-paste-string) '(("Prompt"))))
        (drover-claude-code-ide-test-fire)
        (should (equal (drover-claude-code-ide-test-called 'ghostel-send-key) '(("return"))))
        (should-not (drover-claude-code-ide-test-pending))
        (should-not drover-claude-code-ide-test-warnings)))))

(ert-deftest drover-claude-code-ide-test-send-in-order ()
  "A prompt waits while the previous one is not submitted yet."
  (drover-claude-code-ide-test-with
    (drover-claude-code-ide-test-typing
      (let ((terminal (drover-claude-code-ide-test-terminal "*claude* order")))
        (drover-claude-code-ide-test-ready terminal)
        (drover-claude-code-ide-send-text terminal "First")
        (drover-claude-code-ide-send-text terminal "Second")
        (should (equal (drover-claude-code-ide-test-called 'ghostel-paste-string) '(("First"))))
        (drover-claude-code-ide-test-fire)
        (should (equal (mapcar #'car (seq-filter (lambda (call) (memq (car call) '(ghostel-paste-string ghostel-send-key)))
                                                 (reverse drover-claude-code-ide-test-calls)))
                       '(ghostel-paste-string ghostel-send-key ghostel-paste-string)))
        (drover-claude-code-ide-test-fire)
        (should (equal (drover-claude-code-ide-test-called 'ghostel-send-key) '(("return") ("return"))))))))

(ert-deftest drover-claude-code-ide-test-send-gives-up ()
  "A prompt is given up, and said to be in the kill ring, after 30 seconds or a death."
  (drover-claude-code-ide-test-with
    (drover-claude-code-ide-test-typing
      (let ((terminal (drover-claude-code-ide-test-terminal "*claude* slow"))
            (drover-claude-code-ide-test-clock 0.0))
        (cl-letf (((symbol-function 'float-time) (lambda (&rest _) drover-claude-code-ide-test-clock)))
          (drover-claude-code-ide-send-text terminal "Prompt")
          (setq drover-claude-code-ide-test-clock 29.0)
          (drover-claude-code-ide-test-fire)
          (should-not drover-claude-code-ide-test-warnings)
          (setq drover-claude-code-ide-test-clock 31.0)
          (drover-claude-code-ide-test-fire)
          (should (equal drover-claude-code-ide-test-warnings
                         '("*claude* slow was not ready after 30s; the prompt is in the kill ring")))
          (should-not (drover-claude-code-ide-test-pending))
          (setq drover-claude-code-ide-test-warnings nil)
          (drover-claude-code-ide-send-text terminal "Prompt")
          (kill-buffer terminal)
          (drover-claude-code-ide-test-fire)
          (should (equal drover-claude-code-ide-test-warnings
                         '("*claude* slow died before the prompt was sent; it is in the kill ring")))
          (should-not (drover-claude-code-ide-test-pending))
          (drover-claude-code-ide-send-text terminal "Prompt")
          (should (= (length drover-claude-code-ide-test-warnings) 2))
          (should-not drover-claude-code-ide-test-calls))))))

(ert-deftest drover-claude-code-ide-test-send-failure-reported ()
  "A paste that fails is reported, and nothing is left waiting to be submitted."
  (drover-claude-code-ide-test-with
    (let ((terminal (drover-claude-code-ide-test-terminal "*claude* broken")))
      (drover-claude-code-ide-test-ready terminal)
      (cl-letf (((symbol-function 'ghostel-paste-string) (lambda (_) (error "No terminal"))))
        (drover-claude-code-ide-send-text terminal "Prompt"))
      (should (equal drover-claude-code-ide-test-warnings
                     '("*claude* broken did not take the prompt: No terminal; it is in the kill ring")))
      (should-not (buffer-local-value 'drover-claude-code-ide--submitting terminal))
      (should-not (drover-claude-code-ide-test-pending)))))

(ert-deftest drover-claude-code-ide-test-send-to-dead-session ()
  "A session whose terminal died is reported."
  (drover-claude-code-ide-test-with
    (let* ((terminal (drover-claude-code-ide-test-terminal "*claude* dead"))
           (session (drover-claude-code-ide-test-session terminal)))
      (kill-buffer terminal)
      (drover-claude-code-ide--send session "Prompt" nil)
      (should (equal drover-claude-code-ide-test-warnings
                     '("dead died before the prompt was sent; it is in the kill ring"))))))

;;;; Options

(defun drover-claude-code-ide-test-apply (terminal options)
  "Apply OPTIONS to the session of TERMINAL; return how often THEN ran so far."
  (let ((then 0))
    (drover-claude-code-ide--apply-options (drover-claude-code-ide-test-session terminal) options
                                           (lambda () (cl-incf then)))
    (lambda () then)))

(ert-deftest drover-claude-code-ide-test-agents-reported ()
  "A running terminal cannot change its agent nor enter plan mode, and says so."
  (drover-claude-code-ide-test-with
    (let ((terminal (drover-claude-code-ide-test-terminal "*claude* agents"))
          (drover-claude-code-agent-modes '(("plan" . "plan"))))
      (should (= (funcall (drover-claude-code-ide-test-apply terminal '(:agent "plan"))) 1))
      (should (equal drover-claude-code-ide-test-warnings
                     '("*claude* agents enters plan mode only from its own prompt; keeping its current mode")))
      (setq drover-claude-code-ide-test-warnings nil)
      (should (= (funcall (drover-claude-code-ide-test-apply terminal '(:agent "reviewer"))) 1))
      (should (equal drover-claude-code-ide-test-warnings
                     '("*claude* agents takes its agent at session start; keeping its current agent instead of reviewer")))
      (setq drover-claude-code-ide-test-warnings nil)
      (should (= (funcall (drover-claude-code-ide-test-apply terminal nil)) 1))
      (should (= (funcall (drover-claude-code-ide-test-apply terminal '(:variant high))) 1))
      (should (equal drover-claude-code-ide-test-warnings
                     '("Variant selection requires an explicit model for Claude Code"))))))

(ert-deftest drover-claude-code-ide-test-model-switched ()
  "A model is typed as /model once the interface reads input; the prompt follows."
  (drover-claude-code-ide-test-with
    (drover-claude-code-ide-test-typing
      (let ((terminal (drover-claude-code-ide-test-terminal "*claude* model")))
        (drover-claude-code-ide-test-ready terminal)
        (let ((then (drover-claude-code-ide-test-apply
                     terminal (list :model (drover-claude-code-ide-test-model "opus") :variant 'high))))
          (should (equal (drover-claude-code-ide-test-called 'ghostel-send-string) '(("/model opus"))))
          (should (equal (drover-claude-code-ide-test-called 'ghostel-send-key) '(("return"))))
          (should (= (funcall then) 0))
          (should-not (drover-claude-code-ide--takes-input-p terminal))
          (should (equal (mapcar #'car (drover-claude-code-ide-test-pending))
                         (list drover-claude-code-ide--model-delay)))
          (drover-claude-code-ide-test-fire)
          (should (= (funcall then) 1))
          (should (drover-claude-code-ide--takes-input-p terminal))
          (should-not drover-claude-code-ide-test-warnings))
        ;; THEN runs even when the terminal died meanwhile.
        (let ((then (drover-claude-code-ide-test-apply
                     terminal (list :model (drover-claude-code-ide-test-model "sonnet")))))
          (kill-buffer terminal)
          (drover-claude-code-ide-test-fire)
          (should (= (funcall then) 1)))))))

(ert-deftest drover-claude-code-ide-test-model-not-applied ()
  "A terminal not ready, or a model Claude Code cannot run, keeps its model."
  (drover-claude-code-ide-test-with
    (drover-claude-code-ide-test-typing
      (let ((terminal (drover-claude-code-ide-test-terminal "*claude* busy")))
        (should (= (funcall (drover-claude-code-ide-test-apply
                             terminal (list :model (drover-claude-code-ide-test-model "opus"))))
                   1))
        (should (equal drover-claude-code-ide-test-warnings
                       '("*claude* busy is not ready for /model; keeping its current model")))
        (setq drover-claude-code-ide-test-warnings nil)
        (drover-claude-code-ide-test-ready terminal)
        (should (= (funcall (drover-claude-code-ide-test-apply
                             terminal (list :model (drover-claude-code-ide-test-model "gpt-5" 'opencode))))
                   1))
        (should (equal drover-claude-code-ide-test-warnings
                       '("*claude* busy cannot run gpt-5; keeping its current model")))
        (should-not drover-claude-code-ide-test-calls)
        (setq drover-claude-code-ide-test-warnings nil)
        (cl-letf (((symbol-function 'ghostel-send-string) (lambda (_) (error "Gone"))))
          (should (= (funcall (drover-claude-code-ide-test-apply
                               terminal (list :model (drover-claude-code-ide-test-model "opus"))))
                     1)))
        (should (equal drover-claude-code-ide-test-warnings
                       '("Could not set the model of *claude* busy: Gone; sending the prompt anyway")))
        (should-not (drover-claude-code-ide-test-pending))))))

(ert-deftest drover-claude-code-ide-test-current-options ()
  "What a session runs is what the newest turn of its transcript ran."
  (drover-claude-code-ide-test-with
    (let* ((transcript (make-temp-file "drover-transcript" nil ".jsonl"))
           (terminal (drover-claude-code-ide-test-terminal "*claude* options"))
           (model (drover-claude-code-ide-test-model "claude-opus-4-5"))
           (turn '("claude-opus-4-5" . high)))
      (unwind-protect
          (cl-letf (((symbol-function 'drover-claude-code-transcript)
                     (lambda (id directory)
                       (push (list 'transcript id directory) drover-claude-code-ide-test-calls)
                       transcript))
                    ((symbol-function 'drover-claude-code-last-turn)
                     (lambda (file) (should (equal file transcript)) turn))
                    ((symbol-function 'drover-claude-code-catalog) (lambda () 'catalog))
                    ((symbol-function 'drover-claude-code-model)
                     (lambda (id layers catalog)
                       (should (equal id "claude-opus-4-5"))
                       (should-not layers)
                       (should (eq catalog 'catalog))
                       model)))
            (let ((session (drover-claude-code-ide-test-session terminal)))
              (should-not (drover-claude-code-ide--options session))
              (with-current-buffer terminal
                (setq-local drover-claude-code-ide-session-id "known"))
              (should (equal (drover-claude-code-ide--options session) (list :model model :variant 'high)))
              (should (member '(transcript "known" "/work/drover/") drover-claude-code-ide-test-calls))
              (setq turn nil)
              (should-not (drover-claude-code-ide--options session))
              (should (equal (drover-claude-code-ide--settable session) '(:model)))))
        (delete-file transcript)))))

(ert-deftest drover-claude-code-ide-test-recap ()
  "A session's recap is read from its transcript, once its session is known."
  (drover-claude-code-ide-test-with
    (let* ((transcript (make-temp-file "drover-transcript" nil ".jsonl"
                                       (concat (json-encode '((type . "user") (message (content . "Go"))))
                                               "\n")))
           (terminal (drover-claude-code-ide-test-terminal "*claude* recap")))
      (unwind-protect
          (cl-letf (((symbol-function 'drover-claude-code-transcript)
                     (lambda (id directory)
                       (should (equal (list id directory) '("known" "/work/drover/")))
                       transcript)))
            (let ((session (drover-claude-code-ide-test-session terminal))
                  (recap nil))
              (drover-recap session (lambda (value) (setq recap value)))
              (should (equal recap '(:failure "No transcript of this session yet")))
              (with-current-buffer terminal
                (setq-local drover-claude-code-ide-session-id "known"))
              (drover-recap session (lambda (value) (setq recap value)))
              (should (equal recap '(:prompt "Go" :reply nil :changes nil :error nil)))))
        (delete-file transcript)))))

(ert-deftest drover-claude-code-ide-test-session-runs ()
  "A session shows the model and effort of its newest turn, read again once it changed."
  (drover-claude-code-ide-test-with
    (let* ((transcript (make-temp-file "drover-transcript" nil ".jsonl" "{}\n"))
           (terminal (drover-claude-code-ide-test-terminal "*claude* runs"))
           (turn '("claude-opus-4-5" . high))
           (reads 0))
      (unwind-protect
          (cl-letf (((symbol-function 'drover-claude-code-transcript)
                     (lambda (id directory)
                       (should (equal (list id directory) '("known" "/work/drover/")))
                       transcript))
                    ((symbol-function 'drover-claude-code-last-turn)
                     (lambda (file) (should (equal file transcript)) (cl-incf reads) turn))
                    ((symbol-function 'drover-claude-code-catalog) (lambda () 'catalog))
                    ((symbol-function 'drover-claude-code-model)
                     (lambda (id _layers _catalog)
                       (drover-model-create :id id :name (and (equal id "claude-opus-4-5")
                                                              "Claude Opus 4.5")))))
            ;; No transcript known: nothing guessed.
            (let ((session (drover-claude-code-ide-test-session terminal)))
              (should-not (drover-session-model session))
              (should (= reads 0)))
            (with-current-buffer terminal
              (setq-local drover-claude-code-ide-session-id "known"))
            (let ((session (drover-claude-code-ide-test-session terminal)))
              (should (equal (drover-session-model session) "Opus 4.5"))
              (should (equal (drover-session-effort session) "high"))
              ;; Neither whether it is the default nor the provider is known.
              (should-not (drover-session-effort-default session))
              (should-not (drover-session-provider session))
              (should-not (drover-session-provider-name session)))
            ;; Listed again, the unchanged transcript is not read again.
            (drover-claude-code-ide-test-session terminal)
            (should (= reads 1))
            ;; A new turn appends to it: read again.
            (setq turn '("claude-sonnet-9" . nil))
            (write-region "{\"more\": true}\n" nil transcript t 'silent)
            (let ((session (drover-claude-code-ide-test-session terminal)))
              (should (= reads 2))
              (should (equal (drover-session-model session) "claude-sonnet-9"))
              (should-not (drover-session-effort session))))
        (delete-file transcript)))))

;;;; Actions

(ert-deftest drover-claude-code-ide-test-rename ()
  "A renamed terminal keeps its name against terminal titles."
  (drover-claude-code-ide-test-with
    (let ((ghostel (drover-claude-code-ide-test-terminal "*claude* ghostel"))
          (vterm (drover-claude-code-ide-test-terminal "*claude* vterm" :mode 'vterm-mode)))
      (with-current-buffer ghostel
        (setq-local ghostel-buffer-name-function #'ignore))
      (with-current-buffer vterm
        (setq-local vterm-buffer-name-string "*claude* %s"))
      (drover-rename (drover-claude-code-ide-test-session ghostel) "Review")
      (drover-rename (drover-claude-code-ide-test-session vterm) "Build")
      (should (equal (buffer-name ghostel) "*claude* Review"))
      (should (equal (buffer-name vterm) "*claude* Build"))
      (should-not (buffer-local-value 'ghostel-buffer-name-function ghostel))
      (should-not (buffer-local-value 'vterm-buffer-name-string vterm))
      (should (equal (drover-session-title (drover-claude-code-ide-test-session ghostel)) "Review"))
      (should drover-claude-code-ide-test-changes))))

(ert-deftest drover-claude-code-ide-test-interrupt ()
  "Interrupting presses escape in the terminal."
  (drover-claude-code-ide-test-with
    (drover-claude-code-ide-test-typing
      (let ((ghostel (drover-claude-code-ide-test-terminal "*claude* ghostel"))
            (vterm (drover-claude-code-ide-test-terminal "*claude* vterm" :mode 'vterm-mode)))
        (drover-backend-call (drover-claude-code-ide-test-session ghostel) :interrupt)
        (drover-backend-call (drover-claude-code-ide-test-session vterm) :interrupt)
        (should (equal (reverse drover-claude-code-ide-test-calls)
                       '((ghostel-send-key "escape") (vterm-send-escape))))))))

(ert-deftest drover-claude-code-ide-test-preview ()
  "A preview shows the last lines of the terminal."
  (drover-claude-code-ide-test-with
    (let ((terminal (drover-claude-code-ide-test-terminal "*claude* preview"))
          preview)
      (cl-flet ((preview ()
                  (drover-preview (drover-claude-code-ide-test-session terminal)
                                  (lambda (text) (setq preview text)))
                  preview))
        (should-not (preview))
        (with-current-buffer terminal
          (dotimes (line 50)
            (insert (format "line %d\n" line)))
          (insert "\n  \n"))
        (let ((lines (split-string (preview) "\n")))
          (should (= (length lines) drover-claude-code-ide--preview-lines))
          (should (equal (car (last lines)) "line 49"))
          (should (equal (car lines) "line 10")))))))

(ert-deftest drover-claude-code-ide-test-open ()
  "Opening shows the terminal and makes its session the most recently used."
  (drover-claude-code-ide-test-with
    (let* ((terminal (drover-claude-code-ide-test-terminal "*claude-code[open]*" :mcp-session t))
           (mcp-session (buffer-local-value 'claude-code-ide--session terminal))
           (session (drover-claude-code-ide-test-session terminal)))
      (save-window-excursion
        (drover-open session)
        (should (eq (window-buffer (selected-window)) terminal))
        (should (numberp (claude-code-ide-mcp-session-last-used mcp-session))))
      ;; In another window, the one selected keeping its buffer.
      (save-window-excursion
        (delete-other-windows)
        (switch-to-buffer "*scratch*")
        (let ((origin (selected-window)))
          (drover-open session t)
          (should-not (eq (selected-window) origin))
          (should (eq (window-buffer (selected-window)) terminal))
          (should (eq (window-buffer origin) (get-buffer "*scratch*")))))
      (save-window-excursion
        (drover-respond session)
        (should (eq (window-buffer (selected-window)) terminal)))
      (kill-buffer terminal)
      (should-error (drover-open session) :type 'user-error))))

(ert-deftest drover-claude-code-ide-test-buffer-for-directory ()
  "The most recently used live session of a directory is found."
  (drover-claude-code-ide-test-isolated
    (let* ((claude-code-ide-mcp--sessions (make-hash-table :test #'equal))
           (old (drover-claude-code-ide-test-terminal "*claude-code[a]*" :mcp-session "/work/a/"))
           (new (drover-claude-code-ide-test-terminal "*claude-code[a]<2>*" :mcp-session "/work/a/")))
      (dolist (session drover-claude-code-ide-test-mcp-sessions)
        (puthash (claude-code-ide-mcp-session-session-id session) session claude-code-ide-mcp--sessions))
      (setf (claude-code-ide-mcp-session-last-used (buffer-local-value 'claude-code-ide--session old)) 1.0)
      (setf (claude-code-ide-mcp-session-last-used (buffer-local-value 'claude-code-ide--session new)) 2.0)
      (should (eq (drover-claude-code-ide-buffer-for-directory "/work/a/") new))
      (should-not (drover-claude-code-ide-buffer-for-directory "/work/b/"))
      (delete-process (get-buffer-process new))
      (should-not (drover-claude-code-ide-buffer-for-directory "/work/a/")))))

;;;; Installing

(ert-deftest drover-claude-code-ide-test-models ()
  "While a session runs, the catalog its CLI caches gives Claude Code's models."
  (drover-test-with-homes
    (drover-claude-code-ide-test-with
      (should (member '(claude-code -5 drover-claude-code-ide--models t) drover--model-sources))
      (drover-test-write (expand-file-name "cache/model-catalog/org-cc.json" drover-claude-code-home)
                         "{\"catalog\": {\"surface\": \"cc\", \"config\": {\"models\": [
                            {\"id\": \"claude-opus-5-5\", \"name\": \"Opus 5.5\",
                             \"thinking\": {\"effort_options\": [{\"id\": \"low\"}, {\"id\": \"high\"}]}},
                            {\"id\": \"claude-haiku-4-5\", \"name\": \"Haiku 4.5\"}]}}}")
      ;; No session runs.
      (should-not (drover-claude-code-ide--models "/work/drover/"))
      (let ((terminal (drover-claude-code-ide-test-terminal "*claude* models" :mcp-session t)))
        (let ((models (drover-claude-code-ide--models "/work/drover/")))
          (should (equal (mapcar #'drover-model-id models) '("claude-opus-5-5" "claude-haiku-4-5")))
          (should (equal (drover-model-variants (car models)) '((low) (high))))
          (should (equal (drover-model-engines (car models)) '(claude-code))))
        (delete-process (get-buffer-process terminal))
        (should-not (drover-claude-code-ide--models "/work/drover/")))
      ;; Uninstalling removes the source.
      (drover-claude-code-ide--uninstall)
      (should-not (seq-find (lambda (source) (eq (nth 2 source) #'drover-claude-code-ide--models))
                            drover--model-sources)))))

(ert-deftest drover-claude-code-ide-test-backend ()
  "The backend offers the operations of a terminal, and runs Claude Code."
  (drover-claude-code-ide-test-with
    ;; With it, the backend listing Claude Code's transcripts, whose
    ;; conversations it resumes.
    (should (equal (sort (drover-backends) #'string<) '(claude-code claude-code-ide)))
    (should (equal (drover-backend-property 'claude-code :recorded) '(claude-code)))
    (should (eq (cdr (drover-resumer 'claude-code)) #'drover-claude-code-ide-resume))
    (should (equal (drover-backend-label 'claude-code-ide) "Claude Code IDE"))
    (should (equal (drover-backend-engines 'claude-code-ide) '(claude-code)))
    (dolist (operation '(:sessions :open :respond :interrupt :mark-seen :preview :recap :new :buffer
                                   :rename :create :apply-options :send :options :settable
                                   :delete :fork :compact :supports))
      (should (functionp (drover-backend-property 'claude-code-ide operation))))
    ;; Drover archives its sessions: it has no archive of its own.
    (dolist (operation '(:archive :unarchive :archived :check :history))
      (should-not (drover-backend-property 'claude-code-ide operation)))))

(ert-deftest drover-claude-code-ide-test-engine-front-end ()
  "The Claude Code engine reads claude-code-ide's flags and start directory."
  (drover-claude-code-ide-test-with
    (let ((claude-code-ide-cli-extra-flags "--model opus"))
      (should (equal (funcall drover-claude-code-start-flags-function) "--model opus"))
      (cl-letf (((symbol-function 'claude-code-ide--get-working-directory)
                 (lambda () (concat default-directory "root/"))))
        (should (equal (funcall drover-claude-code-project-directory-function "/work/app")
                       "/work/app/root/")))))
  ;; A function of the user's stays.
  (drover-claude-code-ide-test-isolated
    (setq drover-claude-code-start-flags-function #'ignore
          drover-claude-code-project-directory-function #'identity)
    (drover-claude-code-ide--install)
    (drover-claude-code-ide--uninstall)
    (should (eq drover-claude-code-start-flags-function #'ignore))
    (should (eq drover-claude-code-project-directory-function #'identity))))

(ert-deftest drover-claude-code-ide-test-install-and-uninstall ()
  "Installing follows terminals; uninstalling leaves nothing behind."
  (drover-claude-code-ide-test-isolated
    (let ((ghostel-mode-hook nil)
          (vterm-mode-hook nil)
          (ghostel-program-status-functions '(ghostel-status-mode-line))
          (ghostel-exit-functions nil)
          (terminal (drover-claude-code-ide-test-terminal "*claude* existing"))
          (text (generate-new-buffer " text")))
      (push text drover-claude-code-ide-test-buffers)
      (with-current-buffer text
        (setq-local drover-session-at-point-function #'ignore))
      (drover-claude-code-ide--install)
      (should (assq 'claude-code-ide drover--backends))
      (should (eq drover-claude-code-start-flags-function #'drover-claude-code-ide--cli-flags))
      (should (eq drover-claude-code-project-directory-function #'drover-claude-code-ide--working-directory))
      (should (advice-member-p #'drover-claude-code-ide--track-terminal 'claude-code-ide--create-terminal-session))
      (should (memq #'drover-claude-code-ide--setup-terminal ghostel-mode-hook))
      (should (memq #'drover-claude-code-ide--setup-terminal vterm-mode-hook))
      (should (equal ghostel-program-status-functions
                     '(drover-claude-code-ide--status-changed ghostel-status-mode-line)))
      (should (memq #'drover-claude-code-ide--terminal-exited ghostel-exit-functions))
      (should (memq #'drover-claude-code-ide--add-hooks after-load-functions))
      (should (memq #'drover-claude-code-ide--note-focus window-selection-change-functions))
      (should (memq #'drover-claude-code-ide--note-focus window-buffer-change-functions))
      (should (advice-function-member-p #'drover-claude-code-ide--note-focus after-focus-change-function))
      (with-current-buffer terminal
        (should (eq drover-session-at-point-function #'drover-claude-code-ide--buffer-session))
        (should (memq #'drover-claude-code-ide--terminal-killed kill-buffer-hook)))
      ;; Installing twice adds nothing twice.
      (drover-claude-code-ide--install)
      (should (= (length ghostel-program-status-functions) 2))
      (drover-claude-code-ide--uninstall)
      (should-not (assq 'claude-code-ide drover--backends))
      (should-not drover-claude-code-start-flags-function)
      (should-not drover-claude-code-project-directory-function)
      (should-not (advice-member-p #'drover-claude-code-ide--track-terminal
                                   'claude-code-ide--create-terminal-session))
      (should-not ghostel-mode-hook)
      (should-not vterm-mode-hook)
      (should (equal ghostel-program-status-functions '(ghostel-status-mode-line)))
      (should-not ghostel-exit-functions)
      (should-not (memq #'drover-claude-code-ide--add-hooks after-load-functions))
      (should-not (memq #'drover-claude-code-ide--note-focus window-selection-change-functions))
      (should-not (memq #'drover-claude-code-ide--note-focus window-buffer-change-functions))
      (should-not (advice-function-member-p #'drover-claude-code-ide--note-focus after-focus-change-function))
      (with-current-buffer terminal
        (should-not drover-session-at-point-function)
        (should-not (memq #'drover-claude-code-ide--terminal-killed kill-buffer-hook)))
      (with-current-buffer text
        (should (eq drover-session-at-point-function #'ignore))))))

(ert-deftest drover-claude-code-ide-test-hooks-wait-for-their-package ()
  "Hooks of terminal packages not loaded yet are left alone until they load."
  (should-not (boundp 'ghostel-program-status-functions))
  (should-not (boundp 'vterm-exit-functions))
  (drover-claude-code-ide-test-isolated
    (drover-claude-code-ide--install)
    (unwind-protect
        (progn
          (should-not (boundp 'ghostel-program-status-functions))
          (should-not (boundp 'eat-mode-hook))
          ;; The package loads.
          (let ((ghostel-program-status-functions '(ghostel-status-mode-line)))
            (should (memq #'drover-claude-code-ide--add-hooks after-load-functions))
            (drover-claude-code-ide--add-hooks "/elpa/ghostel/ghostel-status.el")
            (should (memq #'drover-claude-code-ide--status-changed ghostel-program-status-functions))))
      (drover-claude-code-ide--uninstall)))
  (should-not (boundp 'ghostel-program-status-functions))
  (should-not (boundp 'eat-mode-hook))
  (should-not (boundp 'vterm-exit-functions)))

;;;; Prompts sent later

(ert-deftest drover-claude-code-ide-test-input-empty ()
  "The live cursor in an empty input box takes a prompt; old screens and drafts do not.
The pill of the context Claude Code attaches is no draft; what follows it is."
  (drover-claude-code-ide-test-isolated
    (let ((terminal (drover-claude-code-ide-test-terminal "*claude* input"))
          (cursor nil))
      (with-current-buffer terminal
        (insert "old transcript\n> \nold answer\n───\n>" (string #xa0) "\n───\nfooter\n")
        (goto-char (point-min))
        (search-forward "old answer")
        (setq cursor (point))
        (cl-letf (((symbol-function 'ghostel-cursor-point) (lambda () cursor)))
          (should-not (drover-claude-code-ide-input-empty-p terminal))
          (goto-char (point-max))
          (search-backward (string #xa0))
          (forward-char 1)
          (setq cursor (point))
          (should (drover-claude-code-ide-input-empty-p terminal))
          (insert "unfinished draft")
          (should-not (drover-claude-code-ide-input-empty-p terminal))
          (delete-region cursor (line-end-position))
          (insert "[⧉ In Emacs buffer '*claude* fixture'] ")
          (setq cursor (point))
          (should (drover-claude-code-ide-input-empty-p terminal))
          (insert "unfinished draft")
          (should-not (drover-claude-code-ide-input-empty-p terminal))))
      ;; vterm tells its cursor too; a terminal without one is not ready.
      (with-current-buffer terminal
        (setq major-mode 'eat-mode))
      (should-not (drover-claude-code-ide-input-empty-p terminal)))))

(ert-deftest drover-claude-code-ide-test-scheduled-prompt ()
  "A scheduled prompt waits for an empty input box, then is pasted and submitted."
  (drover-claude-code-ide-test-with
    (drover-claude-code-ide-test-typing
      (let* ((terminal (drover-claude-code-ide-test-terminal "*claude* later"))
             (session (drover-claude-code-ide-test-session terminal))
             (ready nil) (outcome nil))
        (should (eq (drover-backend-property 'claude-code-ide :blocker) #'drover-claude-code-ide--blocker))
        (should (eq (drover-backend-property 'claude-code-ide :submit) #'drover-claude-code-ide--deliver))
        (cl-letf (((symbol-function 'drover-claude-code-ide-input-empty-p) (lambda (_) ready)))
          (should (equal (drover-claude-code-ide--blocker session) "the cursor is not in an empty input box"))
          (setq ready t)
          (should-not (drover-claude-code-ide--blocker session))
          (drover-claude-code-ide--deliver session "Line 1\nLine 2"
                                           (lambda () (setq outcome 'sent))
                                           (lambda (message) (setq outcome message)))
          (should (equal (drover-claude-code-ide-test-called 'ghostel-paste-string) '(("Line 1\nLine 2"))))
          ;; While it is submitted, no other prompt goes.
          (should (equal (drover-claude-code-ide--blocker session) "another prompt is being submitted"))
          (should-not outcome)
          (drover-claude-code-ide-test-fire)
          (should (equal (drover-claude-code-ide-test-called 'ghostel-send-key) '(("return"))))
          (should (eq outcome 'sent))
          (should-not (drover-claude-code-ide--blocker session))
          ;; A terminal exiting before the return interrupts the prompt.
          (setq outcome nil)
          (drover-claude-code-ide--deliver session "again" (lambda () (setq outcome 'sent))
                                           (lambda (message) (setq outcome message)))
          (delete-process (get-buffer-process terminal))
          (drover-claude-code-ide-test-fire)
          (should (equal outcome "the session exited before its prompt was submitted"))
          (should (= 1 (length (drover-claude-code-ide-test-called 'ghostel-send-key)))))
        (kill-buffer terminal)
        (should (equal (drover-claude-code-ide--blocker session) "the terminal is gone"))))))

(ert-deftest drover-claude-code-ide-test-registered-on-load ()
  "Loading the backend registers it and follows terminals; unloading undoes it."
  (drover-claude-code-ide-test-isolated
    (unwind-protect
        (progn
          (load "drover-claude-code-ide" nil t)
          (should (assq 'claude-code-ide drover--backends))
          (should (advice-member-p #'drover-claude-code-ide--track-terminal
                                   'claude-code-ide--create-terminal-session))
          (should-not (drover-claude-code-ide-unload-function))
          (should-not (assq 'claude-code-ide drover--backends))
          (should-not (advice-member-p #'drover-claude-code-ide--track-terminal
                                       'claude-code-ide--create-terminal-session)))
      (drover-claude-code-ide--uninstall))))

(provide 'drover-claude-code-ide-test)
;;; drover-claude-code-ide-test.el ends here
