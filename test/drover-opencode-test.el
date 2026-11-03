;;; drover-opencode-test.el --- Tests of the OpenCode backend  -*- lexical-binding: t; -*-

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

;; Every opencode.el function the backend calls is stubbed: the registry
;; serves `drover-opencode-test-entries', the status functions follow
;; the `opencode-state' contract, and actions are recorded in
;; `drover-opencode-test-calls'.  No request reaches a service.  The
;; engine's tests answer from the catalogs of `drover-opencode-fixtures';
;; those of the prompt operations go through opencode.el's own switches
;; and send checks, with `opencode-request' stubbed.

;;; Code:

(require 'drover-test-helpers)
;; opencode first: drover loads the backend once opencode is loaded.
(require 'opencode)
(require 'drover-opencode)
(require 'drover-prompt)
(require 'drover-opencode-fixtures)

(defvar opencode-state-updated-functions)
(defvar opencode-session-buffer-functions)

(defvar drover-opencode-test-entries nil
  "Registry entries `opencode-state-sessions' returns.")

(defvar drover-opencode-test-calls nil
  "Calls of stubbed opencode.el functions, newest first, as (FUNCTION . ARGUMENTS).")

(defvar drover-opencode-test-fetches nil
  "Pending projects fetches, oldest first, as (CALLBACK . ELSE).")

(defvar drover-opencode-test-handlers nil
  "Registered event handlers, oldest first, as (TYPE . FUNCTION).")

(defvar drover-opencode-test-branches '(("/work/drover/" . "main") ("/work/opencode/" . "dev"))
  "Branches of the directories, for the stubbed `drover-git-branch'.")

(defvar drover-opencode-test-projects
  '(((id . "prj_drover") (name . "Drover") (canonical . "/work/drover"))
    ((id . "prj_opencode") (canonical . "/work/opencode") (sandboxes "/work/opencode-wt"))
    ((id . "global") (canonical . "/")))
  "Projects of the stubbed service.")

(defun drover-opencode-test--unread-p (entry)
  "Return non-nil when registry ENTRY is unread.
This follows the `opencode-state' contract."
  (let ((idle (plist-get entry :idle))
        (viewed (plist-get entry :viewed)))
    (and (not (plist-get entry :running)) idle (or (null viewed) (< viewed idle)))))

(defun drover-opencode-test--status (entry)
  "Return the status of registry ENTRY, per the `opencode-state' contract."
  (cond ((plist-get entry :requests) 'waiting)
        ((plist-get entry :running) (if (plist-get entry :retry) 'retrying 'working))
        ((drover-opencode-test--unread-p entry)
         (if (eq (plist-get entry :outcome) 'failed) 'failed 'done))
        (t 'idle)))

(defun drover-opencode-test--since (entry)
  "Return when the status of registry ENTRY began.
This follows the `opencode-state' contract."
  (pcase (drover-opencode-test--status entry)
    ('waiting (plist-get (car (plist-get entry :requests)) :time))
    ((or 'working 'retrying) (plist-get entry :started))
    ((or 'failed 'done) (plist-get entry :idle))))

(defun drover-opencode-test--recorder (function)
  "Return a stub of FUNCTION recording its calls."
  (lambda (&rest arguments)
    (push (cons function arguments) drover-opencode-test-calls)
    nil))

(defvar drover-opencode-test-fetched nil
  "Callback of the last stubbed `opencode-session-fetch'.")

(defvar drover-opencode-test-archives nil
  "Archives asked of the stubbed `opencode-session-archive', newest first.
Each is (ID RESTORE THEN ELSE), for a test to answer as the service does.")

(defun drover-opencode-test-answer-archive (id &optional refusal)
  "Answer the oldest archive asked of session ID, with REFUSAL if given.
REFUSAL is the error's message: ELSE gets the `opencode-error' of a
409, as the service refuses; otherwise THEN gets the session's info."
  (let ((asked (car (last (seq-filter (lambda (asked) (equal (car asked) id))
                                      drover-opencode-test-archives)))))
    (should asked)
    (setq drover-opencode-test-archives (remq asked drover-opencode-test-archives))
    ;; Without callbacks, as restoring asks, opencode.el reports itself.
    (if refusal
        (funcall (or (nth 3 asked) #'ignore)
                 (opencode-error--make :status 409 :method 'patch
                                       :path (concat "/api/session/" id) :message refusal))
      (funcall (or (nth 2 asked) #'ignore) `((id . ,id))))))

(defmacro drover-opencode-test-with (entries &rest body)
  "Run BODY with the OpenCode backend alone registered, over registry ENTRIES."
  (declare (indent 1) (debug t))
  `(let ((drover--backends nil)
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
         (drover--confirming nil)
         (drover-opencode-test-archives nil)
         (drover-show-subagents 'active)
         (drover-show-history nil)
         (drover--history nil)
         (drover-show-archived nil)
         (drover--archived nil)
         (drover-opencode--archived-ids nil)
         (drover-opencode--refetch-timer nil)
         (drover-opencode--projects nil)
         (drover-opencode--made nil)
         (drover-opencode--hiding (make-hash-table :test #'equal))
         (drover-opencode--projects-fetching nil)
         (drover-opencode--projects-stale nil)
         (drover-opencode--projects-known nil)
         (opencode-state-updated-functions nil)
         (opencode-session-buffer-functions nil)
         (opencode-catalog-changed-functions nil)
         (opencode-session-buffers (make-hash-table :test #'equal))
         (drover-opencode-test-entries ,entries)
         (drover-opencode-test-calls nil)
         (drover-opencode-test-fetches nil)
         (drover-opencode-test-handlers nil))
     (cl-letf (((symbol-function 'opencode-state-sessions)
                (lambda (&optional archived)
                  (should-not archived)
                  drover-opencode-test-entries))
               ((symbol-function 'opencode-state-get)
                (lambda (id)
                  (seq-find (lambda (entry) (equal (plist-get entry :id) id))
                            drover-opencode-test-entries)))
               ((symbol-function 'opencode-state-descendants)
                (lambda (id)
                  (let ((descendants nil)
                        (queue (list id)))
                    (while queue
                      (let ((parent (pop queue)))
                        (dolist (entry drover-opencode-test-entries)
                          (when (equal (plist-get entry :parent-id) parent)
                            (setq descendants (append descendants (list entry))
                                  queue (append queue (list (plist-get entry :id))))))))
                    descendants)))
               ((symbol-function 'opencode-state-status) #'drover-opencode-test--status)
               ((symbol-function 'opencode-state-since) #'drover-opencode-test--since)
               ((symbol-function 'opencode-state-cache)
                (lambda (id) (cdr (assoc id drover-opencode-test-caches))))
               ((symbol-function 'opencode-state-ensure) (drover-opencode-test--recorder 'opencode-state-ensure))
               ((symbol-function 'drover-git-branch)
                (lambda (directory) (cdr (assoc directory drover-opencode-test-branches))))
               ((symbol-function 'opencode-model-display)
                (lambda (model &optional directory)
                  (format "%s in %s" (alist-get 'id model) directory)))
               ((symbol-function 'opencode-variant-display)
                (lambda (model &optional _directory)
                  (if-let* ((variant (alist-get 'variant model))
                            ((not (equal variant "default"))))
                      (propertize variant 'face 'success 'help-echo "Variant: selected")
                    (propertize "medium" 'face 'shadow 'help-echo "Variant: default"))))
               ((symbol-function 'opencode-provider-display)
                (lambda (provider &optional _directory)
                  (propertize (concat "icon-" provider) 'help-echo (concat "Provider: " provider))))
               ((symbol-function 'opencode-agent-display)
                (lambda (agent &optional directory)
                  (propertize (format "%s in %s" agent directory) 'face 'bold)))
               ((symbol-function 'opencode-provider-info)
                (lambda (provider &optional _directory)
                  (and (equal provider "anthropic") '((id . "anthropic") (name . "Anthropic")))))
               ((symbol-function 'opencode-context-usage)
                (lambda (tokens _model &optional _directory)
                  (list :tokens tokens :limit 200000 :fraction (/ tokens 200000.0)
                        :tier (and (>= tokens 180000) '(near 200000 "2×")))))
               ((symbol-function 'opencode-state-context)
                (lambda (id)
                  (push (list 'opencode-state-context id) drover-opencode-test-calls)
                  (plist-get (seq-find (lambda (entry) (equal (plist-get entry :id) id))
                                       drover-opencode-test-entries)
                             :context)))
               ((symbol-function 'opencode-fetch-projects)
                (lambda (callback &optional else)
                  (setq drover-opencode-test-fetches
                        (append drover-opencode-test-fetches (list (cons callback else))))))
               ((symbol-function 'opencode-project-for-directory)
                (lambda (projects directory)
                  (seq-find (lambda (project)
                              (seq-some (lambda (root) (string-prefix-p (file-name-as-directory root) directory))
                                        (cons (alist-get 'canonical project) (alist-get 'sandboxes project))))
                            projects)))
               ((symbol-function 'opencode-project-name)
                (lambda (project)
                  (or (alist-get 'name project)
                      (file-name-nondirectory (directory-file-name (alist-get 'canonical project))))))
               ((symbol-function 'opencode-events-register)
                (lambda (type function)
                  (setq drover-opencode-test-handlers
                        (append drover-opencode-test-handlers (list (cons type function))))))
               ((symbol-function 'opencode-events-unregister) (drover-opencode-test--recorder 'opencode-events-unregister))
               ((symbol-function 'opencode-session-open-by-id) (drover-opencode-test--recorder 'opencode-session-open-by-id))
               ((symbol-function 'opencode-session-fetch)
                (lambda (id callback &optional directory _else)
                  (push (list 'opencode-session-fetch id directory) drover-opencode-test-calls)
                  (setq drover-opencode-test-fetched callback)))
               ((symbol-function 'opencode-session-open)
                (lambda (info &rest arguments)
                  (push (append (list 'opencode-session-open (alist-get 'id info) (selected-window)) arguments)
                        drover-opencode-test-calls)))
               ((symbol-function 'opencode-respond-session) (drover-opencode-test--recorder 'opencode-respond-session))
               ((symbol-function 'opencode-session-interrupt) (drover-opencode-test--recorder 'opencode-session-interrupt))
               ((symbol-function 'opencode-session-archive)
                (lambda (id &optional restore then else)
                  (push (if restore (list 'opencode-session-archive id t) (list 'opencode-session-archive id))
                        drover-opencode-test-calls)
                  (push (list id restore then else) drover-opencode-test-archives)))
               ((symbol-function 'opencode-session-view) (drover-opencode-test--recorder 'opencode-session-view))
               ((symbol-function 'opencode-session-last-reply)
                (lambda (id callback &rest arguments)
                  (push (append (list 'opencode-session-last-reply id) arguments) drover-opencode-test-calls)
                  (funcall callback (concat "Last reply of " id))))
               ((symbol-function 'opencode-session-last-turn)
                (lambda (id callback)
                  (push (list 'opencode-session-last-turn id) drover-opencode-test-calls)
                  (funcall callback (list :prompt (concat "Prompt of " id) :reply "Done"
                                          :changes '(("a.el" 3 1))))))
               ((symbol-function 'opencode-session-create) (drover-opencode-test--recorder 'opencode-session-create))
               ((symbol-function 'opencode-rename-session) (drover-opencode-test--recorder 'opencode-rename-session))
               ((symbol-function 'opencode-session-delete) (drover-opencode-test--recorder 'opencode-session-delete))
               ((symbol-function 'opencode-session-fork) (drover-opencode-test--recorder 'opencode-session-fork))
               ((symbol-function 'opencode-session-compact) (drover-opencode-test--recorder 'opencode-session-compact)))
       (unwind-protect
           (progn (drover-opencode--register) ,@body)
         (when (timerp drover--change-timer)
           (cancel-timer drover--change-timer))))))

(defun drover-opencode-test-answer-projects (&optional projects)
  "Answer the oldest pending projects fetch with PROJECTS.
PROJECTS defaults to `drover-opencode-test-projects'."
  (let ((fetch (pop drover-opencode-test-fetches)))
    (should fetch)
    (funcall (car fetch) (or projects drover-opencode-test-projects))))

(defun drover-opencode-test-fail-projects ()
  "Fail the oldest pending projects fetch."
  (let ((fetch (pop drover-opencode-test-fetches)))
    (should fetch)
    (funcall (cdr fetch) 'opencode-error)))

(defun drover-opencode-test-handler (type)
  "Return the event handler registered for TYPE."
  (cdr (assoc type drover-opencode-test-handlers)))

(defvar drover-opencode-test-caches nil
  "Alist of session IDs and their registry cache, for `opencode-state-cache'.")

(defun drover-opencode-test-session (id)
  "Return the drover session ID of the OpenCode backend."
  (drover-find 'opencode id))

(defun drover-opencode-test-entry (id &rest properties)
  "Return a registry entry ID with PROPERTIES, which override the defaults."
  (append (list :id id) properties
          (list :title (concat "Title " id) :directory "/work/drover/"
                :project-id "prj_drover" :created 10.0 :updated 20.0)))

;;;; Registration

(ert-deftest drover-opencode-test-registered-on-load ()
  (should (assq 'opencode (default-value 'drover--backends))))

(ert-deftest drover-opencode-test-backend-properties ()
  (drover-opencode-test-with nil
    (should (equal (drover-backends) '(opencode)))
    (should (equal (drover-backend-label 'opencode) "OpenCode"))
    (should (equal (drover-backend-engines 'opencode) '(opencode)))
    (let ((session (drover-session-create :backend 'opencode :id "ses_1")))
      (dolist (operation '(:start :sessions :open :respond :interrupt :archive :mark-seen :preview :new
                                  :buffer :rename :delete :fork :compact :history :prefetch :recap
                                  :create :check :apply-options :send :options))
        (should (drover-supports-p session operation)))
      ;; A running session can change all three.
      (should-not (drover-supports-p session :settable)))))

;;;; Mapping

(ert-deftest drover-opencode-test-maps-every-field ()
  (drover-opencode-test-with
      (list (list :id "ses_1" :title "Fix the build" :directory "/work/drover/"
                  :project-id "prj_drover" :parent-id "ses_0" :agent "build"
                  :model '((id . "claude-opus") (providerID . "anthropic") (variant . "high"))
                  :context 190000 :running t :retry nil
                  :requests '((:kind permission :id "per_1" :summary "bash: make test" :time 100.5)
                              (:kind question :id "frm_1" :summary "Which branch?" :time 110.0))
                  :outcome nil :error nil :idle 50.0 :viewed 50.0 :archived nil
                  :created 10.0 :updated 120.25 :started 90.0))
    (drover-start)
    (drover-opencode-test-answer-projects)
    (let ((session (car (drover-sessions))))
      (should (drover-session-p session))
      (should (eq (drover-session-backend session) 'opencode))
      (should (equal (drover-session-id session) "ses_1"))
      ;; The backend's identifiers are the engine's.
      (should (eq (drover-session-engine session) 'opencode))
      (should (equal (drover-session-engine-id session) "ses_1"))
      (should (equal (drover-session-title session) "Fix the build"))
      (should (equal (drover-session-directory session) "/work/drover/"))
      (should (equal (drover-session-project session) "Drover"))
      (should (equal (drover-session-branch session) "main"))
      (should (equal (drover-session-parent session) "ses_0"))
      (should (eq (drover-session-status session) 'waiting))
      (should (equal (drover-session-requests session)
                     '((:kind permission :summary "bash: make test")
                       (:kind question :summary "Which branch?"))))
      (should (eql (drover-session-since session) 100.5))
      (should (eql (drover-session-updated session) 120.25))
      (should (eql (drover-session-created session) 10.0))
      ;; The registry knows nothing of prompt caches yet.
      (should-not (drover-session-cache-expires session))
      (should-not (drover-session-cache-ttl session))
      (should (equal (drover-session-agent session) "build"))
      ;; What it runs, as opencode.el's displays show it in its directory,
      ;; without their properties: drover draws its own.
      (should (equal (drover-session-model session) "claude-opus in /work/drover/"))
      (should (equal-including-properties (drover-session-effort session) "high"))
      (should-not (drover-session-effort-default session))
      (should (equal-including-properties (drover-session-provider session) "icon-anthropic"))
      (should (equal (drover-session-provider-name session) "Anthropic"))
      (should (eql (drover-session-context-tokens session) 190000))
      (should (eql (drover-session-context-limit session) 200000))
      (should (equal (drover-session-context-tier session) '(near 200000 "2×")))
      (should (= (drover-session-context-fraction session) 0.95))
      ;; The size is the one the registry knows: the record asks nothing.
      (should-not (assq 'opencode-state-context drover-opencode-test-calls))
      (should-not (drover-session-detail session))
      (should (equal (drover-session-summary session) "permit: bash: make test")))))

(ert-deftest drover-opencode-test-maps-estimated-context ()
  "A context the service estimated after a compaction is told so, until measured."
  (drover-opencode-test-with
      (list (drover-opencode-test-entry "ses_1" :model '((id . "claude-opus") (providerID . "anthropic"))
                                        :context 40000 :context-estimated t)
            (drover-opencode-test-entry "ses_2" :model '((id . "claude-opus") (providerID . "anthropic"))
                                        :context 40000))
    (let ((estimated (drover-opencode-test-session "ses_1"))
          (measured (drover-opencode-test-session "ses_2")))
      (should (eql (drover-session-context-tokens estimated) 40000))
      (should (drover-session-context-estimated estimated))
      (should (string-prefix-p "~40k of 200k" (drover-session-context-description estimated)))
      (should-not (drover-session-context-estimated measured))
      ;; The next step measures it: the registry clears the flag.
      (setq drover-opencode-test-entries
            (list (drover-opencode-test-entry "ses_1" :model '((id . "claude-opus") (providerID . "anthropic"))
                                              :context 52000)))
      (should-not (drover-session-context-estimated (drover-opencode-test-session "ses_1"))))))

(ert-deftest drover-opencode-test-maps-missing-fields ()
  (drover-opencode-test-with (list (list :id "ses_1" :created 10.0 :updated 20.0))
    (cl-letf (((symbol-function 'drover-git-branch) (lambda (_) (error "No directory to read")))
              ((symbol-function 'opencode-model-display) (lambda (&rest _) (error "No model to show")))
              ((symbol-function 'opencode-variant-display) (lambda (&rest _) (error "No model to show")))
              ((symbol-function 'opencode-provider-display) (lambda (&rest _) (error "No provider")))
              ((symbol-function 'opencode-context-usage) (lambda (&rest _) (error "No context"))))
      (let ((session (car (drover-sessions))))
        (should (equal (drover-session-id session) "ses_1"))
        (should-not (drover-session-title session))
        (should-not (drover-session-directory session))
        (should-not (drover-session-project session))
        (should-not (drover-session-branch session))
        (should-not (drover-session-parent session))
        (should (eq (drover-session-status session) 'idle))
        (should-not (drover-session-requests session))
        (should-not (drover-session-since session))
        (should-not (drover-session-agent session))
        (should-not (drover-session-model session))
        (should-not (drover-session-effort session))
        (should-not (drover-session-provider session))
        (should-not (drover-session-provider-name session))
        (should-not (drover-session-context-tokens session))
        (should-not (drover-session-context-limit session))
        (should-not (drover-session-detail session))))))

(ert-deftest drover-opencode-test-status-working ()
  (drover-opencode-test-with (list (drover-opencode-test-entry "ses_1" :running t :started 90.0))
    (let ((session (drover-opencode-test-session "ses_1")))
      (should (eq (drover-session-status session) 'working))
      (should (eql (drover-session-since session) 90.0))
      (should-not (drover-session-detail session)))))

(ert-deftest drover-opencode-test-status-retrying ()
  (let ((at 1800000000.0))
    (drover-opencode-test-with
        (list (drover-opencode-test-entry "ses_1" :running t :started 90.0
                                          :retry (list :attempt 2 :at at :message "Rate limited"))
              (drover-opencode-test-entry "ses_2" :running t :started 95.0
                                          :retry (list :attempt 1 :at at)))
      (let ((session (drover-opencode-test-session "ses_1")))
        (should (eq (drover-session-status session) 'retrying))
        (should (eql (drover-session-since session) 90.0))
        (should (equal (drover-session-detail session)
                       (format "retry 2 at %s: Rate limited" (format-time-string "%H:%M" at))))
        (should (string-match-p "\\`retry 2 at [0-2][0-9]:[0-5][0-9]: Rate limited\\'"
                                (drover-session-detail session)))
        (should (equal (drover-session-summary session) (drover-session-detail session))))
      (should (equal (drover-session-detail (drover-opencode-test-session "ses_2"))
                     (format "retry 1 at %s" (format-time-string "%H:%M" at)))))))

(ert-deftest drover-opencode-test-status-retrying-time-zone ()
  (let ((zone (getenv "TZ")))
    (unwind-protect
        (progn
          (set-time-zone-rule "UTC")
          (drover-opencode-test-with
              (list (drover-opencode-test-entry "ses_1" :running t
                                                :retry '(:attempt 3 :at 1800000000.0 :message "Overloaded")))
            (should (equal (drover-session-detail (drover-opencode-test-session "ses_1"))
                           "retry 3 at 08:00: Overloaded"))))
      (set-time-zone-rule zone))))

(ert-deftest drover-opencode-test-status-failed ()
  (drover-opencode-test-with
      (list (drover-opencode-test-entry "ses_1" :idle 70.0 :viewed 60.0 :outcome 'failed
                                        :error "Provider returned 500"))
    (let ((session (drover-opencode-test-session "ses_1")))
      (should (eq (drover-session-status session) 'failed))
      (should (eql (drover-session-since session) 70.0))
      (should (equal (drover-session-detail session) "Provider returned 500")))))

(ert-deftest drover-opencode-test-status-done ()
  (drover-opencode-test-with
      (list (drover-opencode-test-entry "ses_1" :idle 70.0 :outcome 'succeeded
                                        :error "An older failure"))
    (let ((session (drover-opencode-test-session "ses_1")))
      (should (eq (drover-session-status session) 'done))
      (should (eql (drover-session-since session) 70.0))
      ;; The error belongs to an earlier turn.
      (should-not (drover-session-detail session)))))

(ert-deftest drover-opencode-test-status-idle ()
  (drover-opencode-test-with
      (list (drover-opencode-test-entry "ses_1" :idle 70.0 :viewed 70.0 :outcome 'failed :error "Seen"))
    (let ((session (drover-opencode-test-session "ses_1")))
      (should (eq (drover-session-status session) 'idle))
      (should-not (drover-session-since session))
      (should-not (drover-session-detail session)))))

(ert-deftest drover-opencode-test-every-entry-in-order ()
  (drover-opencode-test-with (list (drover-opencode-test-entry "ses_1")
                                   (drover-opencode-test-entry "ses_2")
                                   (drover-opencode-test-entry "ses_3"))
    (should (equal (mapcar #'drover-session-id (drover-sessions)) '("ses_1" "ses_2" "ses_3")))))

(ert-deftest drover-opencode-test-records-made-once ()
  "A session listed again of the same inputs is the record made before."
  (let ((entries (list (drover-opencode-test-entry "ses_1" :agent "build")
                       (drover-opencode-test-entry "ses_2" :directory "/work/opencode/"))))
    (drover-opencode-test-with entries
      (let ((made 0))
        (cl-letf* ((make (symbol-function 'drover-opencode--session))
                   ((symbol-function 'drover-opencode--session)
                    (lambda (entry) (cl-incf made) (funcall make entry))))
          (let ((first (drover-opencode--sessions)))
            (should (= made 2))
            (should (equal (mapcar #'drover-opencode--listed entries) first))
            (cl-flet ((again ()
                        (let ((records (drover-opencode--sessions)))
                          (prog1 (list (eq (nth 0 records) (nth 0 first))
                                       (eq (nth 1 records) (nth 1 first)))
                            (setq first records)))))
              (should (equal (again) '(t t)))
              (should (= made 2))
              ;; By id, as a change reported.
              (should (eq (drover-opencode--session-by-id "ses_2") (nth 1 first)))
              ;; The registry's entry replaced: that session alone.
              (setcar drover-opencode-test-entries
                      (drover-opencode-test-entry "ses_1" :agent "build" :title "Renamed"))
              (should (equal (again) '(nil t)))
              (should (equal (drover-session-title (nth 0 first)) "Renamed"))
              ;; Replaced by an equal entry, it is the same.
              (setcar drover-opencode-test-entries
                      (drover-opencode-test-entry "ses_1" :agent "build" :title "Renamed"))
              (should (equal (again) '(t t)))
              ;; Its prompt cache, its branch.
              (let ((drover-opencode-test-caches '(("ses_1" :expires 1000.0 :ttl 300.0))))
                (should (equal (again) '(nil t))))
              (should (equal (again) '(nil t)))
              (let ((drover-opencode-test-branches '(("/work/drover/" . "main") ("/work/opencode/" . "next"))))
                (should (equal (again) '(t nil)))
                (should (equal (drover-session-branch (nth 1 first)) "next")))
              (should (equal (again) '(t nil)))
              ;; The projects fetched, and the catalogs, for every session.
              (let ((drover-opencode--projects (list '((id . "prj_drover") (name . "Drover")))))
                (should (equal (again) '(nil nil))))
              (should (equal (again) '(nil nil)))
              (let ((opencode-catalog-generation (1+ opencode-catalog-generation)))
                (should (equal (again) '(nil nil))))
              (should (equal (again) '(nil nil)))
              (should (equal (again) '(t t)))
              ;; Gone from the registry, it is forgotten.
              (setq drover-opencode-test-entries (list (car drover-opencode-test-entries)))
              (drover-opencode--sessions)
              (should (equal (hash-table-keys (cdr drover-opencode--made)) '("ses_1")))
              ;; Another version of the records: all made anew.
              (setcar drover-opencode--made '((drover-session . stale)))
              (setq made 0)
              (drover-opencode--sessions)
              (should (= made 1)))))))))

(ert-deftest drover-opencode-test-registry-is-authoritative ()
  (drover-opencode-test-with (list (drover-opencode-test-entry "ses_1" :title "Before"))
    (should (equal (drover-session-title (drover-opencode-test-session "ses_1")) "Before"))
    (setq drover-opencode-test-entries (list (drover-opencode-test-entry "ses_1" :title "After" :running t)
                                             (drover-opencode-test-entry "ses_2")))
    (should (equal (drover-session-title (drover-opencode-test-session "ses_1")) "After"))
    (should (eq (drover-session-status (drover-opencode-test-session "ses_1")) 'working))
    (should (drover-opencode-test-session "ses_2"))
    (setq drover-opencode-test-entries nil)
    (should-not (drover-sessions))))

(ert-deftest drover-opencode-test-session-by-id ()
  "A session reported changed is read from its registry entry alone, as the registry lists it."
  (drover-opencode-test-with (list (drover-opencode-test-entry "ses_1" :title "One")
                                   (drover-opencode-test-entry "ses_2" :title "Two"))
    (drover-index)
    ;; Registering reported the backend changed.
    (drover-changed-now)
    (let ((listed 0))
      (cl-letf* ((sessions (symbol-function 'opencode-state-sessions))
                 ((symbol-function 'opencode-state-sessions)
                  (lambda (&rest arguments) (cl-incf listed) (apply sessions arguments))))
        (setq drover-opencode-test-entries (list (drover-opencode-test-entry "ses_1" :title "Renamed")
                                                 (drover-opencode-test-entry "ses_2" :title "Two")))
        (drover-opencode--updated "ses_1")
        (drover-changed-now)
        (should (zerop listed))
        (should (equal (drover-session-title (drover-index-session '(opencode . "ses_1"))) "Renamed"))
        ;; Archived, it leaves the sessions listed.
        (setq drover-opencode-test-entries (list (drover-opencode-test-entry "ses_1" :archived 1.0)
                                                 (drover-opencode-test-entry "ses_2" :title "Two")))
        (drover-opencode--updated "ses_1")
        (drover-changed-now)
        (should-not (drover-index-session '(opencode . "ses_1")))
        (should (equal (mapcar #'drover-session-key (drover-index-sessions))
                       (mapcar #'drover-session-key (drover-sessions))))
        ;; A session the registry no longer holds is gone.
        (should-not (drover-opencode--session-by-id "ses_3"))))))

(ert-deftest drover-opencode-test-model-in-session-location ()
  "Catalogs belong to a location: a session's own, else the current directory."
  (drover-opencode-test-with
      (list (drover-opencode-test-entry "ses_1" :directory "/work/opencode/" :model '((id . "gpt")))
            (drover-opencode-test-entry "ses_2" :directory nil :model '((id . "gpt"))))
    (should (equal (drover-session-model (drover-opencode-test-session "ses_1")) "gpt in /work/opencode/"))
    (let ((default-directory "/elsewhere/"))
      (should (equal (drover-session-model (drover-opencode-test-session "ses_2")) "gpt in /elsewhere/")))))

(ert-deftest drover-opencode-test-default-effort ()
  "An effort the session did not choose is the model's default, which lists dim."
  (drover-opencode-test-with
      (list (drover-opencode-test-entry "ses_1" :model '((id . "gpt") (providerID . "openai")))
            (drover-opencode-test-entry "ses_2" :model '((id . "gpt") (providerID . "openai")
                                                         (variant . "default")))
            (drover-opencode-test-entry "ses_3" :model '((id . "gpt") (providerID . "openai")
                                                         (variant . "low"))))
    (dolist (id '("ses_1" "ses_2"))
      (let ((session (drover-opencode-test-session id)))
        (should (equal (drover-session-effort session) "medium"))
        (should (drover-session-effort-default session))))
    (let ((session (drover-opencode-test-session "ses_3")))
      (should (equal (drover-session-effort session) "low"))
      (should-not (drover-session-effort-default session)))
    ;; A provider without a catalog name goes by its identifier.
    (should (equal (drover-session-provider-name (drover-opencode-test-session "ses_1")) "openai"))))

(defconst drover-opencode-test--displays
  (mapcar (lambda (function) (cons function (symbol-function function)))
          '(opencode-model-display opencode-variant-display opencode-provider-display
                                   opencode-provider-info opencode-context-usage
                                   opencode-agent-display))
  "The displays of opencode.el, as loaded, before tests stub them.")

(defvar drover-opencode-test--catalog-changes 0
  "How many times tests changed opencode.el's catalogs.")

(defmacro drover-opencode-test-with-catalogs (catalogs &rest body)
  "Run BODY with opencode.el's real displays reading CATALOGS.
CATALOGS is non-nil for the fixture catalogs to be cached, nil for
none: opencode.el's public catalog functions are stubbed, so that
nothing is fetched."
  (declare (indent 1) (debug t))
  `(let ((cached ,catalogs)
         (stubs (mapcar (lambda (entry) (cons (car entry) (symbol-function (car entry))))
                        drover-opencode-test--displays))
         ;; Catalogs arriving or forgotten change their generation.
         (opencode-catalog-generation (cl-incf drover-opencode-test--catalog-changes)))
     (unwind-protect
         (progn
           (dolist (entry drover-opencode-test--displays)
             (fset (car entry) (cdr entry)))
           (cl-letf (((symbol-function 'opencode-catalog-ensure) #'ignore)
                     ((symbol-function 'opencode-catalog-cached-p) (lambda (&rest _) cached))
                     ((symbol-function 'opencode-catalog)
                      (lambda (kind &optional _directory)
                        (and cached (pcase kind
                                      ('model drover-opencode-fixtures-models)
                                      ('provider drover-opencode-fixtures-providers)
                                      ('agent drover-opencode-fixtures-agents))))))
             ,@body))
       (dolist (entry stubs)
         (fset (car entry) (cdr entry))))))

(ert-deftest drover-opencode-test-real-displays ()
  "A session shows what it runs as opencode.el's mode line does, once catalogs arrive."
  (let ((entries (list (drover-opencode-test-entry "ses_1" :context 190000 :agent "build"
                                                   :model '((id . "claude-opus-5-5") (providerID . "anthropic")
                                                            (variant . "max")))
                       (drover-opencode-test-entry "ses_2" :context 8400 :agent "plan"
                                                   :model '((id . "gpt-5.5") (providerID . "openai")))
                       (drover-opencode-test-entry "ses_3" :context 1000
                                                   :model '((id . "retired") (providerID . "acme"))))))
    (drover-opencode-test-with entries
      (drover-opencode-test-with-catalogs t
        (let ((opus (drover-opencode-test-session "ses_1"))
              (gpt (drover-opencode-test-session "ses_2"))
              (retired (drover-opencode-test-session "ses_3")))
          ;; Compact, as the mode line names it.
          (should (equal (drover-session-model opus) "Opus 5.5"))
          (should (equal (drover-session-effort opus) "max"))
          (should-not (drover-session-effort-default opus))
          (should (equal (drover-session-provider opus) "\uec82"))
          (should-not (text-properties-at 0 (drover-session-provider opus)))
          (should (equal (drover-session-provider-name opus) "Anthropic"))
          ;; Past 90% of the 200k tier of its pricing, of a 1M context.
          (should (eql (drover-session-context-tokens opus) 190000))
          (should (eql (drover-session-context-limit opus) 1000000))
          (should (equal (drover-session-context-tier opus) '(near 200000 "2×")))
          ;; The effort a request without one sends.
          (should (equal (drover-session-model gpt) "GPT-5.5"))
          (should (equal (drover-session-effort gpt)
                         (symbol-name (drover-model-default-variant
                                       (drover-opencode-fixtures-model "openai" "gpt-5.5")))))
          (should (drover-session-effort-default gpt))
          (should (equal (drover-session-provider gpt) "\uec81"))
          (should (equal (drover-session-provider-name gpt) "OpenAI"))
          (should-not (drover-session-context-limit gpt))
          ;; A model the catalog lost goes by its identifier; its
          ;; provider by a generic icon and its identifier.
          (should (equal (drover-session-model retired) "retired"))
          (should-not (drover-session-effort retired))
          (should (equal (drover-session-provider retired) "\U000F048D acme"))
          (should (equal (drover-session-provider-name retired) "acme"))
          ;; The agent as the mode line draws it: its catalog name, bold,
          ;; in its colour, else a face of its own among the primary ones.
          (dolist (session (list opus gpt))
            (should (equal-including-properties
                     (drover-session-agent-display session)
                     (opencode-agent-display (drover-session-agent session) "/work/drover/"))))
          (should (equal (drover-session-agent-display opus) "Build"))
          (should (equal (get-text-property 0 'face (drover-session-agent-display opus))
                         (list 'bold (nth 0 opencode-mode-line-agent-faces))))
          (should (equal (get-text-property 0 'face (drover-session-agent-display gpt))
                         (list 'bold (nth 1 opencode-mode-line-agent-faces))))
          ;; Views still group and filter by the agent's identifier.
          (should (equal (drover-session-agent opus) "build"))
          (should (equal (drover-agent-label opus)
                         (concat (propertize "@" 'face (list 'bold (nth 0 opencode-mode-line-agent-faces)))
                                 "Build")))
          (should-not (drover-session-agent-display retired))))
      (drover-opencode-test-with-catalogs nil
        ;; Until the catalogs arrive: the identifiers, the effort chosen.
        (let ((opus (drover-opencode-test-session "ses_1"))
              (gpt (drover-opencode-test-session "ses_2")))
          (should (equal (drover-session-model opus) "claude-opus-5-5"))
          (should (equal (drover-session-effort opus) "max"))
          (should-not (drover-session-provider opus))
          (should (equal (drover-session-provider-name opus) "anthropic"))
          (should (eql (drover-session-context-tokens opus) 190000))
          (should-not (drover-session-context-limit opus))
          (should-not (drover-session-effort gpt))
          ;; The agent by its identifier, plain.
          (should (equal-including-properties (drover-session-agent-display opus) "build")))))))

(ert-deftest drover-opencode-test-catalog-changes-redraw ()
  "A catalog arriving or forgotten is a change: what sessions run reads otherwise."
  (drover-opencode-test-with (list (drover-opencode-test-entry "ses_1"))
    (drover-start)
    (should (memq #'drover-opencode--catalog-changed opencode-catalog-changed-functions))
    (let ((changes nil))
      (cl-letf (((symbol-function 'drover-backend-changed)
                 (lambda (&rest arguments) (push arguments changes))))
        (run-hook-with-args 'opencode-catalog-changed-functions 'model "/work/drover/")
        (run-hook-with-args 'opencode-catalog-changed-functions nil nil))
      (should (equal changes '((opencode) (opencode)))))))

(ert-deftest drover-opencode-test-prefetch ()
  "The context of the sessions a view shows is asked of the registry, which fetches it."
  (drover-opencode-test-with (list (drover-opencode-test-entry "ses_1")
                                   (drover-opencode-test-entry "ses_2" :context 5000)
                                   (drover-opencode-test-entry "ses_3"))
    (should (drover-supports-p (drover-opencode-test-session "ses_1") :prefetch))
    (drover-sessions)
    ;; Listing sessions asks nothing.
    (should-not (assq 'opencode-state-context drover-opencode-test-calls))
    (drover-prefetch (list (drover-opencode-test-session "ses_3") (drover-opencode-test-session "ses_1")))
    (should (equal (seq-filter (lambda (call) (eq (car call) 'opencode-state-context))
                               (reverse drover-opencode-test-calls))
                   '((opencode-state-context "ses_3") (opencode-state-context "ses_1"))))))

;;;; Projects

(ert-deftest drover-opencode-test-projects-fetched-on-start ()
  (drover-opencode-test-with
      (list (drover-opencode-test-entry "ses_1")
            (drover-opencode-test-entry "ses_2" :project-id nil :directory "/work/opencode-wt/sub/")
            (drover-opencode-test-entry "ses_3" :project-id "prj_gone" :directory "/work/opencode/")
            (drover-opencode-test-entry "ses_4" :project-id "global" :directory "/tmp/scratch/")
            (drover-opencode-test-entry "ses_5" :project-id nil :directory "/nowhere/"))
    (should-not (drover-session-project (drover-opencode-test-session "ses_1")))
    (drover-start)
    (should (= (length drover-opencode-test-fetches) 1))
    (should-not (drover-session-project (drover-opencode-test-session "ses_1")))
    (drover-opencode-test-answer-projects)
    (should (timerp drover--change-timer))
    (should (equal (drover-session-project (drover-opencode-test-session "ses_1")) "Drover"))
    ;; A sandbox belongs to its project.
    (should (equal (drover-session-project (drover-opencode-test-session "ses_2")) "opencode"))
    ;; An unknown project ID falls back to the directory.
    (should (equal (drover-session-project (drover-opencode-test-session "ses_3")) "opencode"))
    ;; The root project has no name.
    (should-not (drover-session-project (drover-opencode-test-session "ses_4")))
    (should-not (drover-session-project (drover-opencode-test-session "ses_5")))))

(ert-deftest drover-opencode-test-projects-with-real-lookup ()
  (let ((for-directory (symbol-function 'opencode-project-for-directory))
        (name (symbol-function 'opencode-project-name))
        (root (file-name-as-directory (make-temp-file "drover-opencode" t))))
    (unwind-protect
        (let* ((root (opencode-normalize-directory root))
               (inner (file-name-as-directory (expand-file-name "inner" root)))
               (projects `(((id . "outer") (canonical . ,(directory-file-name root)))
                           ((id . "inner") (name . "Inner") (canonical . ,(directory-file-name inner))))))
          (make-directory inner)
          (drover-opencode-test-with
              (list (drover-opencode-test-entry "ses_1" :project-id nil :directory (expand-file-name "src/" inner))
                    (drover-opencode-test-entry "ses_2" :project-id nil :directory root))
            (cl-letf (((symbol-function 'opencode-project-for-directory) for-directory)
                      ((symbol-function 'opencode-project-name) name))
              (drover-start)
              (drover-opencode-test-answer-projects projects)
              (should (equal (drover-session-project (drover-opencode-test-session "ses_1")) "Inner"))
              (should (equal (drover-session-project (drover-opencode-test-session "ses_2"))
                             (file-name-nondirectory (directory-file-name root)))))))
      (delete-directory root t))))

(ert-deftest drover-opencode-test-projects-cached ()
  (drover-opencode-test-with (list (drover-opencode-test-entry "ses_1"))
    (drover-start)
    (drover-opencode-test-answer-projects)
    (dotimes (_ 3)
      (should (equal (drover-session-project (drover-opencode-test-session "ses_1")) "Drover")))
    (should-not drover-opencode-test-fetches)))

(ert-deftest drover-opencode-test-projects-refreshed-on-update ()
  (drover-opencode-test-with (list (drover-opencode-test-entry "ses_1"))
    (drover-start)
    (drover-opencode-test-answer-projects)
    (funcall (drover-opencode-test-handler "project.updated")
             '((type . "project.updated") (data (id . "prj_drover") (name . "Renamed"))))
    (should (= (length drover-opencode-test-fetches) 1))
    ;; The previous names stay until the answer.
    (should (equal (drover-session-project (drover-opencode-test-session "ses_1")) "Drover"))
    (drover-opencode-test-answer-projects '(((id . "prj_drover") (name . "Renamed") (canonical . "/work/drover"))))
    (should (equal (drover-session-project (drover-opencode-test-session "ses_1")) "Renamed"))))

(ert-deftest drover-opencode-test-projects-taken-from-events ()
  "A project an event sends whole is taken without a request, which would start a process."
  (drover-opencode-test-with (list (drover-opencode-test-entry "ses_1"))
    (drover-start)
    (let ((handler (drover-opencode-test-handler "project.updated"))
          (changes 0))
      ;; Starting fetched them.
      (drover-opencode-test-answer-projects)
      (add-hook 'drover-changed-hook (lambda () (cl-incf changes)))
      (funcall handler '((type . "project.updated")
                         (data (id . "prj_drover") (name . "Renamed") (canonical . "/work/drover")
                               (sandboxes) (time (created . 1) (updated . 3) (active . 3)))))
      (should-not drover-opencode-test-fetches)
      (drover-changed-now)
      (should (= changes 1))
      (should (equal (drover-session-project (drover-opencode-test-session "ses_1")) "Renamed"))
      ;; Only its times changed: nothing to draw again.
      (let ((projects drover-opencode--projects))
        (funcall handler '((type . "project.updated")
                           (data (id . "prj_drover") (name . "Renamed") (canonical . "/work/drover")
                                 (sandboxes) (time (created . 1) (updated . 4) (active . 4)))))
        (should (eq drover-opencode--projects projects)))
      ;; A new project is added.
      (funcall handler '((type . "project.updated")
                         (data (id . "prj_new") (canonical . "/work/new") (sandboxes "/work/new-wt")
                               (time (created . 5) (updated . 5) (active . 5)))))
      (should-not drover-opencode-test-fetches)
      (should (equal (alist-get 'canonical (car (last drover-opencode--projects))) "/work/new"))
      (should (= (length drover-opencode--projects) 4))
      ;; One not whole, or one during a fetch, fetches.
      (funcall handler '((type . "project.updated") (data (id . "prj_drover") (sandboxes . 3))))
      (should (= (length drover-opencode-test-fetches) 1))
      (funcall handler '((type . "project.updated")
                         (data (id . "prj_drover") (name . "Late") (canonical . "/work/drover")
                               (sandboxes))))
      (should (= (length drover-opencode-test-fetches) 1))
      (should drover-opencode--projects-stale))))

(ert-deftest drover-opencode-test-projects-refreshed-on-reconnection ()
  (drover-opencode-test-with (list (drover-opencode-test-entry "ses_1"))
    (drover-start)
    (drover-opencode-test-answer-projects)
    (funcall (drover-opencode-test-handler "server.connected") '((type . "server.connected")))
    (should (= (length drover-opencode-test-fetches) 1))))

(ert-deftest drover-opencode-test-projects-fetches-coalesce ()
  (drover-opencode-test-with (list (drover-opencode-test-entry "ses_1"))
    (drover-start)
    (let ((handler (drover-opencode-test-handler "project.updated")))
      (funcall handler nil)
      (funcall handler nil)
      (should (= (length drover-opencode-test-fetches) 1))
      (drover-opencode-test-answer-projects '(((id . "prj_drover") (name . "Stale") (canonical . "/work/drover"))))
      ;; Changes during the fetch fetch once more.
      (should (= (length drover-opencode-test-fetches) 1))
      (drover-opencode-test-answer-projects)
      (should-not drover-opencode-test-fetches)
      (should-not drover-opencode--projects-fetching)
      (should (equal (drover-session-project (drover-opencode-test-session "ses_1")) "Drover")))))

(ert-deftest drover-opencode-test-projects-failure-keeps-names ()
  (drover-opencode-test-with (list (drover-opencode-test-entry "ses_1"))
    (drover-start)
    (drover-opencode-test-answer-projects)
    (funcall (drover-opencode-test-handler "project.updated") nil)
    (drover-opencode-test-fail-projects)
    (should-not drover-opencode--projects-fetching)
    (should (equal (drover-session-project (drover-opencode-test-session "ses_1")) "Drover"))
    ;; The next change fetches again.
    (funcall (drover-opencode-test-handler "project.updated") nil)
    (should (= (length drover-opencode-test-fetches) 1))))

(ert-deftest drover-opencode-test-projects-failure-retries-when-stale ()
  (drover-opencode-test-with nil
    (drover-start)
    (funcall (drover-opencode-test-handler "project.updated") nil)
    (drover-opencode-test-fail-projects)
    (should (= (length drover-opencode-test-fetches) 1))
    (drover-opencode-test-fail-projects)
    (should-not drover-opencode-test-fetches)
    (should-not drover-opencode--projects-fetching)))

(ert-deftest drover-opencode-test-projects-signal-resets ()
  (drover-opencode-test-with nil
    (cl-letf (((symbol-function 'opencode-fetch-projects) (lambda (&rest _) (error "No service"))))
      (should-error (drover-opencode--fetch-projects))
      (should-not drover-opencode--projects-fetching))
    (drover-opencode--fetch-projects)
    (should (= (length drover-opencode-test-fetches) 1))))

;;;; Starting and changes

(ert-deftest drover-opencode-test-start ()
  (drover-opencode-test-with nil
    (drover-start)
    (should (memq #'drover-opencode--updated opencode-state-updated-functions))
    (should (equal (mapcar #'car drover-opencode-test-handlers)
                   '("project.updated" "server.connected"
                     "session.archived" "session.unarchived" "session.deleted" "server.connected")))
    (should (eq (drover-opencode-test-handler "project.updated") #'drover-opencode--projects-changed))
    (let ((ensure (assq 'opencode-state-ensure drover-opencode-test-calls)))
      (should ensure)
      (should (functionp (cadr ensure)))
      ;; Readiness redraws, even with no session to report.
      (setq drover--change-timer nil)
      (funcall (cadr ensure))
      (should (timerp drover--change-timer)))
    (should (= (length drover-opencode-test-fetches) 1))
    ;; Started once.
    (drover-start)
    (should (= (length drover-opencode-test-fetches) 1))))

(ert-deftest drover-opencode-test-changes-forwarded ()
  (drover-opencode-test-with nil
    (drover-start)
    (let ((changes nil)
          (runs 0))
      (cl-letf (((symbol-function 'drover-backend-changed)
                 (lambda (&rest arguments) (push arguments changes))))
        (run-hook-with-args 'opencode-state-updated-functions "ses_1"))
      (should (equal changes '((opencode "ses_1"))))
      (add-hook 'drover-changed-hook (lambda () (cl-incf runs)))
      (run-hook-with-args 'opencode-state-updated-functions "ses_1")
      (run-hook-with-args 'opencode-state-updated-functions "ses_2")
      (drover-changed-now)
      (should (= runs 1)))))

(ert-deftest drover-opencode-test-buffer-changes-forwarded ()
  "A session buffer opening or closing is a change: lists tell open sessions."
  (drover-opencode-test-with (list (drover-opencode-test-entry "ses_1"))
    (drover-start)
    (should (memq #'drover-opencode--updated opencode-session-buffer-functions))
    (let ((changes nil)
          (buffer (generate-new-buffer " *opencode ses_1*")))
      (cl-letf (((symbol-function 'drover-backend-changed)
                 (lambda (&rest arguments) (push arguments changes))))
        ;; opencode.el runs the hook once it registered a new buffer.
        (with-current-buffer buffer
          (opencode-session-mode)
          (setq opencode-session-id "ses_1"))
        (puthash "ses_1" buffer opencode-session-buffers)
        (with-current-buffer buffer
          (run-hook-with-args 'opencode-session-buffer-functions "ses_1"))
        (should (equal changes '((opencode "ses_1"))))
        (should (eq (drover-session-buffer (drover-opencode-test-session "ses_1")) buffer))
        ;; Killed, the buffer unregisters itself, through opencode.el's
        ;; own cleanup.
        (setq changes nil)
        (kill-buffer buffer)
        (should (equal changes '((opencode "ses_1"))))
        (should-not (drover-session-buffer (drover-opencode-test-session "ses_1")))))))

(ert-deftest drover-opencode-test-unload ()
  (drover-test-with-engines
    (drover-opencode--register-engine)
    (drover-opencode-test-with nil
      (drover-start)
      (unwind-protect
          (should-not (drover-opencode-unload-function))
        ;; The tests that follow need what unloading undid.
        (drover-opencode--install))
      (should-not (drover-backends))
      (should-not (drover-engines))
      (should-not drover--model-sources)
      (should-not (memq #'drover-opencode--updated opencode-state-updated-functions))
      (should-not (memq #'drover-opencode--updated opencode-session-buffer-functions))
      (should-not (memq #'drover-opencode--catalog-changed opencode-catalog-changed-functions))
      (should (member '(opencode-events-unregister "project.updated" drover-opencode--projects-changed)
                      drover-opencode-test-calls))
      (should (member '(opencode-events-unregister "server.connected" drover-opencode--projects-changed)
                      drover-opencode-test-calls)))))

;;;; Actions

(ert-deftest drover-opencode-test-open ()
  "A session opens in the window drover selects, or in another one it chooses."
  (drover-opencode-test-with (list (drover-opencode-test-entry "ses_1"))
    (let ((session (drover-opencode-test-session "ses_1"))
          (buffer (generate-new-buffer " ses_1")))
      (unwind-protect
          (save-window-excursion
            (delete-other-windows)
            (let ((origin (selected-window)))
              (puthash "ses_1" buffer opencode-session-buffers)
              (drover-open session)
              (should (eq (selected-window) origin))
              (drover-open session t)
              (should-not (eq (selected-window) origin))
              (should (= (length (window-list nil 'nomini)) 2))
              ;; opencode.el shows it in the window selected, whichever.
              (should (equal (reverse drover-opencode-test-calls)
                             '((opencode-session-open-by-id "ses_1" :other-window nil)
                               (opencode-session-open-by-id "ses_1" :other-window nil))))))
        (kill-buffer buffer)))))

(ert-deftest drover-opencode-test-open-fetches-unopened-session ()
  (drover-opencode-test-with (list (drover-opencode-test-entry "ses_1"))
    (let ((session (drover-opencode-test-session "ses_1"))
          (drover-opencode-test-fetched nil)
          (origin (selected-window)))
      (drover-open session)
      (should (equal drover-opencode-test-calls
                     `((opencode-session-fetch "ses_1" ,(drover-session-directory session)))))
      (funcall drover-opencode-test-fetched '((id . "ses_1")))
      (should (equal (car drover-opencode-test-calls)
                     `(opencode-session-open "ses_1" ,origin :other-window nil))))))

(ert-deftest drover-opencode-test-open-shows-from-the-asking-window ()
  "A session fetched first shows in the window chosen for it, wherever the user went."
  (drover-opencode-test-with (list (drover-opencode-test-entry "ses_1"))
    (save-window-excursion
      (delete-other-windows)
      (let* ((session (drover-opencode-test-session "ses_1"))
             (drover-opencode-test-fetched nil)
             (origin (selected-window))
             (other (split-window)))
        (drover-open session t)
        (should (eq (selected-window) other))
        ;; The user moved back before the session arrived.
        (select-window origin)
        (funcall drover-opencode-test-fetched '((id . "ses_1")))
        (should (equal (car drover-opencode-test-calls)
                       `(opencode-session-open "ses_1" ,other :other-window nil)))
        (should (eq (selected-window) origin))))))

(ert-deftest drover-opencode-test-buffer ()
  (drover-opencode-test-with (list (drover-opencode-test-entry "ses_1"))
    (let ((session (drover-opencode-test-session "ses_1"))
          (buffer (generate-new-buffer " ses_1")))
      (should-not (drover-session-buffer session))
      (puthash "ses_1" buffer opencode-session-buffers)
      (should (eq (drover-session-buffer session) buffer))
      (kill-buffer buffer)
      (should-not (drover-session-buffer session)))))

(ert-deftest drover-opencode-test-actions ()
  (drover-opencode-test-with (list (drover-opencode-test-entry "ses_1"))
    (let ((session (drover-opencode-test-session "ses_1")))
      (drover-respond session)
      (drover-interrupt session)
      (drover-archive session)
      (drover-mark-seen session)
      (should (equal (reverse drover-opencode-test-calls)
                     '((opencode-respond-session "ses_1" nil t)
                       (opencode-session-interrupt "ses_1")
                       (opencode-session-archive "ses_1")
                       (opencode-session-view "ses_1")))))))

(ert-deftest drover-opencode-test-respond-subagent ()
  "Answering a parent asking nothing answers its subagent's oldest request, in place."
  (drover-opencode-test-with
      (list (drover-opencode-test-entry "ses_parent" :running t)
            (drover-opencode-test-entry "ses_mid" :parent-id "ses_parent" :running t)
            (drover-opencode-test-entry "ses_deep" :parent-id "ses_mid" :running t
                                        :requests '((:kind question :id "frm_1" :summary "Color?" :time 100.0)))
            (drover-opencode-test-entry "ses_near" :parent-id "ses_parent" :running t
                                        :requests '((:kind permission :id "per_1" :summary "bash" :time 200.0))))
    (should (equal (drover-session-id (drover-session-to-answer (drover-opencode-test-session "ses_parent")))
                   "ses_deep"))
    (drover-respond (drover-opencode-test-session "ses_parent"))
    (drover-respond (drover-opencode-test-session "ses_near"))
    (should (equal (reverse drover-opencode-test-calls)
                   '((opencode-respond-session "ses_deep" nil t)
                     (opencode-respond-session "ses_near" nil t))))))

(ert-deftest drover-opencode-test-preview ()
  (drover-opencode-test-with (list (drover-opencode-test-entry "ses_1"))
    (let ((text nil))
      (drover-preview (drover-opencode-test-session "ses_1") (lambda (reply) (setq text reply)))
      (should (equal text "Last reply of ses_1"))
      (should (equal drover-opencode-test-calls '((opencode-session-last-reply "ses_1")))))))

(ert-deftest drover-opencode-test-recap ()
  "The recap of a session is opencode.el's last turn of it."
  (drover-opencode-test-with (list (drover-opencode-test-entry "ses_1"))
    (let ((recap nil))
      (drover-recap (drover-opencode-test-session "ses_1") (lambda (value) (setq recap value)))
      (should (equal recap '(:prompt "Prompt of ses_1" :reply "Done" :changes (("a.el" 3 1)))))
      (should (equal drover-opencode-test-calls '((opencode-session-last-turn "ses_1")))))))

(ert-deftest drover-opencode-test-new ()
  (drover-opencode-test-with nil
    (funcall (drover-backend-property 'opencode :new) "/work/drover/")
    (should (equal drover-opencode-test-calls
                   '((opencode-session-create :directory "/work/drover/"))))))

;;;; New operations

(ert-deftest drover-opencode-test-new-operations ()
  (drover-opencode-test-with (list (drover-opencode-test-entry "ses_1"))
    (let ((session (drover-opencode-test-session "ses_1")))
      (drover-rename session "Renamed")
      (drover-delete session)
      (drover-fork session)
      (drover-compact session)
      (should (equal (reverse drover-opencode-test-calls)
                     '((opencode-rename-session "ses_1" "Renamed")
                       (opencode-session-delete "ses_1")
                       (opencode-session-fork "ses_1")
                       (opencode-session-compact "ses_1")))))))

;;;; Session buffers

(defvar drover-opencode-test-prompt-actions
  '(send clear attach slash mention revert revert-cancel copy-block agent model effort directory
         compact fork last-answer open-link schedule)
  "The prompt actions OpenCode session buffers define.")

(ert-deftest drover-opencode-test-session-cache ()
  "A session carries its prompt cache from the registry: expiry, lifetime and tokens."
  (drover-opencode-test-with (list (drover-opencode-test-entry "ses_1") (drover-opencode-test-entry "ses_2"))
    (let ((drover-opencode-test-caches '(("ses_1" :expires 1000.0 :ttl 300.0 :tokens 42
                                          :prompt-tokens 50))))
      (let ((cached (drover-opencode-test-session "ses_1"))
            (bare (drover-opencode-test-session "ses_2")))
        (should (= (drover-session-cache-expires cached) 1000.0))
        (should (= (drover-session-cache-ttl cached) 300.0))
        (should (= (drover-session-cache-tokens cached) 42))
        (should (= (drover-session-cache-prompt-tokens cached) 50))
        (should-not (drover-session-cache-expires bare))
        (should-not (drover-session-cache-ttl bare))
        (should-not (drover-session-cache-tokens bare))
        (should-not (drover-session-cache-prompt-tokens bare))))))

(ert-deftest drover-opencode-test-session-buffer ()
  (drover-opencode-test-with (list (drover-opencode-test-entry "ses_1"))
    (with-temp-buffer
      (opencode-session-mode)
      (should (eq drover-session-at-point-function #'drover-opencode--buffer-session))
      (should (equal (sort (mapcar #'car drover-prompt-actions) #'string<)
                     (sort (copy-sequence drover-opencode-test-prompt-actions) #'string<)))
      (should (eq (alist-get 'send drover-prompt-actions) #'opencode-send-input))
      (should (eq (alist-get 'effort drover-prompt-actions) #'opencode-select-variant))
      (dolist (entry drover-prompt-actions)
        (should (symbolp (cdr entry))))
      ;; Before the session is known, there is none.
      (should-not (drover-session-at-point t))
      (setq opencode-session-id "ses_1")
      (should (equal (drover-session-at-point) (drover-opencode-test-session "ses_1")))
      (should-not (drover-session-list-p)))))

(ert-deftest drover-opencode-test-session-buffer-quotes-line ()
  "In a session buffer, the prompt quotes the line at point, never the session."
  (require 'drover-prompt)
  (drover-opencode-test-with (list (drover-opencode-test-entry "ses_1"))
    (let ((drover-prompt-buffer-name "*drover opencode quote test*")
          (drover-prompt-require-features nil)
          (org-mode-hook nil)
          (recaps nil))
      (unwind-protect
          (cl-letf (((symbol-function 'drover-prompt--display) #'ignore)
                    ((symbol-function 'drover-prompt--fetch-defaults) #'ignore)
                    ((symbol-function 'drover-prompt--schedule-refresh) #'ignore)
                    ((symbol-function 'drover-recap)
                     (lambda (session _callback) (push (drover-session-id session) recaps))))
            (with-temp-buffer
              (opencode-session-mode)
              (setq opencode-session-id "ses_1")
              (let ((inhibit-read-only t))
                (insert "An answer of the agent, quoted whole\n"))
              (goto-char 4)
              (should (drover-session-at-point))
              (drover-prompt-dwim)
              (should (string-match-p "\n#\\+begin_src markdown\nAn answer of the agent, quoted whole\n#\\+end_src\n"
                                      (with-current-buffer drover-prompt-buffer-name (buffer-string))))
              (should-not recaps)))
        (when-let* ((buffer (get-buffer drover-prompt-buffer-name)))
          (kill-buffer buffer))))))

(ert-deftest drover-opencode-test-session-buffer-quotes-sent-prompt ()
  "A sent prompt's line is quoted whole, its \"> \" too, from anywhere on it.
opencode.el writes the prompt as Comint output, a field of its own,
and the input after it, which `line-beginning-position' and
`line-end-position' stop at: from the start of the line, the quote
held \">\" alone.  The transcript is written by opencode.el's own
rendering of a prompt and of output."
  (require 'drover-prompt)
  (drover-opencode-test-with (list (drover-opencode-test-entry "ses_1"))
    (let ((drover-prompt-buffer-name "*drover opencode quote test*")
          (drover-prompt-require-features nil)
          (org-mode-hook nil)
          (prompt "Please investigate: https://example.slack.com/archives/C1/p2"))
      (unwind-protect
          (cl-letf (((symbol-function 'drover-prompt--display) #'ignore)
                    ((symbol-function 'drover-prompt--fetch-defaults) #'ignore)
                    ((symbol-function 'drover-prompt--schedule-refresh) #'ignore))
            (with-temp-buffer
              (opencode-session-mode)
              (setq opencode-session-id "ses_1")
              (let ((process (opencode--make-process (current-buffer)))
                    (name (buffer-name)))
                (unwind-protect
                    (progn
                      (opencode--render-user-prompt prompt)
                      (opencode-output "An answer of the agent\n")
                      (goto-char (point-min))
                      (should (looking-at-p (regexp-quote (concat "> " prompt "\n"))))
                      (should (eq (get-text-property 1 'field) 'output))
                      (should-not (get-text-property 3 'field))
                      (dolist (position (list 1 2 3 20 (pos-eol)))
                        (goto-char position)
                        (drover-prompt-dwim)
                        (with-current-buffer drover-prompt-buffer-name
                          (should (equal (cons position (buffer-string))
                                         (cons position
                                               (format "From buffer %s line 1:\n#+begin_src markdown\n> %s\n#+end_src\n\n"
                                                       name prompt))))
                          (erase-buffer))))
                  (delete-process process)))))
        (when-let* ((buffer (get-buffer drover-prompt-buffer-name)))
          (kill-buffer buffer))))))

(ert-deftest drover-opencode-test-session-buffer-outside-registry ()
  "A session the registry does not hold is described by its buffer."
  (drover-opencode-test-with nil
    (with-temp-buffer
      (opencode-session-mode)
      (setq opencode-session-id "ses_old"
            opencode-session-info '((id . "ses_old") (title . "Old one") (parentID . "ses_0"))
            default-directory "/work/drover/")
      (let ((session (drover-session-at-point)))
        (should (eq (drover-session-backend session) 'opencode))
        (should (equal (drover-session-id session) "ses_old"))
        (should (equal (drover-session-engine-key session) '(opencode . "ses_old")))
        (should (equal (drover-session-title session) "Old one"))
        (should (equal (drover-session-directory session) "/work/drover/"))
        (should (equal (drover-session-parent session) "ses_0"))
        (should (eq (drover-session-status session) 'idle))
        (drover-rename session "New")
        (should (equal (car drover-opencode-test-calls) '(opencode-rename-session "ses_old" "New")))))))

(ert-deftest drover-opencode-test-prompt-actions-run ()
  (drover-opencode-test-with (list (drover-opencode-test-entry "ses_1"))
    (with-temp-buffer
      (opencode-session-mode)
      (setq opencode-session-id "ses_1")
      (let ((ran nil))
        (cl-letf (((symbol-function 'opencode-revert-to-point) (lambda () (interactive) (push 'revert ran)))
                  ((symbol-function 'opencode-select-model) (lambda () (interactive) (push 'model ran))))
          (call-interactively #'drover-prompt-revert)
          (call-interactively #'drover-prompt-model))
        (should (equal ran '(model revert))))
      ;; Compacting goes through the service function.
      (call-interactively #'drover-compact)
      (should (equal (car drover-opencode-test-calls) '(opencode-session-compact "ses_1"))))))

(ert-deftest drover-opencode-test-directory-action ()
  "Moving a session is `cd' in its buffer, which opencode.el forwards."
  (drover-opencode-test-with (list (drover-opencode-test-entry "ses_1"))
    (let ((target (file-name-as-directory (make-temp-file "drover-cd" t))))
      (unwind-protect
          (with-temp-buffer
            (drover-opencode--setup-session-buffer)
            (cl-letf (((symbol-function 'read-directory-name) (lambda (&rest _) target)))
              (call-interactively #'drover-prompt-directory))
            (should (equal default-directory target)))
        (delete-directory target t)))))

(ert-deftest drover-opencode-test-teardown-keeps-other-actions ()
  (drover-opencode-test-with nil
    (with-temp-buffer
      (opencode-session-mode)
      (drover-prompt-define 'schedule #'ignore)
      (drover-prompt-define 'send #'ignore)
      (drover-opencode--teardown-session-buffer)
      ;; Actions others defined stay, even over the backend's.
      (should (equal drover-prompt-actions '((send . ignore) (schedule . ignore))))
      (should-not drover-session-at-point-function))))

;;;; Clearing the prompt

(ert-deftest drover-opencode-test-clear-input ()
  "Clearing erases what follows the process mark, undoably."
  (drover-opencode-test-with nil
    (let* ((buffer (generate-new-buffer "*drover-opencode-test-clear*"))
           (process nil))
      (unwind-protect
          (with-current-buffer buffer
            (opencode-session-mode)
            (setq process (make-pipe-process :name "drover-test-prompt" :buffer buffer :noquery t))
            (let ((inhibit-read-only t))
              (insert "Earlier answer\n"))
            (set-marker (process-mark process) (point))
            (insert "A prompt being written")
            (buffer-enable-undo)
            (undo-boundary)
            (should (eq (alist-get 'clear drover-prompt-actions) #'drover-opencode--clear-input))
            (call-interactively #'drover-prompt-clear)
            (should (equal (buffer-string) "Earlier answer\n"))
            (primitive-undo 1 buffer-undo-list)
            (should (equal (buffer-string) "Earlier answer\nA prompt being written"))
            ;; Nothing written: nothing to erase.
            (delete-region (process-mark process) (point-max))
            (call-interactively #'drover-prompt-clear)
            (should (equal (buffer-string) "Earlier answer\n")))
        (when (process-live-p process)
          (delete-process process))
        (kill-buffer buffer)))
    (with-temp-buffer
      (drover-opencode--setup-session-buffer)
      (should-error (call-interactively #'drover-prompt-clear) :type 'user-error))))

;;;; Older sessions

(defvar drover-opencode-test-history-fetch nil
  "Arguments of the last stubbed `opencode-fetch-sessions', as a plist.")

(defmacro drover-opencode-test-with-history (&rest body)
  "Run BODY with `opencode-fetch-sessions' stubbed, its call recorded."
  (declare (indent 0) (debug t))
  `(let ((drover-opencode-test-history-fetch nil))
     (cl-letf (((symbol-function 'opencode-fetch-sessions)
                (lambda (query callback &optional else)
                  (setq drover-opencode-test-history-fetch
                        (list :query query :callback callback :else else
                              :directory default-directory)))))
       ,@body)))

(ert-deftest drover-opencode-test-history ()
  (drover-opencode-test-with (list (drover-opencode-test-entry "ses_live" :running t :title "Live"))
    (drover-opencode-test-with-history
      (drover-start)
      (drover-opencode-test-answer-projects)
      (let ((received :none))
        (funcall (drover-backend-property 'opencode :history)
                 (lambda (sessions) (setq received sessions)))
        ;; Every unarchived session of every project.
        (should (equal (plist-get drover-opencode-test-history-fetch :query) '((archived . "false"))))
        (should (equal (plist-get drover-opencode-test-history-fetch :directory) (expand-file-name "~/")))
        (funcall (plist-get drover-opencode-test-history-fetch :callback)
                 '(((id . "ses_old") (title . "An old one") (projectID . "prj_drover")
                    (location (directory . "/work/drover/")) (agent . "build")
                    (model (id . "claude")) (parentID . "ses_parent")
                    (time (created . 1000000) (updated . 2000000) (idle . 2000000) (viewed . 2000000)))
                   ((id . "ses_unseen") (title . "Ended unseen") (outcome . "failed")
                    (location (directory . "/work/opencode/"))
                    (time (created . 3000000) (updated . 4000000) (idle . 4000000) (viewed . 1000)))
                   ((id . "ses_bare"))))
        (should (equal (mapcar #'drover-session-id received) '("ses_old" "ses_unseen" "ses_bare")))
        (let ((old (nth 0 received))
              (unseen (nth 1 received))
              (bare (nth 2 received)))
          (should (eq (drover-session-backend old) 'opencode))
          (should (equal (drover-session-engine-key old) '(opencode . "ses_old")))
          (should (equal (drover-session-title old) "An old one"))
          (should (equal (drover-session-directory old) "/work/drover/"))
          (should (equal (drover-session-project old) "Drover"))
          (should (equal (drover-session-branch old) "main"))
          (should (equal (drover-session-parent old) "ses_parent"))
          (should (equal (drover-session-agent old) "build"))
          (should (equal (drover-session-model old) "claude in /work/drover/"))
          ;; The registry has no context size of a session it does not hold.
          (should-not (drover-session-context-tokens old))
          (should (eql (drover-session-created old) 1000.0))
          (should (eql (drover-session-updated old) 2000.0))
          ;; Statuses are what the service tells: seen is idle.
          (should (eq (drover-session-status old) 'idle))
          (should (eq (drover-session-status unseen) 'failed))
          (should (eql (drover-session-since unseen) 4000.0))
          (should (equal (drover-session-project unseen) "opencode"))
          (should (eq (drover-session-status bare) 'idle))
          (should-not (drover-session-directory bare)))))))

(ert-deftest drover-opencode-test-history-in-lists ()
  "Older sessions join the live ones, which win."
  (drover-opencode-test-with (list (drover-opencode-test-entry "ses_live" :running t :title "Live"))
    (drover-opencode-test-with-history
      (let ((drover-show-history nil)
            (drover--history nil))
        (drover-set-show-history t)
        (funcall (plist-get drover-opencode-test-history-fetch :callback)
                 '(((id . "ses_live") (title . "Stale"))
                   ((id . "ses_old") (title . "Old"))))
        (should (equal (mapcar #'drover-session-title (drover-known-sessions)) '("Live" "Old")))
        (should (equal (drover-session-title (drover-find 'opencode "ses_old")) "Old"))
        (drover-set-show-history nil)))))

(ert-deftest drover-opencode-test-history-failure ()
  (drover-opencode-test-with nil
    (drover-opencode-test-with-history
      (let ((called nil)
            (messages nil))
        (funcall (drover-backend-property 'opencode :history) (lambda (_) (setq called t)))
        (cl-letf (((symbol-function 'message)
                   (lambda (format &rest arguments) (push (apply #'format format arguments) messages))))
          (funcall (plist-get drover-opencode-test-history-fetch :else)
                   (opencode-error--make :status 500 :method 'get :path "/api/session" :message "Down")))
        (should-not called)
        (should (string-match-p "older OpenCode sessions.*Down" (car messages)))))))

(ert-deftest drover-opencode-test-archived ()
  "Archived sessions come from the service on demand, telling when; one is restored."
  (drover-opencode-test-with (list (drover-opencode-test-entry "ses_live" :running t :title "Live"))
    (drover-opencode-test-with-history
      (drover-set-show-archived t)
      ;; Every archived session of every project, not waited for.
      (should (equal (plist-get drover-opencode-test-history-fetch :query) '((archived . "true"))))
      (should (equal (plist-get drover-opencode-test-history-fetch :directory) (expand-file-name "~/")))
      (should-not (drover-archived-sessions))
      (funcall (plist-get drover-opencode-test-history-fetch :callback)
               '(((id . "ses_gone") (title . "Put away") (location (directory . "/work/drover/"))
                  (time (created . 1000000) (updated . 2000000) (archived . 3000000)))))
      (let ((gone (drover-find 'opencode "ses_gone")))
        (should (eql (drover-session-archived gone) 3000.0))
        (should (drover-session-archived-p gone))
        (should (drover-unarchivable-p gone))
        (should-not (drover-archivable-p gone))
        ;; Listed with the live sessions, never among them.
        (should (equal (mapcar #'drover-session-id (drover-known-sessions)) '("ses_live" "ses_gone")))
        (should (equal (mapcar #'drover-session-id (drover-sessions)) '("ses_live")))
        (should (equal (mapcar #'car (drover-group-trees (drover-view-trees) 'status))
                       '(working archived)))
        ;; Z restores it on the service.
        (drover-unarchive gone)
        (should (equal (assq 'opencode-session-archive drover-opencode-test-calls)
                       '(opencode-session-archive "ses_gone" t))))
      ;; z archives a live one there, not in drover's archive.
      (drover-archive (drover-find 'opencode "ses_live"))
      (should (equal (assq 'opencode-session-archive drover-opencode-test-calls)
                     '(opencode-session-archive "ses_live")))
      (drover-set-show-archived nil)
      (should-not drover--archived))))

(defmacro drover-opencode-test-with-buffers (inputs &rest body)
  "Run BODY with session buffers of ses_1, ses_2... holding INPUTS unsent.
`buffers' is bound to them, in that order; each refuses to be killed
by asking, and is killed after BODY if it is still live."
  (declare (indent 1) (debug t))
  `(let ((buffers (seq-map-indexed (lambda (_input index)
                                     (generate-new-buffer (format "*drover-opencode-test-ses_%d*" (1+ index))))
                                   ,inputs)))
     (unwind-protect
         (progn
           (seq-map-indexed
            (lambda (input index)
              (let ((buffer (nth index buffers))
                    (id (format "ses_%d" (1+ index))))
                (with-current-buffer buffer
                  (opencode-session-mode)
                  (setq-local opencode-session-id id)
                  (let ((process (make-pipe-process :name "drover-test-archive" :buffer buffer
                                                    :noquery t)))
                    (let ((inhibit-read-only t))
                      (insert "An answer\n"))
                    (set-marker (process-mark process) (point))
                    (insert input))
                  (add-hook 'kill-buffer-query-functions (lambda () (error "Asked")) nil t))
                (puthash id buffer opencode-session-buffers)))
            ,inputs)
           ,@body)
       (dolist (buffer buffers)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (let ((kill-buffer-query-functions nil))
               (kill-buffer buffer))))))))

(ert-deftest drover-opencode-test-archive-closes-buffer ()
  "Archiving closes the session's buffer once the service took it, mid-turn too.
It asks nothing, and keeps the input typed; the report tells the buffer
closes then, and the close tells only of the input kept."
  (drover-opencode-test-with (list (drover-opencode-test-entry "ses_1" :running t)
                                   (drover-opencode-test-entry "ses_2"))
    (drover-opencode-test-with-buffers '("A follow-up half typed" "")
      (let ((kill-ring nil)
            (interprogram-cut-function nil)
            (messages nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (format &rest arguments)
                     (push (apply #'format-message format arguments) messages))))
          (should (eq (drover-session-status (drover-opencode-test-session "ses_1")) 'working))
          (drover-archive (drover-opencode-test-session "ses_1"))
          (drover-archive (drover-opencode-test-session "ses_2"))
          (should (equal (seq-filter (lambda (call) (eq (car call) 'opencode-session-archive))
                                     drover-opencode-test-calls)
                         '((opencode-session-archive "ses_2") (opencode-session-archive "ses_1"))))
          ;; Not before the service answered.
          (should (seq-every-p #'buffer-live-p buffers))
          (should (equal messages
                         '("Archived Title ses_2; its buffer closes once the archive is confirmed"
                           "Archived Title ses_1; its buffer closes once the archive is confirmed")))
          (setq messages nil)
          (drover-opencode-test-answer-archive "ses_2")
          (should-not (buffer-live-p (nth 1 buffers)))
          (should (buffer-live-p (nth 0 buffers)))
          (should-not messages)
          (drover-opencode-test-answer-archive "ses_1")
          (should-not (seq-some #'buffer-live-p buffers))
          (should (zerop (hash-table-count opencode-session-buffers)))
          (should (equal kill-ring '("A follow-up half typed")))
          (should (equal messages '("Archived Title ses_1; its unsent input is in the kill ring")))
          (should-not drover--closing)
          (should-not drover--confirming))))))

(ert-deftest drover-opencode-test-archive-refused-keeps-buffer ()
  "A refused archive leaves the buffer open, its input in place, and says why on one line."
  (drover-opencode-test-with (list (drover-opencode-test-entry "ses_1"))
    (drover-opencode-test-with-buffers '("Not sent yet")
      (let ((kill-ring nil)
            (warnings nil))
        (cl-letf (((symbol-function 'display-warning)
                   (lambda (type message &rest _) (push (list type message) warnings))))
          (drover-archive (drover-opencode-test-session "ses_1"))
          (drover-opencode-test-answer-archive "ses_1" (concat "Session is busy\n" (make-string 500000 ?x)))
          (should (buffer-live-p (car buffers)))
          (should (equal (with-current-buffer (car buffers)
                           (buffer-substring-no-properties
                            (process-mark (get-buffer-process (current-buffer))) (point-max)))
                         "Not sent yet"))
          (should-not kill-ring)
          (should (= (length warnings) 1))
          (pcase-let ((`(,type ,message) (car warnings)))
            (should (eq type 'drover))
            (should (string-prefix-p "Could not archive Title ses_1: PATCH /api/session/ses_1 failed" message))
            (should (string-suffix-p "; its buffer stays open" message))
            (should-not (string-search "\n" message))
            (should (< (length message) 400)))
          (should-not drover--confirming))))))

(ert-deftest drover-opencode-test-archive-restored-before-confirmed ()
  "A session restored before its archive was confirmed keeps its buffer."
  (drover-opencode-test-with (list (drover-opencode-test-entry "ses_1"))
    (drover-opencode-test-with-buffers '("")
      (let ((warnings nil))
        (cl-letf (((symbol-function 'display-warning)
                   (lambda (type message &rest _) (push (list type message) warnings))))
          (drover-archive (drover-opencode-test-session "ses_1"))
          (drover-unarchive (drover-opencode--session
                             (append '(:archived 1.0) (opencode-state-get "ses_1"))))
          (drover-opencode-test-answer-archive "ses_1")
          (drover-opencode-test-answer-archive "ses_1")
          (should (buffer-live-p (car buffers)))
          (should-not warnings)
          ;; Archived again, then killed by the user before the answer:
          ;; nothing to close, nothing told.
          (drover-archive (drover-opencode-test-session "ses_1"))
          (with-current-buffer (car buffers)
            (let ((kill-buffer-query-functions nil))
              (kill-buffer)))
          (drover-opencode-test-answer-archive "ses_1")
          (should-not warnings)
          (should-not drover--confirming))))))

(ert-deftest drover-opencode-test-archived-buffer-session ()
  "The buffer of a session the registry knows archived tells it is."
  (drover-opencode-test-with nil
    (cl-letf (((symbol-function 'opencode-state-get)
               (lambda (id) (and (equal id "ses_put") (list :id id :archived 3000.0)))))
      (should (eql (drover-session-archived (drover-opencode--session-of "ses_put" "Put away")) 3000.0))
      (should (drover-unarchivable-p (drover-opencode--session-of "ses_put")))
      (should-not (drover-session-archived (drover-opencode--session-of "ses_other"))))))

(ert-deftest drover-opencode-test-archived-failure ()
  (drover-opencode-test-with nil
    (drover-opencode-test-with-history
      (let ((called nil)
            (messages nil))
        (funcall (drover-backend-property 'opencode :archived) (lambda (_) (setq called t)))
        (cl-letf (((symbol-function 'message)
                   (lambda (format &rest arguments) (push (apply #'format format arguments) messages))))
          (funcall (plist-get drover-opencode-test-history-fetch :else)
                   (opencode-error--make :status 500 :method 'get :path "/api/session" :message "Down")))
        (should-not called)
        (should (string-match-p "archived OpenCode sessions.*Down" (car messages)))))))

(ert-deftest drover-opencode-test-archived-fetched-again ()
  "Archiving, restoring or deleting a session fetches the lists shown again, once a burst ends."
  (drover-opencode-test-with nil
    (drover-start)
    (let ((fetched nil))
      (cl-letf (((symbol-function 'drover-history-fetch) (lambda (&rest arguments) (push (cons 'history arguments) fetched)))
                ((symbol-function 'drover-archived-fetch) (lambda (&rest arguments) (push (cons 'archived arguments) fetched))))
        (dolist (type '("session.archived" "session.unarchived" "session.deleted" "server.connected"))
          (should (memq #'drover-opencode--sessions-moved
                        (mapcar #'cdr (seq-filter (lambda (entry) (equal (car entry) type))
                                                  drover-opencode-test-handlers)))))
        ;; Nothing shown, nothing fetched.
        (drover-opencode--sessions-moved '((type . "session.archived")))
        (should-not drover-opencode--refetch-timer)
        (let ((drover-show-archived t))
          (drover-opencode--sessions-moved '((type . "session.archived")))
          (let ((first drover-opencode--refetch-timer))
            (should (timerp first))
            (drover-opencode--sessions-moved '((type . "session.unarchived")))
            ;; A burst is fetched once, after the last event.
            (should-not (memq first timer-list))
            (should (timerp drover-opencode--refetch-timer)))
          (cancel-timer drover-opencode--refetch-timer)
          (drover-opencode--refetch)
          (should (equal fetched '((archived opencode))))
          (should-not drover-opencode--refetch-timer)
          (let ((drover-show-history t))
            (setq fetched nil)
            (drover-opencode--refetch)
            (should (equal fetched '((archived opencode) (history opencode))))))))))

;;;; The dispatch menu

(defun drover-opencode-test--archived-infos (&rest ids)
  "Return the infos of archived sessions IDS, as the service lists them."
  (mapcar (lambda (id)
            `((id . ,id) (title . ,(concat "Put away " id)) (location (directory . "/work/drover/"))
              (time (created . 1000000) (updated . 2000000) (archived . 3000000))))
          ids))

(ert-deftest drover-opencode-test-restored-leaves-archived ()
  "A session the registry tells restored, as a prompt sent to it restores it, leaves the archived ones.
The service's event fetches them again later; the registry tells first."
  (drover-opencode-test-with (list (drover-opencode-test-entry "ses_live"))
    (drover-opencode-test-with-history
      (drover-set-show-archived t)
      (funcall (plist-get drover-opencode-test-history-fetch :callback)
               (drover-opencode-test--archived-infos "ses_a" "ses_b" "ses_c"))
      (cl-letf (((symbol-function 'opencode-state-sessions)
                 ;; As the registry lists them: archived ones left out.
                 (lambda (&optional _)
                   (seq-remove (lambda (entry) (plist-get entry :archived)) drover-opencode-test-entries))))
        (cl-flet ((archived () (sort (mapcar #'drover-session-id (drover-archived-sessions)) #'string<))
                (registry (id archived)
                  (setq drover-opencode-test-entries
                        (append (seq-remove (lambda (entry) (equal (plist-get entry :id) id))
                                            drover-opencode-test-entries)
                                (list (drover-opencode-test-entry id :archived archived))))))
        (should (equal (archived) '("ses_a" "ses_b" "ses_c")))
        ;; A buffer opened on it, the registry holds it, archived still.
        (registry "ses_a" 3000.0)
        (drover-opencode--updated "ses_a")
        (should (equal (archived) '("ses_a" "ses_b" "ses_c")))
        ;; Restored: it leaves the archived sessions, and lists show it live.
        (registry "ses_a" nil)
        (drover-opencode--updated "ses_a")
        (should (equal (archived) '("ses_b" "ses_c")))
        (should (memq 'opencode drover--changed-backends))
        (should (equal (mapcar #'drover-session-id (drover-known-sessions)) '("ses_live" "ses_a" "ses_b" "ses_c")))
        (should-not (drover-session-archived (drover-find 'opencode "ses_a")))
        ;; Several changes told at once.
        (registry "ses_c" nil)
        (drover-opencode--updated nil)
        (should (equal (archived) '("ses_b")))
        ;; A session the registry does not hold tells nothing.
        (drover-opencode--updated "ses_b")
        (should (equal (archived) '("ses_b")))
        ;; Archived sessions hidden, nothing is looked at.
        (registry "ses_b" nil)
        (drover-set-show-archived nil)
        (setq drover--changed-backends nil)
        (drover-opencode--updated "ses_b")
        (should-not (memq 'opencode drover--changed-backends)))))))

(defun drover-opencode-test--as-registry ()
  "Return the entries `opencode-state-sessions' lists, as the registry does.
Archived sessions are left out, unless their buffer is open."
  (seq-remove (lambda (entry)
                (and (plist-get entry :archived)
                     (not (buffer-live-p (gethash (plist-get entry :id) opencode-session-buffers)))))
              drover-opencode-test-entries))

(defun drover-opencode-test--set-entry (id &rest properties)
  "Make the registry entry of ID that of `drover-opencode-test-entry' with PROPERTIES."
  (setq drover-opencode-test-entries
        (mapcar (lambda (entry)
                  (if (equal (plist-get entry :id) id) (apply #'drover-opencode-test-entry id properties) entry))
                drover-opencode-test-entries)))

(ert-deftest drover-opencode-test-archived-parent-takes-subagents ()
  "The subagents of a session archived alone go with it, into the archive.
The service archives a session alone: its subagents, unarchived, stayed
among the sessions in use, at the top of trees of their own.  They go
whether or not a buffer is open, as an archived session does; a parent
the registry does not hold, deleted or not fetched, archives nothing."
  (drover-opencode-test-with
      (list (drover-opencode-test-entry "ses_live")
            (drover-opencode-test-entry "ses_root" :archived 5.0)
            (drover-opencode-test-entry "ses_sub" :parent-id "ses_root")
            (drover-opencode-test-entry "ses_deep" :parent-id "ses_sub")
            (drover-opencode-test-entry "ses_orphan" :parent-id "ses_deleted")
            (drover-opencode-test-entry "ses_open" :archived 6.0)
            (drover-opencode-test-entry "ses_kept" :parent-id "ses_open"))
    (let ((buffer (generate-new-buffer " *drover-opencode-test-open*")))
      (unwind-protect
          (cl-letf (((symbol-function 'opencode-state-sessions)
                     (lambda (&optional _) (drover-opencode-test--as-registry))))
            (puthash "ses_open" buffer opencode-session-buffers)
            (puthash "ses_kept" buffer opencode-session-buffers)
            (drover-index)
            (drover-changed-now)
            (cl-flet ((listed () (sort (mapcar #'drover-session-id (drover-index-sessions)) #'string<)))
              (should (equal (listed) '("ses_live" "ses_orphan")))
              (should (equal (listed) (sort (mapcar #'drover-session-id (drover-sessions)) #'string<)))
              (dolist (id '("ses_sub" "ses_deep" "ses_kept"))
                (should-not (drover-opencode--session-by-id id)))
              (should (drover-opencode--session-by-id "ses_orphan"))
              ;; Restored, its subagents come back with it, told one by one.
              (drover-opencode-test--set-entry "ses_root")
              (drover-opencode--updated "ses_root")
              (drover-changed-now)
              (should (equal (listed) '("ses_deep" "ses_live" "ses_orphan" "ses_root" "ses_sub")))
              (should (equal (drover-index-parent '(opencode . "ses_sub")) '(opencode . "ses_root")))
              ;; Archived again, they leave with it.
              (drover-opencode-test--set-entry "ses_root" :archived 7.0)
              (drover-opencode--updated "ses_root")
              (drover-changed-now)
              (should (equal (listed) '("ses_live" "ses_orphan")))
              ;; Deleted, it leaves them at the top of trees of their own.
              (setq drover-opencode-test-entries
                    (seq-remove (lambda (entry) (equal (plist-get entry :id) "ses_root"))
                                drover-opencode-test-entries))
              (drover-opencode--updated "ses_root")
              (drover-changed-now)
              (should (equal (listed) '("ses_deep" "ses_live" "ses_orphan" "ses_sub")))
              (should-not (drover-index-parent '(opencode . "ses_sub")))
              (should (equal (listed) (sort (mapcar #'drover-session-id (drover-sessions)) #'string<)))))
        (kill-buffer buffer)))))

(ert-deftest drover-opencode-test-archived-subagents-in-archive ()
  "The subagents of an archived session show with it, among the archived ones, never older.
Lists show them under it, in the archived group."
  (drover-opencode-test-with
      (list (drover-opencode-test-entry "ses_live")
            (drover-opencode-test-entry "ses_root" :archived 3000.0)
            (drover-opencode-test-entry "ses_sub" :parent-id "ses_root"))
    (drover-opencode-test-with-history
      (cl-letf (((symbol-function 'opencode-state-sessions)
                 (lambda (&optional _) (drover-opencode-test--as-registry))))
        (let ((drover-show-subagents 'all))
          (drover-set-show-archived t)
          (funcall (plist-get drover-opencode-test-history-fetch :callback)
                   (drover-opencode-test--archived-infos "ses_root"))
          (should (equal (mapcar #'drover-session-id (drover-archived-sessions)) '("ses_root" "ses_sub")))
          (should (equal (mapcar #'drover-session-id (drover-known-sessions)) '("ses_live" "ses_root" "ses_sub")))
          (should (equal (mapcar (lambda (group) (cons (car group) (drover-test-tree-shape (cdr group))))
                                 (drover-group-trees (drover-view-trees) 'status))
                         '((idle ("ses_live")) (archived ("ses_root" ("ses_sub"))))))
          (drover-set-show-archived nil)
          (drover-set-show-history t)
          (funcall (plist-get drover-opencode-test-history-fetch :callback)
                   '(((id . "ses_sub") (parentID . "ses_root")) ((id . "ses_old"))))
          (should (equal (mapcar #'drover-session-id (drover-known-sessions)) '("ses_live" "ses_old")))
          (drover-set-show-history nil))))))

(ert-deftest drover-opencode-test-unarchive-confirmed ()
  "Restoring an archived session drops it from the archived ones once the service took it.
A refusal is told on one line, as a warning, the session staying archived."
  (drover-opencode-test-with nil
    (drover-opencode-test-with-history
      (drover-set-show-archived t)
      (funcall (plist-get drover-opencode-test-history-fetch :callback)
               (drover-opencode-test--archived-infos "ses_a" "ses_b"))
      (let ((warnings nil))
        (cl-letf (((symbol-function 'display-warning)
                   (lambda (_type message &rest _) (push message warnings))))
          (drover-unarchive (drover-find 'opencode "ses_a"))
          (should (equal (mapcar #'drover-session-id (drover-archived-sessions)) '("ses_a" "ses_b")))
          (drover-opencode-test-answer-archive "ses_a")
          (should (equal (mapcar #'drover-session-id (drover-archived-sessions)) '("ses_b")))
          (should-not warnings)
          (drover-unarchive (drover-find 'opencode "ses_b"))
          (drover-opencode-test-answer-archive "ses_b" (concat "Session is busy\n" (make-string 500000 ?x)))
          (should (equal (mapcar #'drover-session-id (drover-archived-sessions)) '("ses_b")))
          (should (= (length warnings) 1))
          (should (string-prefix-p "Could not restore Put away ses_b from the archive: PATCH /api/session/ses_b failed"
                                   (car warnings)))
          (should-not (string-search "\n" (car warnings)))
          (should (< (length (car warnings)) 400)))))))

(ert-deftest drover-opencode-test-dispatch ()
  (let ((keys (mapcar (lambda (key)
                        (plist-get (cdr (transient-get-suffix 'drover-dispatch key)) :command))
                      '("l" "L" "p" "S"))))
    (should (equal keys '(drover-list drover-list-side drover-select-project
                                      opencode-consult-sessions))))
  ;; After drover's lists, in their column.
  (should (equal (mapcar (lambda (child) (plist-get (cdr child) :key))
                         (aref (transient-get-suffix 'drover-dispatch '(0 0)) 2))
                 '("j" "B" "b" "l" "L" "p" "S")))
  ;; The keys opencode.el's lists had are gone.
  (dolist (key '("1" "2" "3"))
    (should-not (ignore-errors (transient-get-suffix 'drover-dispatch key)))))

(ert-deftest drover-opencode-test-command-keys ()
  "opencode.el's hints name the keys of drover's menu."
  (let ((map (make-sparse-keymap)))
    (keymap-set map "C-'" #'drover-dispatch)
    (let ((global-map map)
          (opencode-command-keys-function #'drover-opencode--command-keys))
      (with-temp-buffer
        (use-global-map map)
        (unwind-protect
            (progn
              (should (equal (drover-opencode--command-keys 'opencode-respond) "C-' A"))
              (should (equal (drover-opencode--command-keys 'opencode-interrupt) "C-' K"))
              (should (equal (drover-opencode--command-keys 'opencode-revert-clear) "C-' T"))
              (should-not (drover-opencode--command-keys 'opencode-new-worktree))
              (should (equal (substring-no-properties (opencode-command-keys 'opencode-respond)) "C-' A")))
          (use-global-map global-map)))))
  ;; Without a key for the menu, opencode.el falls back on its own keymaps.
  (let ((map (make-sparse-keymap)))
    (with-temp-buffer
      (let ((global-map map))
        (use-global-map map)
        (unwind-protect
            (should-not (drover-opencode--command-keys 'opencode-respond))
          (use-global-map global-map))))))

;;;; Installing and unloading

(ert-deftest drover-opencode-test-uninstall-and-install ()
  (drover-opencode-test-with nil
    (let ((session-buffer (generate-new-buffer " session")))
      (unwind-protect
          (progn
            (with-current-buffer session-buffer
              (opencode-session-mode)
              (drover-prompt-define 'schedule #'ignore))
            (drover-opencode--uninstall)
            (should-not opencode-command-keys-function)
            (should-not (memq #'drover-opencode--setup-session-buffer opencode-session-mode-hook))
            (with-current-buffer session-buffer
              (should-not drover-session-at-point-function)
              (should (equal drover-prompt-actions '((schedule . ignore)))))
            (should-not (ignore-errors (transient-get-suffix 'drover-dispatch "S")))
            (should (eq (plist-get (cdr (transient-get-suffix 'drover-dispatch "p")) :command)
                        'drover-select-project))
            (with-temp-buffer
              (opencode-session-mode)
              (should-not drover-prompt-actions))
            ;; Installing again sets up the buffers that exist.
            (drover-opencode--install)
            (with-current-buffer session-buffer
              (should (eq drover-session-at-point-function #'drover-opencode--buffer-session))
              (should (assq 'send drover-prompt-actions))
              (should (assq 'schedule drover-prompt-actions)))
            (should (memq #'drover-opencode--setup-session-buffer opencode-session-mode-hook))
            (should (eq opencode-command-keys-function #'drover-opencode--command-keys))
            (should (equal (plist-get (cdr (transient-get-suffix 'drover-dispatch "S")) :command)
                           'opencode-consult-sessions))
            ;; Installing twice adds nothing twice.
            (drover-opencode--install)
            (should (equal (mapcar (lambda (child) (plist-get (cdr child) :key))
                                   (aref (transient-get-suffix 'drover-dispatch '(0 0)) 2))
                           '("j" "B" "b" "l" "L" "p" "S"))))
        (kill-buffer session-buffer)
        (drover-opencode--install)))))

(ert-deftest drover-opencode-test-old-input-function-installed ()
  "drover edits earlier prompts in its prompt, unless the user chose a function."
  (drover-opencode-test-with nil
    (let ((opencode-old-input-function nil))
      (drover-opencode--install)
      (should (eq opencode-old-input-function #'drover-opencode-edit-old-input))
      (drover-opencode--uninstall)
      (should-not opencode-old-input-function)
      (setq opencode-old-input-function #'opencode-put-old-input)
      (drover-opencode--install)
      (should (eq opencode-old-input-function #'opencode-put-old-input))
      (drover-opencode--uninstall)
      (should (eq opencode-old-input-function #'opencode-put-old-input))
      (drover-opencode--install))))

(ert-deftest drover-opencode-test-ret-on-earlier-prompt ()
  "RET on an earlier prompt puts it whole in drover's prompt, set to go to its session."
  (drover-opencode-test-with (list (drover-opencode-test-entry "ses_1") (drover-opencode-test-entry "ses_2"))
    (drover-opencode-test-with-buffers '("half typed" "")
      (let ((opencode-old-input-function #'drover-opencode-edit-old-input)
            (drover-prompt-buffer-name "*drover opencode prompt test*")
            (drover-prompt-require-features nil)
            (org-mode-hook nil)
            (sent nil))
        (unwind-protect
            (cl-letf (((symbol-function 'drover-prompt--display) #'ignore)
                      ((symbol-function 'drover-prompt--schedule-refresh) #'ignore)
                      ((symbol-function 'drover-prompt--fetch-defaults) #'ignore)
                      ((symbol-function 'opencode-session-send)
                       (lambda (text &rest _) (push text sent))))
              (with-current-buffer (car buffers)
                (let ((inhibit-read-only t))
                  (goto-char (point-min))
                  (insert (propertize "first line\nsecond line" 'opencode-prompt t) "\n"))
                (goto-char (+ (point-min) 13))
                (opencode-send-input)
                (should-not sent)
                (should (equal (buffer-substring-no-properties (opencode-pmark) (point-max)) "half typed")))
              (with-current-buffer (drover-prompt-buffer)
                (should (equal (buffer-string) "first line\nsecond line"))
                (should (equal (plist-get drover-prompt--chosen-session :key) '(opencode . "ses_1")))
                (cl-letf (((symbol-function 'completing-read)
                           (lambda (&rest _) (ert-fail "The picker opened"))))
                  (should (equal (drover-session-key (drover-prompt-read-target "Send to session: "))
                                 '(opencode . "ses_1"))))))
          (when-let* ((prompt (get-buffer drover-prompt-buffer-name)))
            (kill-buffer prompt)))))))

;;;; The engine: models

(ert-deftest drover-opencode-test-default-variant ()
  "A model's default variant sends the effort a request without one sends."
  (cl-flet ((default (provider id &optional settings)
              (drover-model-default-variant (drover-opencode-fixtures-model provider id settings))))
    ;; OpenCode gives GPT-5 medium effort, GPT-6 nothing.
    (should (eq (default "openai" "gpt-5.5") 'medium))
    (should-not (default "openai" "gpt-6.1-sol-fast"))
    (should (eq (default "openai" "gpt-6.1-sol-fast" '((reasoningEffort . "high"))) 'high))
    ;; A model without variants has none to name.
    (should-not (default "opencode" "big-pickle" '((reasoningEffort . "high"))))
    ;; Claude 5.5 runs adaptive thinking at the API's default effort.
    (should (eq (default "anthropic" "claude-opus-5-5") 'medium))
    (should (eq (default "anthropic" "claude-sonnet-5-5") 'high))
    (should (eq (default "anthropic" "claude-sonnet-5-5" '((output_config (effort . "low")))) 'low))
    (should-not (default "anthropic" "claude-opus-5-5" '((thinking (type . "disabled")))))
    ;; Opus 4.6 thinks only when configured to.
    (should-not (default "anthropic" "claude-opus-4-6"))
    (should (eq (default "anthropic" "claude-opus-4-6" '((thinking (type . "adaptive")) (effort . "max")))
                'max))
    ;; Copilot decides Claude's effort unless it is configured.
    (should-not (default "github-copilot" "claude-opus-5.5"))
    (should (eq (default "github-copilot" "claude-opus-5.5" '((effort . "high"))) 'high))
    ;; Bedrock carries Claude's thinking in the request body; Converse ignores an effort setting.
    (should (eq (default "amazon-bedrock" "claude-opus-5-5") 'medium))
    (should (eq (default "amazon-bedrock" "claude-opus-5-5" '((effort . "low"))) 'medium)))
  ;; Adaptive thinking comes from binding thinking blocks, which the catalog can turn off.
  (let ((haiku `((id . "claude-haiku-5-5") (providerID . "anthropic")
                 (package . "@opencode/ai/providers/anthropic")
                 (settings (effort . "low"))
                 (variants . ,(drover-opencode-fixtures-claude-variants "low" "high")))))
    (should (eq (drover-model-default-variant (drover-opencode-model haiku nil)) 'low))
    (should-not (drover-model-default-variant
                 (drover-opencode-model (cons '(compatibility (supportsThinkingBlockBinding)) haiku)
                                        nil)))))

(ert-deftest drover-opencode-test-model ()
  "A catalog entry becomes a model naming its provider, variants and engines."
  (let ((gpt (drover-opencode-fixtures-model "openai" "gpt-5.5"))
        (opus (drover-opencode-fixtures-model "anthropic" "claude-opus-5-5"))
        (bedrock (drover-opencode-fixtures-model "amazon-bedrock" "claude-opus-5-5"))
        (pickle (drover-opencode-fixtures-model "opencode" "big-pickle")))
    (should (drover-model-p gpt))
    (should (equal (drover-model-provider-id gpt) "openai"))
    (should (equal (drover-model-provider-name gpt) "OpenAI"))
    (should (equal (drover-model-id gpt) "gpt-5.5"))
    (should (equal (drover-model-name gpt) "GPT-5.5"))
    (should (equal (drover-model-label gpt) "OpenAI: GPT-5.5"))
    ;; Each variant tells the effort it sends.
    (should (equal (drover-model-variants gpt)
                   '((none :effort "none") (low :effort "low") (medium :effort "medium")
                     (high :effort "high") (xhigh :effort "xhigh"))))
    (should (equal (cdr (assq 'max (drover-model-variants opus))) '(:effort "max")))
    (should (equal (cdr (assq 'low (drover-model-variants bedrock))) '(:effort "low")))
    ;; OpenCode runs any provider's model; the families' engines their own.
    (should (equal (drover-model-engines gpt) '(opencode codex)))
    (should (equal (drover-model-engines opus) '(opencode claude-code)))
    (should (equal (drover-model-engines pickle) '(opencode)))
    (should (equal (drover-model-id-for gpt 'opencode) "openai/gpt-5.5"))
    (should-not (drover-model-variants pickle))
    (should-not (drover-model-default-variant pickle)))
  ;; A variant sending no effort the paths know is a bare symbol.
  (let ((model (drover-opencode-model '((id . "m") (providerID . "p")
                                        (variants ((id . "fast") (body (service_tier . "priority")))))
                                      nil)))
    (should (equal (drover-model-variants model) '((fast))))
    (should-not (drover-model-provider-name model))))

(ert-deftest drover-opencode-test-variant-effort-candidates ()
  "An OpenCode variant offers the effort it sends, then its name."
  (let* ((defaults (drover-opencode-fixtures-defaults))
         (gpt (drover-opencode-fixtures-catalog-model defaults "openai" "gpt-5.5"))
         (bedrock (drover-opencode-fixtures-catalog-model defaults "amazon-bedrock" "claude-opus-5-5")))
    (should (equal (drover-model-effort-candidates gpt 'high) '("high" "high")))
    (should (equal (drover-model-effort-candidates bedrock 'low) '("low" "low")))
    (should (equal (drover-model-effort-candidates nil 'max) '("max")))))

(ert-deftest drover-opencode-test-model-ref ()
  "A model is referred to by provider and identifier, with the variant named."
  (let ((gpt (drover-opencode-fixtures-model "openai" "gpt-5.5")))
    (should (equal (drover-opencode-model-ref gpt) '((id . "gpt-5.5") (providerID . "openai"))))
    (should (equal (drover-opencode-model-ref gpt 'high)
                   '((id . "gpt-5.5") (providerID . "openai") (variant . "high"))))
    (should-not (drover-opencode-model-ref (drover-model-create :id "sonnet" :engines '(claude-code))))
    (should-not (drover-opencode-model-ref (drover-model-create :id "sonnet") 'high))))

(ert-deftest drover-opencode-test-ref-variants ()
  (should (eq (drover-opencode--ref-variant '((id . "m") (providerID . "p") (variant . "low"))) 'low))
  (should-not (drover-opencode--ref-variant '((id . "m") (providerID . "p") (variant . "default"))))
  (should-not (drover-opencode--ref-variant '((id . "m") (providerID . "p") (variant . ""))))
  (should-not (drover-opencode--ref-variant '((id . "m") (providerID . "p"))))
  (let ((ref '((id . "m") (providerID . "p") (variant . "low"))))
    (should (equal (drover-opencode--ref-with-variant ref 'high)
                   '((id . "m") (providerID . "p") (variant . "high"))))
    ;; The reference given is left alone.
    (should (equal ref '((id . "m") (providerID . "p") (variant . "low"))))
    (should (eq (drover-opencode--ref-with-variant ref nil) ref))))

(ert-deftest drover-opencode-test-ref-model ()
  "A reference resolves to the catalog's model, else to a bare one."
  (let* ((defaults (drover-opencode-fixtures-defaults))
         (catalog (plist-get defaults :catalog))
         (gpt (drover-opencode-fixtures-catalog-model defaults "openai" "gpt-5.5")))
    (should (eq (drover-opencode--ref-model '((id . "gpt-5.5") (providerID . "openai") (variant . "low"))
                                            catalog)
                gpt))
    (let ((retired (drover-opencode--ref-model '((id . "gpt-4") (providerID . "openai")) catalog)))
      (should (equal (drover-model-id retired) "gpt-4"))
      (should (equal (drover-model-provider-id retired) "openai"))
      (should (equal (drover-model-engines retired) '(opencode codex)))
      (should-not (drover-model-variants retired)))
    (should-not (drover-opencode--ref-model '((id . "gpt-5.5")) catalog))
    (should-not (drover-opencode--ref-model nil catalog))))

(ert-deftest drover-opencode-test-default-options ()
  "The catalogs give the default agent, each agent's pin and the default model."
  (should (equal (plist-get (drover-opencode-fixtures-defaults "plan") :agent) "plan"))
  ;; The default agent is the first the service lists that a session can run.
  (should (equal (plist-get (drover-opencode-fixtures-defaults "general") :agent) "build"))
  (should (equal (plist-get (drover-opencode-fixtures-defaults "compaction") :agent) "build"))
  (should (equal (plist-get (drover-opencode-fixtures-defaults) :agent) "build"))
  (should (equal (plist-get (drover-opencode--defaults-from
                             (seq-remove (lambda (agent) (equal (alist-get 'id agent) "build"))
                                         drover-opencode-fixtures-agents)
                             nil nil nil)
                            :agent)
                 "plan"))
  (let* ((defaults (drover-opencode-fixtures-defaults "plan"))
         (pin (cdr (assoc "plan" (plist-get defaults :agents)))))
    (should (eq (car pin) (drover-opencode-fixtures-catalog-model defaults "openai" "gpt-6.1-sol-fast")))
    (should (eq (cdr pin) 'high))
    (should-not (assoc "build" (plist-get defaults :agents)))
    (should (eq (plist-get defaults :default-model)
                (drover-opencode-fixtures-catalog-model defaults "opencode" "big-pickle")))
    (should (equal (drover-model-label (plist-get defaults :default-model)) "OpenCode: Big Pickle"))
    (should (equal (mapcar #'car (drover-model-variants
                                  (drover-opencode-fixtures-catalog-model defaults "openai" "gpt-5.5")))
                   '(none low medium high xhigh))))
  ;; A default model missing from the catalog is still a full model.
  (let ((defaults (drover-opencode--defaults-from
                   nil nil drover-opencode-fixtures-providers
                   (drover-opencode-fixtures-model-info "openai" "gpt-5.5"))))
    (should-not (plist-get defaults :catalog))
    (should (equal (drover-model-label (plist-get defaults :default-model)) "OpenAI: GPT-5.5"))
    (should (eq (drover-model-default-variant (plist-get defaults :default-model)) 'medium))))

(ert-deftest drover-opencode-test-recorded-catalogs ()
  "The recorded catalogs resolve: build is the default agent and GPT-5 runs medium."
  (let ((defaults (drover-opencode--defaults-from
                   (alist-get 'data (drover-opencode-fixtures-recorded "agent.json"))
                   (alist-get 'data (drover-opencode-fixtures-recorded "model.json"))
                   (alist-get 'data (drover-opencode-fixtures-recorded "provider.json"))
                   nil)))
    (should (equal (plist-get defaults :agent) "build"))
    (should-not (plist-get defaults :agents))
    (let ((gpt5 (drover-opencode-fixtures-catalog-model defaults "openai" "gpt-5.5"))
          (gpt6 (drover-opencode-fixtures-catalog-model defaults "openai" "gpt-6.1-sol"))
          (pickle (drover-opencode-fixtures-catalog-model defaults "opencode" "big-pickle")))
      (should (equal (drover-model-label gpt5) "OpenAI: GPT-5.5"))
      (should (eq (drover-model-default-variant gpt5) 'medium))
      (should (memq 'opencode (drover-model-engines gpt5)))
      (should (assq 'xhigh (drover-model-variants gpt6)))
      (should-not (drover-model-default-variant gpt6))
      (should-not (drover-model-variants pickle)))))

;;;; The engine: defaults

(ert-deftest drover-opencode-test-precedence ()
  "A pick wins, then the agent's pin, then the last opencode.el pick, then the default model."
  (let* ((defaults (drover-opencode-fixtures-defaults "build"))
         (gpt5 (drover-opencode-fixtures-catalog-model defaults "openai" "gpt-5.5"))
         (sol (drover-opencode-fixtures-catalog-model defaults "openai" "gpt-6.1-sol-fast"))
         (pickle (drover-opencode-fixtures-catalog-model defaults "opencode" "big-pickle"))
         (opencode-last-model nil))
    (should (equal (drover-opencode--resolve defaults nil nil nil)
                   (list :agent "build" :model pickle :variant nil)))
    (setq opencode-last-model '((id . "gpt-5.5") (providerID . "openai") (variant . "low")))
    (should (equal (drover-opencode--resolve defaults nil nil nil)
                   (list :agent "build" :model gpt5 :variant 'low)))
    ;; A variant the catalog proves the model lacks gives way to its default.
    (setq opencode-last-model '((id . "gpt-5.5") (providerID . "openai") (variant . "max")))
    (should (eq (plist-get (drover-opencode--resolve defaults nil nil nil) :variant) 'medium))
    ;; The plan agent's pin outranks the last pick.
    (should (equal (drover-opencode--resolve defaults "plan" nil nil)
                   (list :agent "plan" :model sol :variant 'high)))
    ;; A picked model runs its own default variant; a picked variant wins.
    (should (equal (drover-opencode--resolve defaults "plan" gpt5 nil)
                   (list :agent "plan" :model gpt5 :variant 'medium)))
    (should (eq (plist-get (drover-opencode--resolve defaults "plan" nil 'low) :variant) 'low))
    ;; Through the engine registry, as the prompt resolves.
    (should (equal (drover-engine-resolve 'opencode defaults "plan" nil nil)
                   (list :agent "plan" :model sol :variant 'high)))))

(ert-deftest drover-opencode-test-precedence-without-defaults ()
  "Before the defaults arrive, the last pick and the choices still resolve."
  (let ((opencode-last-model '((id . "gpt-5.5") (providerID . "openai") (variant . "max"))))
    (let ((resolved (drover-opencode--resolve nil nil nil nil)))
      (should-not (plist-get resolved :agent))
      (should (equal (drover-model-id (plist-get resolved :model)) "gpt-5.5"))
      ;; Without a catalog nothing proves the variant missing.
      (should (eq (plist-get resolved :variant) 'max))))
  (let ((opencode-last-model nil))
    (should (equal (drover-opencode--resolve nil "plan" nil 'low) '(:agent "plan" :model nil :variant low)))))

(defmacro drover-opencode-test-with-service (&rest body)
  "Run BODY with every OpenCode catalog fetch and request recorded, never sent.
`fetches' collects (KIND CALLBACK DIRECTORY ELSE) and `requests' (METHOD
PATH . KEYS), newest first; `warnings' the warnings of drover."
  (declare (indent 0) (debug t))
  `(let (fetches requests warnings)
     (cl-letf (((symbol-function 'opencode-catalog-fetch)
                (lambda (kind callback &optional directory else)
                  (push (list kind callback directory else) fetches)))
               ((symbol-function 'opencode-request)
                (lambda (method path &rest keys) (push (cons method (cons path keys)) requests)))
               ((symbol-function 'opencode-request-sync)
                (lambda (&rest _) (ert-fail "Unexpected synchronous request")))
               ((symbol-function 'drover-warn)
                (lambda (format-string &rest args)
                  (push (apply #'format format-string args) warnings))))
       ,@body)))

(ert-deftest drover-opencode-test-fetch-defaults ()
  "Defaults arrive once every answer did, in either order; a failed default model is unset."
  (dolist (reverse-order '(nil t))
    (drover-opencode-test-with-service
      (let ((delivered nil)
            (directory "/work/drover/"))
        (drover-opencode--fetch-defaults directory (lambda (defaults) (push defaults delivered)))
        (should (equal (sort (mapcar #'car fetches) #'string<) '(agent model provider)))
        (should (equal (mapcar #'cadr requests) '("/api/model/default")))
        ;; Every request is scoped to the directory, so its configuration applies.
        (dolist (fetch fetches)
          (should (equal (nth 2 fetch) directory)))
        (dolist (request requests)
          (should (equal (plist-get (cddr request) :directory) directory))
          (should (plist-member (cddr request) :buffer))
          (should-not (plist-get (cddr request) :buffer)))
        (let ((answers
               (append
                (mapcar (lambda (fetch)
                          (lambda ()
                            (funcall (nth 1 fetch)
                                     (pcase (car fetch)
                                       ('agent drover-opencode-fixtures-agents)
                                       ('model drover-opencode-fixtures-models)
                                       ('provider drover-opencode-fixtures-providers)))))
                        fetches)
                (mapcar (lambda (request)
                          (lambda ()
                            (funcall (plist-get (cddr request) (if reverse-order :else :then))
                                     (if reverse-order
                                         'failure
                                       `((data . ,(drover-opencode-fixtures-model-info
                                                   "openai" "gpt-5.5")))))))
                        requests))))
          (dolist (answer (if reverse-order (reverse answers) answers))
            (should-not delivered)
            (funcall answer))
          (should (= (length delivered) 1))
          (let ((defaults (car delivered)))
            (should (equal (plist-get defaults :agent) "build"))
            (should (equal (length (plist-get defaults :catalog))
                           (length drover-opencode-fixtures-models)))
            (if reverse-order
                (should-not (plist-get defaults :default-model))
              (should (equal (drover-model-id (plist-get defaults :default-model)) "gpt-5.5")))))
        (should-not warnings)))))

(ert-deftest drover-opencode-test-fetch-defaults-failures ()
  "A catalog that never arrives delivers nothing; nothing signals."
  (drover-opencode-test-with-service
    (let ((delivered nil))
      (drover-opencode--fetch-defaults "/work/" (lambda (defaults) (push defaults delivered)))
      ;; The model catalog failed: opencode.el reports it, and never calls back.
      (dolist (fetch fetches)
        (unless (eq (car fetch) 'model)
          (funcall (nth 1 fetch) nil)))
      (funcall (plist-get (cddr (car requests)) :else) 'failure)
      (should-not delivered)))
  (drover-opencode-test-with-service
    (cl-letf (((symbol-function 'opencode-catalog-fetch) (lambda (&rest _) (error "No service"))))
      (drover-opencode--fetch-defaults "/work/" (lambda (_) (ert-fail "Delivered")))
      (should (string-match-p "defaults of /work/: No service" (car warnings)))))
  ;; Data the defaults cannot be made of is reported, from the process filter.
  (drover-opencode-test-with-service
    (let ((delivered nil))
      (drover-opencode--fetch-defaults "/work/" (lambda (defaults) (push defaults delivered)))
      (dolist (fetch fetches)
        (funcall (nth 1 fetch) (if (eq (car fetch) 'model) '(((id . 1) (variants ((id . 2))))) nil)))
      (funcall (plist-get (cddr (car requests)) :else) 'failure)
      (should-not delivered)
      (should (string-match-p "Could not read the OpenCode defaults of /work/" (car warnings))))))

(ert-deftest drover-opencode-test-last-used ()
  "OpenCode was last used when a session last changed in its database."
  (drover-test-with-homes
    (should-not (drover-opencode--last-used))
    (make-directory (file-name-directory drover-opencode-database) t)
    (if (not (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
        (progn
          (drover-test-write drover-opencode-database "")
          (set-file-times drover-opencode-database (seconds-to-time 2500000))
          (should (= (drover-opencode--last-used) 2500000.0)))
      (let ((db (sqlite-open drover-opencode-database)))
        (sqlite-execute db "create table session (id text, time_updated integer)")
        (sqlite-execute db "insert into session values ('a', 9000000000)")
        (sqlite-close db))
      ;; An OpenCode 1 database has no session_v2 table yet: its files' times stand in.
      (set-file-times drover-opencode-database (seconds-to-time 2500000))
      (should (= (drover-opencode--last-used) 2500000.0))
      (drover-test-write (concat drover-opencode-database "-wal") "")
      (set-file-times (concat drover-opencode-database "-wal") (seconds-to-time 2600000))
      (should (= (drover-opencode--last-used) 2600000.0))
      (let ((db (sqlite-open drover-opencode-database)))
        (sqlite-execute db (concat "create table session_v2 (id text primary key, project_id text,"
                                   " parent_id text, directory text, title text, agent text,"
                                   " model text, time_created integer, time_updated integer)"))
        (sqlite-close db))
      ;; An empty table tells nothing either.
      (set-file-times drover-opencode-database (seconds-to-time 2500000))
      (should (= (drover-opencode--last-used) 2600000.0))
      (let ((db (sqlite-open drover-opencode-database)))
        (sqlite-execute db (concat "insert into session_v2 values"
                                   " ('ses_a', 'p', null, '/a', 'A', 'build', null, 1000, 4000000000),"
                                   " ('ses_b', 'p', 'ses_a', '/a', 'B', 'plan', null, 500, 1000)"))
        (sqlite-close db))
      (should (= (drover-opencode--last-used) 4000000.0)))))

;;;; The engine: registration, catalogs and agents

(ert-deftest drover-opencode-test-engine-registered ()
  "Loading the backend registers the engine and its catalog source."
  (let ((engine (alist-get 'opencode (default-value 'drover--engines))))
    (should (equal (plist-get engine :label) "OpenCode"))
    (should (eq (plist-get engine :defaults) #'drover-opencode--fetch-defaults))
    (should (eq (plist-get engine :resolve) #'drover-opencode--resolve))
    (should (eq (plist-get engine :agents) #'drover-opencode--agents))
    (should (eq (plist-get engine :last-used) #'drover-opencode--last-used))
    (should (plist-get engine :model-resets-variant))
    ;; Its defaults are fetched, not read from local files.
    (should-not (plist-get engine :volatile)))
  ;; OpenCode's catalog is the service's, not a live session's: it is
  ;; not remembered.
  (should (member '(opencode 0 drover-opencode--catalog-source nil)
                  (default-value 'drover--model-sources)))
  (drover-test-with-engines
    (drover-opencode--register-engine)
    (should (equal (drover-engines) '(opencode)))
    (should (equal (drover-engine-label 'opencode) "OpenCode"))
    (should (equal drover--model-sources '((opencode 0 drover-opencode--catalog-source nil))))))

(ert-deftest drover-opencode-test-engine-unregistered ()
  "Unregistering keeps the sources other front ends give the engine."
  (drover-test-with-engines
    (let ((shells (lambda (_directory) nil)))
      (drover-opencode--register-engine)
      (drover-register-model-source 'opencode shells 10)
      (drover-opencode--unregister-engine)
      (should (equal (drover-engines) '(opencode)))
      (should (equal (drover-engine-label 'opencode) "OpenCode"))
      (should-not (drover-engine-property 'opencode :defaults))
      (should (equal drover--model-sources (list (list 'opencode 10 shells nil))))
      ;; Without them, the engine goes.
      (drover-unregister-model-source 'opencode shells)
      (drover-opencode--register-engine)
      (drover-opencode--unregister-engine)
      (should-not (drover-engines))
      (should-not drover--model-sources))))

(defmacro drover-opencode-test-with-catalogs (catalogs &rest body)
  "Run BODY with opencode.el's catalogs answering from CATALOGS, at once.
CATALOGS is an alist (KIND . CATALOG); a kind missing from it never
answers.  `fetches' collects (KIND DIRECTORY), newest first, and
`reports' the errors reported."
  (declare (indent 1) (debug t))
  `(let ((catalogs ,catalogs)
         fetches reports)
     (cl-letf (((symbol-function 'opencode-catalog-fetch)
                (lambda (kind callback &optional directory _else)
                  (push (list kind directory) fetches)
                  (when-let* ((entry (assq kind catalogs)))
                    (funcall callback (cdr entry)))))
               ((symbol-function 'opencode-primary-agents) #'drover-opencode-fixtures-primary-agents)
               ((symbol-function 'opencode-report-error) (lambda (text) (push text reports))))
       ,@body)))

(ert-deftest drover-opencode-test-model-source ()
  "The catalogs of a directory are the engine's models."
  (drover-test-with-engines
    (drover-opencode--register-engine)
    (drover-opencode-test-with-catalogs `((model . ,drover-opencode-fixtures-models)
                                         (provider . ,drover-opencode-fixtures-providers))
      (let ((models (drover-models "/work/drover/")))
        (should (equal (mapcar #'drover-model-label models)
                       '("OpenAI: GPT-5.5" "OpenAI: GPT-6.1 Sol Fast" "OpenCode: Big Pickle"
                         "Anthropic: Claude Opus 5.5" "Anthropic: Claude Sonnet 5.5"
                         "Anthropic: Claude Opus 4.6" "GitHub Copilot: Claude Opus 5.5"
                         "Amazon Bedrock: Claude Opus 5.5")))
        (should (eq (drover-model-default-variant (car models)) 'medium))
        (should (equal (sort (mapcar #'car fetches) #'string<) '(model provider)))
        (dolist (fetch fetches)
          (should (equal (cadr fetch) "/work/drover/")))))
    ;; Without a model catalog, the source does not answer, so another may.
    (let ((drover-opencode-timeout 0.05))
      (drover-opencode-test-with-catalogs nil
        (should-not (drover-models "/work/drover/"))))))

(ert-deftest drover-opencode-test-catalog-wait ()
  "Waiting for a catalog is bounded, and a failure is reported."
  (let ((drover-opencode-timeout 0.1))
    (drover-opencode-test-with-catalogs nil
      (let ((start (float-time)))
        (should-not (drover-opencode--catalog-wait 'agent "/work/"))
        (should (< (- (float-time) start) 1.5))))
    (drover-opencode-test-with-catalogs '((agent))
      ;; An empty catalog is no catalog.
      (should-not (drover-opencode--catalog-wait 'agent "/work/")))
    (let (reports)
      (cl-letf (((symbol-function 'opencode-catalog-fetch)
                 (lambda (_kind _callback &optional _directory else)
                   (funcall else (opencode-error--make :method 'get :path "/api/agent" :message "Down"))))
                ((symbol-function 'opencode-report-error) (lambda (text) (push text reports))))
        (should-not (drover-opencode--catalog-wait 'agent "/work/"))
        (should (string-match-p "/api/agent failed: Down" (car reports)))))
    (drover-opencode-test-with-catalogs `((agent . ,drover-opencode-fixtures-agents))
      (should (equal (drover-opencode--catalog-wait 'agent "/work/") drover-opencode-fixtures-agents)))))

(ert-deftest drover-opencode-test-catalog-fetch-once ()
  "A catalog fetched without waiting calls back once, with nil on timeout."
  (let ((drover-opencode-timeout 0.05)
        (pending nil)
        (answers nil))
    (cl-letf (((symbol-function 'opencode-catalog-fetch)
               (lambda (_kind callback &optional _directory _else) (setq pending callback))))
      (drover-opencode--catalog-fetch 'agent "/work/" (lambda (catalog) (push catalog answers)))
      (should-not answers)
      (let ((deadline (+ (float-time) 2)))
        (while (and (not answers) (< (float-time) deadline))
          (accept-process-output nil 0.02)))
      (should (equal answers '(nil)))
      ;; A late answer is ignored.
      (funcall pending drover-opencode-fixtures-agents)
      (should (equal answers '(nil)))
      ;; An answer in time cancels the timeout.
      (setq answers nil)
      (drover-opencode--catalog-fetch 'agent "/work/" (lambda (catalog) (push catalog answers)))
      (funcall pending '(((id . "build"))))
      (sleep-for 0.1)
      (should (equal answers '((((id . "build"))))))))
  ;; A fetch that signals leaves no timer behind to call back later.
  (let ((drover-opencode-timeout 0.05)
        (answers nil))
    (cl-letf (((symbol-function 'opencode-catalog-fetch) (lambda (&rest _) (error "No service"))))
      (should-error (drover-opencode--catalog-fetch 'agent "/work/" (lambda (c) (push c answers))))
      (sleep-for 0.1)
      (should-not answers))))

(ert-deftest drover-opencode-test-agents ()
  "The engine offers the agents a session of the directory can run."
  (drover-opencode-test-with-catalogs `((agent . ,drover-opencode-fixtures-agents))
    (should (equal (drover-opencode--agents "/work/drover/") '("build" "plan")))
    (should (equal fetches '((agent "/work/drover/")))))
  (let ((drover-opencode-timeout 0.05))
    (drover-opencode-test-with-catalogs nil
      (should-not (drover-opencode--agents "/work/drover/")))))

;;;; Prompt operations: running sessions

(defmacro drover-opencode-test-with-session (&rest body)
  "Run BODY with `session' a running OpenCode session on build and GPT-5.5.
`buffer' is its buffer.  The stubbed service answers each request once
`answer' is called, in the buffer that made it, as `opencode-request'
does; `posts' collects (PATH . BODY), oldest first, `pending' the
requests not answered yet, and `warnings' the warnings of drover,
newest first."
  (declare (indent 0) (debug t))
  `(drover-opencode-test-with (list (drover-opencode-test-entry "ses_1"))
     (let ((buffer (generate-new-buffer " *opencode session*"))
           (opencode-last-model nil)
           posts pending warnings)
       (unwind-protect
           (cl-flet ((answer ()
                       (let ((request (pop pending)))
                         (with-current-buffer (plist-get (cdr request) :requested-in)
                           (funcall (plist-get (cdr request) :then) nil)))))
             (with-current-buffer buffer
               (setq-local opencode-session-id "ses_1"
                           opencode-session-agent "build"
                           opencode-session-model '((id . "gpt-5.5") (providerID . "openai"))))
             (puthash "ses_1" buffer opencode-session-buffers)
             (cl-letf (((symbol-function 'opencode-catalog-fetch)
                        (lambda (_kind callback &optional _directory _else)
                          (funcall callback drover-opencode-fixtures-agents)))
                       ((symbol-function 'opencode-primary-agents)
                        #'drover-opencode-fixtures-primary-agents)
                       ((symbol-function 'opencode-request)
                        (lambda (method path &rest keys)
                          (should (eq method 'post))
                          (setq posts (append posts (list (cons path (plist-get keys :body))))
                                pending (append pending
                                                (list (cons path (append keys (list :requested-in
                                                                                    (current-buffer)))))))))
                       ((symbol-function 'drover-warn)
                        (lambda (format-string &rest args)
                          (push (apply #'format format-string args) warnings))))
               (let ((session (drover-opencode-test-session "ses_1")))
                 ,@body)))
         (kill-buffer buffer)))))

(defun drover-opencode-test-apply (session options)
  "Apply OPTIONS to SESSION through the backend.
Return a function telling how many times the backend called back."
  (let ((done nil))
    (drover-backend-call session :apply-options options (lambda () (setq done (1+ (or done 0)))))
    (lambda () done)))

(ert-deftest drover-opencode-test-session-takes-options-in-order ()
  "A running session switches agent, then model, and only then takes the prompt."
  (let ((defaults (drover-opencode-fixtures-defaults "build")))
    (drover-opencode-test-with-session
      ;; The plan agent's pin follows the switch.
      (let ((done (drover-opencode-test-apply session '(:agent "plan" :model nil :variant nil))))
        (should (equal posts '(("/api/session/ses_1/agent" (agent . "plan")))))
        (answer)
        (should-not (funcall done))
        (should (equal (cdr (car (last posts)))
                       '((model (id . "gpt-6.1-sol-fast") (providerID . "openai") (variant . "high")))))
        (answer)
        (should (eql (funcall done) 1)))
      (should (equal (buffer-local-value 'opencode-session-model buffer)
                     '((id . "gpt-6.1-sol-fast") (providerID . "openai") (variant . "high"))))
      (should (equal opencode-last-model
                     '((id . "gpt-6.1-sol-fast") (providerID . "openai") (variant . "high"))))
      ;; An explicit model and variant replace the pin.
      (setq posts nil)
      (with-current-buffer buffer
        (setq opencode-session-agent "build"))
      (let ((done (drover-opencode-test-apply
                   session (list :agent "plan"
                                 :model (drover-opencode-fixtures-catalog-model defaults "openai" "gpt-5.5")
                                 :variant 'xhigh))))
        (answer)
        (answer)
        (should (eql (funcall done) 1)))
      (should (equal (mapcar #'cdr posts)
                     '(((agent . "plan"))
                       ((model (id . "gpt-5.5") (providerID . "openai") (variant . "xhigh"))))))
      ;; A model alone.
      (setq posts nil)
      (let ((done (drover-opencode-test-apply
                   session (list :model (drover-opencode-fixtures-catalog-model defaults "openai" "gpt-5.5")))))
        (answer)
        (should (eql (funcall done) 1)))
      (should (equal (mapcar #'cdr posts) '(((model (id . "gpt-5.5") (providerID . "openai"))))))
      (should-not warnings))))

(ert-deftest drover-opencode-test-session-variant-and-refusals ()
  "A lone variant keeps the model; unknown agents and refused switches still send."
  (drover-opencode-test-with-session
    (let ((done (drover-opencode-test-apply session '(:variant low))))
      (should (equal posts '(("/api/session/ses_1/model"
                              (model (id . "gpt-5.5") (providerID . "openai") (variant . "low"))))))
      (answer)
      (should (eql (funcall done) 1)))
    ;; An agent the directory lacks is reported; the current agent switches nothing.
    (setq posts nil)
    (let ((done (drover-opencode-test-apply session '(:agent "missing"))))
      (should (eql (funcall done) 1))
      (should-not posts)
      (should (string-match-p "refused agent missing (.*cannot run as agent missing); sending the prompt anyway"
                              (car warnings))))
    (let ((done (drover-opencode-test-apply session '(:agent "build"))))
      (should (eql (funcall done) 1))
      (should-not posts))
    ;; A refused switch is reported and the prompt still goes out.
    (let ((done (drover-opencode-test-apply session '(:variant high))))
      (funcall (plist-get (cdr (pop pending)) :else) "HTTP 400")
      (should (eql (funcall done) 1))
      (should (string-match-p "refused variant high (HTTP 400)" (car warnings)))
      (should (equal (alist-get 'variant (buffer-local-value 'opencode-session-model buffer)) "low")))
    ;; A variant with no model to run on.
    (with-current-buffer buffer
      (setq opencode-session-model nil))
    (let ((done (drover-opencode-test-apply session '(:variant high))))
      (should (eql (funcall done) 1))
      (should (string-match-p "names no model to run variant high on; keeping its default"
                              (car warnings))))
    ;; Nothing chosen, nothing asked.
    (setq posts nil)
    (should (eql (funcall (drover-opencode-test-apply session '(:agent nil :model nil :variant nil))) 1))
    (should-not posts)))

(ert-deftest drover-opencode-test-session-step-timeout ()
  "A switch the service never answers is given up, and the prompt goes out."
  (drover-opencode-test-with-session
    (let* ((drover-step-timeout 0.05)
           (done (drover-opencode-test-apply session '(:variant low)))
           (deadline (+ (float-time) 2)))
      (while (and (not (funcall done)) (< (float-time) deadline))
        (accept-process-output nil 0.02))
      (should (eql (funcall done) 1))
      (should (string-match-p "Timed out setting variant low" (car warnings)))
      ;; The late answer changes nothing more.
      (answer)
      (should (eql (funcall done) 1)))))

(ert-deftest drover-opencode-test-session-then-reported ()
  "An error of what follows the options is reported, not signaled from a callback."
  (drover-opencode-test-with-session
    (let ((calls 0))
      (drover-backend-call session :apply-options '(:variant low)
                           (lambda () (cl-incf calls) (error "Broken continuation")))
      (answer)
      (should (= calls 1))
      (should (string-match-p "Broken continuation" (car warnings))))))

(ert-deftest drover-opencode-test-session-opened-unseen ()
  "A session without a buffer is opened, undisplayed, to take options and prompts."
  (drover-opencode-test-with-session
    (remhash "ses_1" opencode-session-buffers)
    (let ((opened nil)
          (sent nil))
      (cl-letf (((symbol-function 'opencode-session-open-by-id)
                 (lambda (id &rest keys)
                   (push (cons id keys) opened)
                   (with-current-buffer buffer
                     (funcall (plist-get keys :callback) '((id . "ses_1"))))))
                ((symbol-function 'opencode-session-send)
                 (lambda (text &rest keys) (push (list (current-buffer) text keys) sent))))
        ;; Nothing to apply opens nothing.
        (should (eql (funcall (drover-opencode-test-apply session nil)) 1))
        (should-not opened)
        (let ((done (drover-opencode-test-apply session '(:variant low))))
          (should (equal (caar opened) "ses_1"))
          (should (plist-get (cdar opened) :no-display))
          (answer)
          (should (eql (funcall done) 1)))
        (drover-backend-call session :send "go" nil)
        (should (eq (car (car sent)) buffer))
        (should (equal (cadr (car sent)) "go"))
        (should (= (length opened) 2))
        ;; Opened later, from a process filter, a send that signals is reported.
        (let ((open-callback nil))
          (cl-letf (((symbol-function 'opencode-session-open-by-id)
                     (lambda (_id &rest keys) (setq open-callback (plist-get keys :callback))))
                    ((symbol-function 'opencode-session-send) (lambda (&rest _) (error "No service"))))
            (drover-backend-call session :send "go" nil)
            (with-current-buffer buffer
              (funcall open-callback '((id . "ses_1"))))
            (should (string-match-p "Title ses_1 did not take the prompt: No service" (car warnings)))))
        ;; A session that cannot be opened is reported, and the prompt still goes.
        (cl-letf (((symbol-function 'opencode-session-open-by-id)
                   (lambda (_id &rest keys)
                     (funcall (plist-get keys :else)
                              (opencode-error--make :method 'get :path "/api/session/ses_1"
                                                    :message "Gone")))))
          (should (eql (funcall (drover-opencode-test-apply session '(:variant low))) 1))
          (should (string-match-p "Could not open Title ses_1: .*Gone; sending the prompt anyway"
                                  (car warnings)))
          (drover-backend-call session :send "go" nil)
          (should (string-match-p "Title ses_1 did not take the prompt: .*Gone; it is in the kill ring"
                                  (car warnings))))))))

(ert-deftest drover-opencode-test-check-and-send ()
  "A prompt runs the session's send checks and goes out literally with their metadata."
  (drover-opencode-test-with-session
    (let (checked sent)
      (cl-letf (((symbol-function 'opencode-check-send)
                 (lambda (request) (push (cons (current-buffer) request) checked)
                   '((thinkingLoss . "accept"))))
                ((symbol-function 'opencode-session-send)
                 (lambda (text &rest keys) (setq sent (list (current-buffer) text keys)))))
        (let ((token (drover-backend-call session :check "/not a command" nil)))
          (should (equal checked `((,buffer :kind prompt :text "/not a command"))))
          (should (equal token '((thinkingLoss . "accept"))))
          (drover-backend-call session :send "/not a command" token))
        (should (eq (car sent) buffer))
        (should (equal (cadr sent) "/not a command"))
        (should (plist-get (nth 2 sent) :show))
        (should (equal (plist-get (nth 2 sent) :metadata) '((thinkingLoss . "accept"))))
        ;; A token that is no metadata sends none.
        (drover-backend-call session :send "go" nil)
        (should-not (plist-get (nth 2 sent) :metadata))
        (drover-backend-call session :send "go" 'fake-token)
        (should-not (plist-get (nth 2 sent) :metadata))
        ;; A failure is reported, the prompt being in the kill ring.
        (funcall (plist-get (nth 2 sent) :else)
                 (opencode-error--make :status 409 :method 'post :path "/api/session/ses_1/prompt"
                                       :message "Busy"))
        (should (string-match-p "\\*opencode session\\* did not take the prompt: .*Busy; it is in the kill ring"
                                (car warnings)))
        ;; A send that signals is reported too.
        (cl-letf (((symbol-function 'opencode-session-send) (lambda (&rest _) (error "No service"))))
          (drover-backend-call session :send "go" nil)
          (should (string-match-p "did not take the prompt: No service" (car warnings))))
        ;; Cancelled checks cancel.
        (cl-letf (((symbol-function 'opencode-check-send) (lambda (_) 'cancel)))
          (should (eq (drover-backend-call session :check "go" nil) 'cancel)))
        ;; Without a buffer, no check runs.
        (remhash "ses_1" opencode-session-buffers)
        (setq checked nil)
        (should-not (drover-backend-call session :check "go" nil))
        (should-not checked)))))

(ert-deftest drover-opencode-test-send-restore-refused ()
  "A prompt to an archived session the service will not restore says so, not that it was refused.
opencode.el restores the session before sending, a PATCH whose error
the send's ELSE gets: nothing was sent."
  (drover-opencode-test-with-session
    (let (sent)
      (cl-letf (((symbol-function 'opencode-session-send)
                 (lambda (text &rest keys) (setq sent (list text keys)))))
        (drover-backend-call session :send "go" nil)
        (funcall (plist-get (nth 1 sent) :else)
                 (opencode-error--make :status 409 :method 'patch :path "/api/session/ses_1"
                                       :message "Session is busy"))
        (should (equal (car warnings)
                       (concat "Could not restore Title ses_1 from the archive: PATCH /api/session/ses_1 "
                               "failed (HTTP 409): Session is busy; it is in the kill ring")))
        ;; The prompt itself refused is the buffer's not taking it.
        (funcall (plist-get (nth 1 sent) :else)
                 (opencode-error--make :status 400 :method 'post :path "/api/session/ses_1/prompt"
                                       :message "Bad"))
        (should (string-match-p "\\*opencode session\\* did not take the prompt: .*Bad; it is in the kill ring"
                                (car warnings)))
        ;; Not an error of a request: the prompt not taken.
        (funcall (plist-get (nth 1 sent) :else) "Gone")
        (should (string-match-p "did not take the prompt: Gone" (car warnings)))))))

(ert-deftest drover-opencode-test-scheduled-prompt-opens-session ()
  "A scheduled prompt to a session without a buffer opens it unseen, as archiving closed it.
A restore the service refuses, or a session that does not open, sends
nothing, which ELSE is told."
  (drover-opencode-test-with (list (drover-opencode-test-entry "ses_1"))
    (let ((session (drover-opencode-test-session "ses_1"))
          (buffer (generate-new-buffer " *opencode reopened*"))
          (opened nil)
          (sent nil)
          (answer nil)
          outcome)
      (unwind-protect
          (cl-letf (((symbol-function 'opencode-session-open-by-id)
                     (lambda (id &rest keys)
                       (push (cons id keys) opened)
                       (puthash id buffer opencode-session-buffers)
                       (with-current-buffer buffer
                         (funcall (plist-get keys :callback) `((id . ,id))))))
                    ((symbol-function 'opencode-session-send)
                     (lambda (text &rest keys)
                       (push (list (current-buffer) text) sent)
                       (funcall (if (eq answer 'refuse) (plist-get keys :else) (plist-get keys :then))
                                (if (eq answer 'refuse)
                                    (opencode-error--make :status 409 :method 'patch :path "/api/session/ses_1"
                                                          :message "Session is busy")
                                  nil)))))
            (cl-flet ((submit ()
                        (setq outcome nil)
                        (drover-opencode--submit session "later"
                                                 (lambda () (setq outcome 'sent))
                                                 (lambda (&rest arguments) (setq outcome arguments)))))
              (should-not (drover-opencode--buffer session))
              (submit)
              (should (eq outcome 'sent))
              (should (equal (caar opened) "ses_1"))
              (should (plist-get (cdar opened) :no-display))
              (should (equal (car sent) (list buffer "later")))
              ;; Its buffer open, it is used as it is.
              (submit)
              (should (eq outcome 'sent))
              (should (= (length opened) 1))
              ;; A refused restore: nothing sent, for sure.
              (setq answer 'refuse)
              (submit)
              (should (equal outcome
                             (list (concat "Could not restore Title ses_1 from the archive: PATCH "
                                           "/api/session/ses_1 failed (HTTP 409): Session is busy")
                                   t)))
              ;; A session that does not open.
              (remhash "ses_1" opencode-session-buffers)
              (cl-letf (((symbol-function 'opencode-session-open-by-id)
                         (lambda (_id &rest keys)
                           (funcall (plist-get keys :else)
                                    (opencode-error--make :method 'get :path "/api/session/ses_1"
                                                          :message "Gone")))))
                (submit))
              (should (string-match-p "Gone" (car outcome)))
              (should (eq (cadr outcome) t))))
        (kill-buffer buffer)))))

(ert-deftest drover-opencode-test-checks-the-switched-turn ()
  "A running session's checks run before any switch, for the agent and model it switches to."
  (drover-opencode-test-with-session
    (let (checked)
      (cl-letf (((symbol-function 'opencode-check-send)
                 (lambda (request) (push request checked) nil)))
        (cl-flet ((check (options)
                    (drover-backend-call session :check "go" options)
                    (pop checked)))
          ;; Nothing chosen: the turn as it stands.
          (should (equal (check nil) '(:kind prompt :text "go")))
          ;; The plan agent brings its pin, and a picked variant applies to it.
          (should (equal (check '(:agent "plan"))
                         '(:kind prompt :text "go" :agent "plan"
                                 :model ((id . "gpt-6.1-sol-fast") (providerID . "openai")
                                         (variant . "high")))))
          (should (equal (plist-get (check '(:agent "plan" :variant low)) :model)
                         '((id . "gpt-6.1-sol-fast") (providerID . "openai") (variant . "low"))))
          ;; The current agent is no switch; a variant alone applies to the session's model.
          (should (equal (check '(:agent "build" :variant low))
                         '(:kind prompt :text "go"
                                 :model ((id . "gpt-5.5") (providerID . "openai") (variant . "low")))))
          ;; A model chosen is the model checked.
          (should (equal (check (list :model (drover-opencode-fixtures-model "anthropic" "claude-opus-5-5")))
                         '(:kind prompt :text "go"
                                 :model ((id . "claude-opus-5-5") (providerID . "anthropic")))))
          ;; The model it runs already is no switch.
          (should (equal (check (list :model (drover-opencode-fixtures-model "openai" "gpt-5.5")))
                         '(:kind prompt :text "go")))
          ;; Neither is an agent the session cannot run.
          (should (equal (check '(:agent "general")) '(:kind prompt :text "go")))
          (should-not posts))))))

(ert-deftest drover-opencode-test-session-current-options ()
  "A running session shows its model, and its variant or the model's default."
  (drover-opencode-test-with-session
    (cl-letf (((symbol-function 'opencode-model-info)
               (lambda (ref &optional _directory)
                 (drover-opencode-fixtures-model-info (alist-get 'providerID ref) (alist-get 'id ref))))
              ((symbol-function 'opencode-catalog)
               (lambda (kind &optional _directory)
                 (and (eq kind 'provider) drover-opencode-fixtures-providers)))
              ((symbol-function 'opencode-state-get)
               (lambda (id)
                 (seq-find (lambda (entry) (equal (plist-get entry :id) id))
                           drover-opencode-test-entries))))
      (let ((options (drover-backend-call session :options)))
        (should (equal (drover-model-label (plist-get options :model)) "OpenAI: GPT-5.5"))
        (should (eq (plist-get options :variant) 'medium)))
      (with-current-buffer buffer
        (setq opencode-session-model '((id . "gpt-5.5") (providerID . "openai") (variant . "xhigh"))))
      (should (eq (plist-get (drover-backend-call session :options) :variant) 'xhigh))
      (with-current-buffer buffer
        (setq opencode-session-model '((id . "retired") (providerID . "openai"))))
      (let ((options (drover-backend-call session :options)))
        (should (equal (drover-model-id (plist-get options :model)) "retired"))
        (should-not (plist-get options :variant)))
      (with-current-buffer buffer
        (setq opencode-session-model nil))
      (should-not (drover-backend-call session :options))
      ;; Without a buffer, the registry tells.
      (remhash "ses_1" opencode-session-buffers)
      (should-not (drover-backend-call session :options))
      (setq drover-opencode-test-entries
            (list (drover-opencode-test-entry "ses_1" :model '((id . "claude-opus-5-5")
                                                              (providerID . "anthropic")))))
      (let ((options (drover-backend-call session :options)))
        (should (equal (drover-model-label (plist-get options :model)) "Anthropic: Claude Opus 5.5"))
        (should (eq (plist-get options :variant) 'medium))))))

;;;; Prompt operations: new sessions

(defmacro drover-opencode-test-with-creation (&rest body)
  "Run BODY with new sessions created in `new-buffer', recorded in `created'.
`opencode-session-create' records its keys in `created', then calls back
in `new-buffer', a session buffer of ses_new; `warnings' collects the
warnings of drover, newest first, `results' what THEN and ELSE got, as
\(then SESSION) and (else MESSAGE)."
  (declare (indent 0) (debug t))
  `(drover-opencode-test-with nil
     (let ((new-buffer (generate-new-buffer " *new opencode session*"))
           (opencode-last-model nil)
           created warnings results)
       (unwind-protect
           (cl-letf (((symbol-function 'opencode-catalog-fetch)
                      (lambda (_kind callback &optional _directory _else)
                        (funcall callback drover-opencode-fixtures-agents)))
                     ((symbol-function 'opencode-primary-agents)
                      #'drover-opencode-fixtures-primary-agents)
                     ((symbol-function 'opencode-default-agent)
                      (lambda (&optional _directory) "build"))
                     ((symbol-function 'opencode-session-create)
                      (lambda (&rest keys)
                        (setq created keys)
                        (with-current-buffer new-buffer
                          (setq-local opencode-session-id "ses_new"
                                      default-directory (plist-get keys :directory))
                          (funcall (plist-get keys :callback)
                                   `((id . "ses_new") (title . ,(plist-get keys :title)))))))
                     ((symbol-function 'drover-warn)
                      (lambda (format-string &rest args)
                        (push (apply #'format format-string args) warnings))))
             (cl-flet ((create (options &optional directory)
                         (setq created nil)
                         (funcall (drover-backend-property 'opencode :create)
                                  (or directory temporary-file-directory) 'opencode options
                                  (lambda (session) (push (list 'then session) results))
                                  (lambda (message) (push (list 'else message) results)))))
               ,@body))
         (kill-buffer new-buffer)))))

(ert-deftest drover-opencode-test-new-session-starts-on-pin ()
  "A new session starts as the chosen agent; opencode.el picks the model it pins."
  (drover-opencode-test-with-creation
    (let ((defaults (drover-opencode-fixtures-defaults "build")))
      (create (list :agent "plan" :title "Task" :defaults defaults))
      (should (equal (plist-get created :directory) temporary-file-directory))
      (should (equal (plist-get created :title) "Task"))
      (should (equal (plist-get created :agent) "plan"))
      ;; `opencode-session-create' starts on the agent's pin itself.
      (should-not (plist-get created :model))
      ;; THEN gets the new session, as soon as its buffer exists.
      (let ((session (cadr (car results))))
        (should (eq (car (car results)) 'then))
        (should (eq (drover-session-backend session) 'opencode))
        (should (equal (drover-session-id session) "ses_new"))
        (should (equal (drover-session-engine-key session) '(opencode . "ses_new")))
        (should (equal (drover-session-title session) "Task"))
        (should (equal (drover-session-directory session) temporary-file-directory))
        (puthash "ses_new" new-buffer opencode-session-buffers)
        (should (eq (drover-session-buffer session) new-buffer)))
      ;; A picked variant applies to the pin.
      (create (list :agent "plan" :variant 'low :defaults defaults))
      (should (equal (plist-get created :model)
                     '((id . "gpt-6.1-sol-fast") (providerID . "openai") (variant . "low"))))
      ;; A picked model leaves the variant to its provider unless one is picked.
      (create (list :agent "plan" :defaults defaults
                    :model (drover-opencode-fixtures-catalog-model defaults "openai" "gpt-5.5")))
      (should (equal (plist-get created :model) '((id . "gpt-5.5") (providerID . "openai"))))
      (create (list :defaults defaults :variant 'high
                    :model (drover-opencode-fixtures-catalog-model defaults "openai" "gpt-5.5")))
      (should (equal (plist-get created :model)
                     '((id . "gpt-5.5") (providerID . "openai") (variant . "high"))))
      ;; An unknown agent is reported and the default one, pinning nothing, starts,
      ;; named so that its reminder is stored before the first prompt.
      (create (list :agent "missing" :defaults defaults))
      (should (equal (plist-get created :agent) "build"))
      (should-not (plist-get created :model))
      (should (string-match-p "no primary OpenCode agent missing; starting its default one"
                              (car warnings)))
      ;; An empty agent or title is none.
      (setq warnings nil)
      (create (list :agent "" :title "" :defaults defaults))
      (should (equal (plist-get created :agent) "build"))
      (should-not (plist-get created :title))
      (should-not warnings)
      ;; A variant alone, without a pin, applies to the model the defaults resolve.
      (create (list :variant 'low :defaults defaults))
      (should (equal (plist-get created :model)
                     '((id . "big-pickle") (providerID . "opencode") (variant . "low"))))
      ;; Before the defaults arrive, a variant alone has no model to apply to.
      (create (list :variant 'low))
      (should-not (plist-get created :model)))
    ;; A picked variant applies to the pin of the default agent too.
    (cl-letf (((symbol-function 'opencode-default-agent) (lambda (&optional _directory) "plan")))
      (create (list :variant 'low :defaults (drover-opencode-fixtures-defaults "plan"))))
    (should (equal (plist-get created :agent) "plan"))
    (should (equal (plist-get created :model)
                   '((id . "gpt-6.1-sol-fast") (providerID . "openai") (variant . "low"))))
    (should (cl-every (lambda (result) (eq (car result) 'then)) results))))

(ert-deftest drover-opencode-test-new-session-failures ()
  "A creation refused, signaling or too long goes to ELSE, once."
  (drover-opencode-test-with-creation
    (cl-letf (((symbol-function 'opencode-session-create)
               (lambda (&rest keys)
                 (funcall (plist-get keys :else)
                          (opencode-error--make :status 500 :method 'post :path "/api/session"
                                                :message "refused")))))
      (create nil)
      (should (equal (car results) '(else "POST /api/session failed (HTTP 500): refused"))))
    (cl-letf (((symbol-function 'opencode-session-create) (lambda (&rest _) (error "No service"))))
      (setq results nil)
      (create nil)
      (should (equal results '((else "No service")))))
    (cl-letf (((symbol-function 'opencode-catalog-fetch) (lambda (&rest _) (error "No service"))))
      (setq results nil)
      (create nil)
      (should (equal results '((else "No service")))))
    ;; Nor does it signal once the catalog arrives, from a process filter.
    (let ((catalog-callback nil))
      (cl-letf (((symbol-function 'opencode-catalog-fetch)
                 (lambda (_kind callback &optional _directory _else) (setq catalog-callback callback)))
                ((symbol-function 'opencode-session-create) (lambda (&rest _) (error "No service"))))
        (setq results nil)
        (create nil)
        (should-not results)
        (funcall catalog-callback drover-opencode-fixtures-agents)
        (should (equal results '((else "No service"))))))
    ;; A session that never comes times out; coming late, it is ignored.
    (let ((drover-opencode-timeout 0.05)
          (late nil))
      (cl-letf (((symbol-function 'opencode-session-create)
                 (lambda (&rest keys) (setq late (plist-get keys :callback)))))
        (setq results nil)
        (create nil)
        (should-not results)
        (let ((deadline (+ (float-time) 2)))
          (while (and (not results) (< (float-time) deadline))
            (accept-process-output nil 0.02)))
        (should (equal results '((else "Timed out waiting for the new OpenCode session"))))
        (with-current-buffer new-buffer
          (setq-local opencode-session-id "ses_new")
          (funcall late '((id . "ses_new"))))
        (should (= (length results) 1))))
    ;; An error of THEN is reported briefly, not signaled from the callback.
    (let ((messages nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (format &rest arguments) (push (apply #'format-message format arguments) messages))))
        (funcall (drover-backend-property 'opencode :create) temporary-file-directory 'opencode nil
                 (lambda (_) (error "Broken continuation"))
                 (lambda (_) (ert-fail "ELSE called"))))
      (should (equal messages '("Drover OpenCode session start: error: Broken continuation"))))
    ;; The backend starts OpenCode sessions only.
    (setq results nil created nil)
    (funcall (drover-backend-property 'opencode :create) temporary-file-directory 'codex nil
             (lambda (session) (push (list 'then session) results))
             (lambda (message) (push (list 'else message) results)))
    ;; The engine's label, whichever engines are registered.
    (should (equal results `((else ,(format "OpenCode sessions run OpenCode, not %s"
                                             (drover-engine-label 'codex))))))
    (should-not created)))

(ert-deftest drover-opencode-test-new-session-does-not-wait ()
  "Creating a session waits for nothing: the agent catalog arrives later."
  (drover-opencode-test-with-creation
    (let ((catalog-callback nil))
      (cl-letf (((symbol-function 'opencode-catalog-fetch)
                 (lambda (_kind callback &optional _directory _else) (setq catalog-callback callback))))
        (create '(:agent "plan"))
        (should catalog-callback)
        (should-not created)
        (should-not results)
        (funcall catalog-callback drover-opencode-fixtures-agents)
        (should (equal (plist-get created :agent) "plan"))
        (should (eq (car (car results)) 'then))))
    ;; An agent catalog that never comes leaves the agent unchecked.
    (let ((drover-opencode-timeout 0.05))
      (cl-letf (((symbol-function 'opencode-catalog-fetch) #'ignore))
        (setq results nil)
        (create '(:agent "missing"))
        (should-not created)
        (let ((deadline (+ (float-time) 2)))
          (while (and (not created) (< (float-time) deadline))
            (accept-process-output nil 0.02)))
        (should (equal (plist-get created :agent) "missing"))
        (should (eq (car (car results)) 'then))
        (should-not warnings)))
    ;; Without a catalog, no agent is named: opencode.el chooses.
    (let ((drover-opencode-timeout 0.05))
      (cl-letf (((symbol-function 'opencode-catalog-fetch)
                 (lambda (_kind callback &optional _directory _else) (funcall callback nil))))
        (create nil)
        (should (plist-member created :agent))
        (should-not (plist-get created :agent))))))

;;;; Conversations by identifier, and prompts sent later

(ert-deftest drover-opencode-test-conversation-directory ()
  "The engine finds where a session runs from the service, or tells it has none."
  (drover-opencode-test-with nil
    (should (eq (drover-engine-property 'opencode :directory) #'drover-opencode--directory))
    (let (found failed)
      (drover-opencode--directory "ses_1" (lambda (directory) (setq found directory)) #'ignore)
      (should (equal (car drover-opencode-test-calls) '(opencode-session-fetch "ses_1" nil)))
      (funcall drover-opencode-test-fetched '((id . "ses_1") (location (directory . "/work/drover"))))
      (should (equal found "/work/drover/"))
      (drover-opencode--directory "ses_2" #'ignore (lambda (message) (setq failed message)))
      (funcall drover-opencode-test-fetched '((id . "ses_2")))
      (should (string-match-p "tells no directory" failed))
      (cl-letf (((symbol-function 'opencode-session-fetch)
                 (lambda (_id _callback &optional _directory else) (funcall else "HTTP 404")))
                ((symbol-function 'opencode-error-describe) #'identity))
        (drover-opencode--directory "ses_gone" #'ignore (lambda (message) (setq failed message))))
      (should (equal failed "No OpenCode session ses_gone: HTTP 404")))))

(ert-deftest drover-opencode-test-resume-by-id ()
  "opencode.el resumes a conversation by identifier, in the selected window, its buffer told once loaded."
  (drover-opencode-test-with nil
    (should (memq 'opencode (drover-resumer-front-ends 'opencode)))
    (should (drover-resumer-property 'opencode 'opencode :by-id))
    (should (drover-resumer-property 'opencode 'opencode :async))
    (let (opened got reported)
      (cl-letf (((symbol-function 'opencode-session-open-by-id)
                 (lambda (id &rest keys) (setq opened (cons id keys))))
                ((symbol-function 'opencode-error-describe) #'identity)
                ((symbol-function 'opencode-report-error) (lambda (text) (setq reported text))))
        (with-temp-buffer
          (drover-resume-with 'opencode 'opencode "ses_1" nil (lambda (buffer) (setq got buffer)))
          (should (equal (car opened) "ses_1"))
          (should (equal (plist-get (cdr opened) :other-window) nil))
          (funcall (plist-get (cdr opened) :callback) '((id . "ses_1")))
          (should (eq got (current-buffer))))
        ;; Without ELSE, a failure is reported.
        (drover-opencode-resume "ses_gone" nil)
        (funcall (plist-get (cdr opened) :else) "HTTP 404")
        (should (equal reported "No OpenCode session ses_gone: HTTP 404"))))))

(ert-deftest drover-opencode-test-scheduled-prompt ()
  "A scheduled prompt goes out literally and shown, consenting to drop stale thinking."
  (drover-opencode-test-with (list (drover-opencode-test-entry "ses_1"))
    (let ((session (drover-opencode-test-session "ses_1"))
          (ready nil))
      (should (equal (drover-opencode--blocker session) "the session has no buffer"))
      (with-temp-buffer
        (puthash "ses_1" (current-buffer) opencode-session-buffers)
        (cl-letf (((symbol-function 'opencode-session-ready-p) (lambda () ready)))
          (should (equal (drover-opencode--blocker session)
                         "the session is not loaded or not connected to OpenCode"))
          (setq ready t)
          (should-not (drover-opencode--blocker session)))
        (dolist (success '(t nil))
          (let (outcome)
            (cl-letf (((symbol-function 'opencode-session-send)
                       (lambda (text &rest arguments)
                         (should (equal text "/literal prompt"))
                         (should (plist-get arguments :show))
                         (should (plist-get arguments :accept-thinking-loss))
                         (if success
                             (funcall (plist-get arguments :then) nil)
                           (funcall (plist-get arguments :else) "Connection lost"))))
                      ((symbol-function 'opencode-error-describe) #'identity)
                      ((symbol-function 'y-or-n-p) (lambda (&rest _) (ert-fail "Never asks"))))
              (drover-opencode--submit session "/literal prompt"
                                       (lambda () (setq outcome 'sent))
                                       (lambda (message) (setq outcome message))))
            (should (equal outcome (if success 'sent "Connection lost")))))))
    (cl-letf (((symbol-function 'opencode-state-ready-p) #'always))
      (should (funcall (drover-backend-property 'opencode :ready))))))

(provide 'drover-opencode-test)
;;; drover-opencode-test.el ends here
