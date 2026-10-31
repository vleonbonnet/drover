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
;; `drover-opencode-test-calls'.  No request reaches a service.

;;; Code:

(require 'drover-test-helpers)
;; opencode first: drover loads the backend once opencode is loaded.
(require 'opencode)
(require 'drover-opencode)

(defvar opencode-state-updated-functions)

(defvar drover-opencode-test-entries nil
  "Registry entries `opencode-state-sessions' returns.")

(defvar drover-opencode-test-calls nil
  "Calls of stubbed opencode.el functions, newest first, as (FUNCTION . ARGUMENTS).")

(defvar drover-opencode-test-fetches nil
  "Pending projects fetches, oldest first, as (CALLBACK . ELSE).")

(defvar drover-opencode-test-handlers nil
  "Registered event handlers, oldest first, as (TYPE . FUNCTION).")

(defvar drover-opencode-test-branches '(("/work/drover/" . "main") ("/work/opencode/" . "dev"))
  "Branches of the directories, for the stubbed `opencode-git-branch'.")

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

(defmacro drover-opencode-test-with (entries &rest body)
  "Run BODY with the OpenCode backend alone registered, over registry ENTRIES."
  (declare (indent 1) (debug t))
  `(let ((drover--backends nil)
         (drover--started nil)
         (drover--running nil)
         (drover--change-timer nil)
         (drover-changed-hook nil)
         (drover-show-children nil)
         (drover-opencode--projects nil)
         (drover-opencode--projects-fetching nil)
         (drover-opencode--projects-stale nil)
         (opencode-state-updated-functions nil)
         (opencode-session-buffers (make-hash-table :test #'equal))
         (drover-opencode-test-entries ,entries)
         (drover-opencode-test-calls nil)
         (drover-opencode-test-fetches nil)
         (drover-opencode-test-handlers nil))
     (cl-letf (((symbol-function 'opencode-state-sessions)
                (lambda (&optional archived)
                  (should-not archived)
                  drover-opencode-test-entries))
               ((symbol-function 'opencode-state-status) #'drover-opencode-test--status)
               ((symbol-function 'opencode-state-since) #'drover-opencode-test--since)
               ((symbol-function 'opencode-state-cache)
                (lambda (id) (cdr (assoc id drover-opencode-test-caches))))
               ((symbol-function 'opencode-state-ensure) (drover-opencode-test--recorder 'opencode-state-ensure))
               ((symbol-function 'opencode-git-branch)
                (lambda (directory) (cdr (assoc directory drover-opencode-test-branches))))
               ((symbol-function 'opencode-model-label)
                (lambda (model)
                  (format "%s in %s" (alist-get 'id model) default-directory)))
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
               ((symbol-function 'opencode-session-archive) (drover-opencode-test--recorder 'opencode-session-archive))
               ((symbol-function 'opencode-session-view) (drover-opencode-test--recorder 'opencode-session-view))
               ((symbol-function 'opencode-session-last-reply)
                (lambda (id callback &rest arguments)
                  (push (append (list 'opencode-session-last-reply id) arguments) drover-opencode-test-calls)
                  (funcall callback (concat "Last reply of " id))))
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
    (let ((session (drover-session-create :backend 'opencode :id "ses_1")))
      (dolist (operation '(:start :sessions :open :respond :interrupt :archive :mark-seen :preview :new
                                  :buffer :rename :delete :fork :compact :history))
        (should (drover-supports-p session operation))))))

;;;; Mapping

(ert-deftest drover-opencode-test-maps-every-field ()
  (drover-opencode-test-with
      (list (list :id "ses_1" :title "Fix the build" :directory "/work/drover/"
                  :project-id "prj_drover" :parent-id "ses_0" :agent "build"
                  :model '((id . "claude-opus") (providerID . "anthropic") (variant . "high"))
                  :running t :retry nil
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
      (should (equal (drover-session-model session) "claude-opus in /work/drover/"))
      (should-not (drover-session-detail session))
      (should (equal (drover-session-summary session) "permit: bash: make test")))))

(ert-deftest drover-opencode-test-maps-missing-fields ()
  (drover-opencode-test-with (list (list :id "ses_1" :created 10.0 :updated 20.0))
    (cl-letf (((symbol-function 'opencode-git-branch) (lambda (_) (error "No directory to read")))
              ((symbol-function 'opencode-model-label) (lambda (_) (error "No model to label"))))
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

(ert-deftest drover-opencode-test-model-in-session-location ()
  (drover-opencode-test-with
      (list (drover-opencode-test-entry "ses_1" :directory "/work/opencode/" :model '((id . "gpt")))
            (drover-opencode-test-entry "ses_2" :directory nil :model '((id . "gpt"))))
    (should (equal (drover-session-model (drover-opencode-test-session "ses_1")) "gpt in /work/opencode/"))
    (let ((default-directory "/elsewhere/"))
      (should (equal (drover-session-model (drover-opencode-test-session "ses_2")) "gpt in /elsewhere/")))))

(ert-deftest drover-opencode-test-uses-real-model-label ()
  (let ((label (symbol-function 'opencode-model-label)))
    (drover-opencode-test-with
        (list (drover-opencode-test-entry "ses_1" :model '((id . "unknown-model") (providerID . "p")
                                                           (variant . "max"))))
      (cl-letf (((symbol-function 'opencode-model-label) label))
        ;; Without a catalog, the model shows its ID.
        (should (equal (drover-session-model (drover-opencode-test-session "ses_1"))
                       "unknown-model max"))))))

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
    (should (equal (mapcar #'car drover-opencode-test-handlers) '("project.updated" "server.connected")))
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

(ert-deftest drover-opencode-test-unload ()
  (drover-opencode-test-with nil
    (drover-start)
    (unwind-protect
        (should-not (drover-opencode-unload-function))
      ;; The tests that follow need what unloading undid.
      (drover-opencode--install))
    (should-not (drover-backends))
    (should-not (memq #'drover-opencode--updated opencode-state-updated-functions))
    (should (member '(opencode-events-unregister "project.updated" drover-opencode--projects-changed)
                    drover-opencode-test-calls))
    (should (member '(opencode-events-unregister "server.connected" drover-opencode--projects-changed)
                    drover-opencode-test-calls))))

;;;; Actions

(ert-deftest drover-opencode-test-open ()
  (drover-opencode-test-with (list (drover-opencode-test-entry "ses_1"))
    (let ((session (drover-opencode-test-session "ses_1"))
          (buffer (generate-new-buffer " ses_1")))
      (unwind-protect
          (progn
            (puthash "ses_1" buffer opencode-session-buffers)
            (drover-open session)
            (drover-open session t)
            (should (equal (reverse drover-opencode-test-calls)
                           '((opencode-session-open-by-id "ses_1" :other-window nil)
                             (opencode-session-open-by-id "ses_1" :other-window t)))))
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
  (drover-opencode-test-with (list (drover-opencode-test-entry "ses_1"))
    (let* ((session (drover-opencode-test-session "ses_1"))
           (drover-opencode-test-fetched nil)
           (origin (selected-window))
           (other (split-window)))
      (unwind-protect
          (progn
            (drover-open session t)
            ;; The user moved to another window before the session arrived.
            (select-window other)
            (funcall drover-opencode-test-fetched '((id . "ses_1")))
            (should (equal (car drover-opencode-test-calls)
                           `(opencode-session-open "ses_1" ,origin :other-window t)))
            (should (eq (selected-window) other)))
        (delete-window other)))))

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
                     '((opencode-respond-session "ses_1")
                       (opencode-session-interrupt "ses_1")
                       (opencode-session-archive "ses_1")
                       (opencode-session-view "ses_1")))))))

(ert-deftest drover-opencode-test-preview ()
  (drover-opencode-test-with (list (drover-opencode-test-entry "ses_1"))
    (let ((text nil))
      (drover-preview (drover-opencode-test-session "ses_1") (lambda (reply) (setq text reply)))
      (should (equal text "Last reply of ses_1"))
      (should (equal drover-opencode-test-calls '((opencode-session-last-reply "ses_1")))))))

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
         compact fork last-answer open-link)
  "The prompt actions OpenCode session buffers define.")

(ert-deftest drover-opencode-test-session-cache ()
  "A session carries the expiry and lifetime of its prompt cache from the registry."
  (drover-opencode-test-with (list (drover-opencode-test-entry "ses_1") (drover-opencode-test-entry "ses_2"))
    (let ((drover-opencode-test-caches '(("ses_1" :expires 1000.0 :ttl 300.0 :tokens 42))))
      (let ((cached (drover-opencode-test-session "ses_1"))
            (bare (drover-opencode-test-session "ses_2")))
        (should (= (drover-session-cache-expires cached) 1000.0))
        (should (= (drover-session-cache-ttl cached) 300.0))
        (should-not (drover-session-cache-expires bare))
        (should-not (drover-session-cache-ttl bare))))))

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
          (should (equal (drover-session-title old) "An old one"))
          (should (equal (drover-session-directory old) "/work/drover/"))
          (should (equal (drover-session-project old) "Drover"))
          (should (equal (drover-session-branch old) "main"))
          (should (equal (drover-session-parent old) "ses_parent"))
          (should (equal (drover-session-agent old) "build"))
          (should (equal (drover-session-model old) "claude in /work/drover/"))
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

;;;; The dispatch menu

(ert-deftest drover-opencode-test-dispatch ()
  (let ((keys (mapcar (lambda (key)
                        (plist-get (cdr (transient-get-suffix 'drover-dispatch key)) :command))
                      '("l" "L" "p" "S"))))
    (should (equal keys '(drover-list drover-list-side drover-select-project
                                      opencode-consult-sessions))))
  ;; After drover's lists, in their column.
  (should (equal (mapcar (lambda (child) (plist-get (cdr child) :key))
                         (aref (transient-get-suffix 'drover-dispatch '(0 1)) 2))
                 '("l" "L" "p" "S")))
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
                                   (aref (transient-get-suffix 'drover-dispatch '(0 1)) 2))
                           '("l" "L" "p" "S"))))
        (kill-buffer session-buffer)
        (drover-opencode--install)))))

(provide 'drover-opencode-test)
;;; drover-opencode-test.el ends here
