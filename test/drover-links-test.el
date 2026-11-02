;;; drover-links-test.el --- Tests of drover's Org links  -*- lexical-binding: t; -*-

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

;; Links name conversations by engine and identifier, are stored from any
;; front end's session, and open where drover shows the conversation, or
;; resume it in a front end.  Engines and front ends are fakes: the
;; engines tell directories from `drover-links-test-directories', the
;; front ends record what they resume in `drover-links-test-resumed'.

;;; Code:

(require 'drover-test-helpers)
(require 'ol)
(require 'drover-links)

(defvar drover-links-test-directories nil
  "Directories the fake engines tell, as an alist (ID . DIRECTORY).")

(defvar drover-links-test-later nil
  "Callbacks of a fake engine telling directories later, as (THEN ELSE).")

(defvar drover-links-test-resumed nil
  "What the fake front ends resumed, newest first, as (FRONT-END ENGINE ID DIRECTORY).")

(defun drover-links-test--directory (id then else)
  "Tell the directory of ID from `drover-links-test-directories', as :directory does."
  (if-let* ((directory (alist-get id drover-links-test-directories nil nil #'equal)))
      (funcall then directory)
    (funcall else (format "No directory of %s" id))))

(defun drover-links-test--resumer (front-end engine)
  "Return a resumer of FRONT-END for ENGINE recording what it resumes."
  (lambda (id directory &rest _)
    (push (list front-end engine id directory) drover-links-test-resumed)
    nil))

(defmacro drover-links-test-with (&rest body)
  "Run BODY with fake engines and front ends, the fake backend serving no session.
The engines `claude-code' and `codex' tell directories at once,
`opencode' later, through `drover-links-test-later'; the front ends
`claude-code-ide', `agent-shell' and `opencode' are registered as
resumers, `opencode' by identifier, and none needs loading."
  (declare (indent 0) (debug t))
  `(drover-test-with-backend nil
     (drover-test-with-engines
       (let ((drover--resumers nil)
             (drover--resumer-properties nil)
             (drover-resume-front-ends nil)
             (drover-links-ask-front-end t)
             (drover-links-front-ends '((claude-code claude-code-ide agent-shell)
                                        (opencode opencode agent-shell)
                                        (codex agent-shell)))
             (drover-links-test-directories nil)
             (drover-links-test-later nil)
             (drover-links-test-resumed nil))
         (drover-test-register-engine 'claude-code :label "Claude Code"
                                      :directory #'drover-links-test--directory)
         (drover-test-register-engine 'codex :label "Codex" :directory #'drover-links-test--directory)
         (drover-test-register-engine 'opencode :label "OpenCode"
                                      :directory (lambda (_id then else)
                                                   (setq drover-links-test-later (list then else))))
         (drover-test-register-engine 'gemini-cli :label "Gemini CLI")
         (pcase-dolist (`(,engine . ,front-ends) '((claude-code claude-code-ide agent-shell)
                                                   (codex agent-shell)
                                                   (opencode agent-shell)))
           (dolist (front-end front-ends)
             (drover-register-resumer engine front-end (drover-links-test--resumer front-end engine))))
         (drover-register-resumer 'opencode 'opencode (drover-links-test--resumer 'opencode 'opencode)
                                  :by-id t :async t)
         (cl-letf (((symbol-function 'drover-links-load-front-end) #'ignore)
                   ((symbol-function 'drover-links--installed-p) #'ignore))
           ,@body)))))

;;;; Naming conversations

(ert-deftest drover-links-test-parse ()
  "Paths name an engine and an identifier, and maybe a host; a front end's type, a bare identifier."
  (should (equal (drover-links-parse "opencode/ses_03289d85affe6j3J60eOWQfsMF")
                 '(:engine opencode :id "ses_03289d85affe6j3J60eOWQfsMF" :host nil)))
  (should (equal (drover-links-parse "claude-code/48d12bd1-15e6-44a6-a7a3-271cda186c41")
                 '(:engine claude-code :id "48d12bd1-15e6-44a6-a7a3-271cda186c41" :host nil)))
  (should (equal (drover-links-parse "//aiur/codex/019a-x")
                 '(:engine codex :id "019a-x" :host "aiur")))
  (should (equal (drover-links-parse "12750ee6-33b0-4b37-a8ab-de48a7b1c0fc" 'claude-code)
                 '(:engine claude-code :id "12750ee6-33b0-4b37-a8ab-de48a7b1c0fc" :host nil)))
  (should (equal (drover-links-parse "opencode/ses_1" 'claude-code)
                 '(:engine opencode :id "ses_1" :host nil)))
  (dolist (path '("" "no-engine" "/ses_1" "opencode/"))
    (should-not (drover-links-parse path)))
  (should-not (drover-links-parse "" 'claude-code))
  (dolist (path '("opencode/ses_1" "//aiur/codex/019a-x"))
    (should (equal (drover-links-path (drover-links-parse path)) path)))
  (should (equal (drover-links-link '(:engine codex :id "r1")) "agent-session:codex/r1")))

(ert-deftest drover-links-test-conversation ()
  "Any front end's session names its conversation once its engine knows it, if the engine can find it again."
  (drover-links-test-with
    (pcase-dolist (`(,backend ,engine ,id ,expected)
                   '((opencode opencode "ses_1" (:engine opencode :id "ses_1" :host nil))
                     (agent-shell codex "rollout" (:engine codex :id "rollout" :host nil))
                     (agent-shell opencode "ses_2" (:engine opencode :id "ses_2" :host nil))
                     (claude-code-ide claude-code "c1" (:engine claude-code :id "c1" :host nil))
                     (claude-code-ide claude-code nil nil)
                     (claude-code-ide claude-code "" nil)
                     (agent-shell gemini-cli "g1" nil)))
      (should (equal (drover-links-conversation
                      (drover-session-create :backend backend :id "token" :engine engine :engine-id id))
                     expected)))))

;;;; Storing

(ert-deftest drover-links-test-store ()
  "A session buffer stores its link named after itself, a list row after the session's title."
  (drover-links-test-with
    (with-temp-buffer
      (rename-buffer "*claude* link fixture" t)
      (setq-local drover-session-at-point-function
                  (lambda () (drover-session-create :backend 'claude-code-ide :id "terminal-1"
                                                    :title "Fix the build"
                                                    :engine 'claude-code :engine-id "c1")))
      (let ((org-store-link-plist nil))
        (should (drover-links-store))
        (should (equal (plist-get org-store-link-plist :type) "agent-session"))
        (should (equal (plist-get org-store-link-plist :link) "agent-session:claude-code/c1"))
        (should (equal (plist-get org-store-link-plist :description) (buffer-name))))
      (setq-local drover-view-p t)
      (let ((org-store-link-plist nil))
        (should (drover-links-store))
        (should (equal (plist-get org-store-link-plist :description) "Fix the build")))
      ;; Drover's list copies the same link.
      (should (equal (drover-session-link (funcall drover-session-at-point-function))
                     "[[agent-session:claude-code/c1][Fix the build]]"))
      (setq-local drover-session-at-point-function #'ignore)
      (let ((org-store-link-plist nil))
        (should-not (drover-links-store))
        (should-not org-store-link-plist)))))

;;;; Opening

(ert-deftest drover-links-test-opens-live-session ()
  "A link to a conversation drover shows in a live buffer opens it there, whatever its front end."
  (drover-links-test-with
    (let ((session (drover-session-create :backend 'agent-shell :id "shell-1"
                                          :engine 'opencode :engine-id "ses_1"))
          opened)
      (with-temp-buffer
        (let ((buffer (current-buffer)))
          (cl-letf (((symbol-function 'drover-find-engine-session)
                     (lambda (engine id) (and (eq engine 'opencode) (equal id "ses_1") session)))
                    ((symbol-function 'drover-session-buffer) (lambda (_) buffer))
                    ((symbol-function 'drover-open) (lambda (target &rest _) (setq opened target)))
                    ((symbol-function 'completing-read)
                     (lambda (&rest _) (ert-fail "A live session needs no front end"))))
            (drover-links-follow "opencode/ses_1"))))
      (should (eq opened session))
      (should-not drover-links-test-resumed))))

(ert-deftest drover-links-test-resumes-claude ()
  "A Claude link drover does not show resumes in the front end chosen, from the session's directory."
  (drover-links-test-with
    (let ((drover-links-test-directories '(("c1" . "/project/")))
          (offered nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt collection &rest _)
                   (setq offered (list prompt (mapcar #'car collection)))
                   "claude-code-ide")))
        (drover-links-follow "claude-code/c1"))
      (should (equal offered '("Open Claude Code conversation in (default claude-code-ide): "
                               ("claude-code-ide" "agent-shell"))))
      (should (equal drover-links-test-resumed '((claude-code-ide claude-code "c1" "/project/"))))
      ;; A directory the engine cannot tell is the user's error.
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "agent-shell")))
        (should-error (drover-links-follow "claude-code/gone") :type 'user-error))
      (should (= 1 (length drover-links-test-resumed))))))

(ert-deftest drover-links-test-front-end-choice ()
  "One front end is not asked for; nor any when asking is off, the one preferred taken."
  (drover-links-test-with
    (let ((drover-links-test-directories '(("r1" . "/work/") ("c1" . "/project/"))))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) (ert-fail "Nothing to choose"))))
        (drover-links-follow "codex/r1")
        (should (equal (car drover-links-test-resumed) '(agent-shell codex "r1" "/work/")))
        (let ((drover-links-ask-front-end nil)
              (drover-resume-front-ends '((claude-code agent-shell))))
          (drover-links-follow "claude-code/c1"))
        (should (equal (car drover-links-test-resumed) '(agent-shell claude-code "c1" "/project/"))))
      ;; Installed front ends are offered before they are loaded.
      (drover-unregister-resumer 'claude-code 'agent-shell)
      (should (equal (drover-links-front-ends-for 'claude-code) '(claude-code-ide)))
      (cl-letf (((symbol-function 'drover-links--installed-p) (lambda (front-end) (eq front-end 'agent-shell))))
        (should (equal (drover-links-front-ends-for 'claude-code) '(claude-code-ide agent-shell))))
      (should-error (drover-links-follow "gemini-cli/g1") :type 'user-error))))

(ert-deftest drover-links-test-opens-opencode ()
  "OpenCode links open the session by identifier, or resume it in agent-shell where the service says it runs."
  (drover-links-test-with
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "opencode.el")))
      (drover-links-follow "opencode/ses_1"))
    (should (equal drover-links-test-resumed '((opencode opencode "ses_1" nil))))
    (should-not drover-links-test-later)
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "agent-shell")))
      (drover-links-follow "opencode/ses_2"))
    ;; The service answers later.
    (should (= 1 (length drover-links-test-resumed)))
    (funcall (car drover-links-test-later) "/work/drover/")
    (should (equal (car drover-links-test-resumed) '(agent-shell opencode "ses_2" "/work/drover/")))))

(ert-deftest drover-links-test-reports-unknown-opencode-session ()
  "A session the service cannot return is reported, not resumed."
  (drover-links-test-with
    (let (warned)
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "agent-shell"))
                ((symbol-function 'drover-warn)
                 (lambda (format-string &rest args) (setq warned (apply #'format format-string args)))))
        (drover-links-follow "opencode/ses_gone")
        (funcall (cadr drover-links-test-later) "No OpenCode session ses_gone: HTTP 404"))
      (should-not drover-links-test-resumed)
      (should (equal warned "No OpenCode session ses_gone: HTTP 404")))))

(ert-deftest drover-links-test-other-hosts ()
  "A conversation of another host is refused, for now."
  (drover-links-test-with
    (should-error (drover-links-follow "//aiur/claude-code/c1") :type 'user-error)
    (should-error (drover-links-follow "not a path") :type 'user-error)
    (should-not drover-links-test-resumed)))

;;;; Types of front ends, and the links of earlier configurations

(defconst drover-links-test-front-end-types
  '(("claude-code" nil claude-code)
    ("claude-code-ide" claude-code-ide claude-code)
    ("agent-shell" agent-shell claude-code)
    ("opencode" opencode opencode))
  "The link types of the configuration drover's links came from.")

(ert-deftest drover-links-test-front-end-types ()
  "Links of earlier configurations open as they did: in their front end, or asking."
  (drover-links-test-with
    (let ((drover-links-front-end-types drover-links-test-front-end-types)
          (drover-links-test-directories '(("12750ee6-33b0-4b37-a8ab-de48a7b1c0fc" . "/a/")
                                           ("90700b47-15f7-4080-9bcb-c5552cb4a9db" . "/b/")
                                           ("c3" . "/c/"))))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "claude-code-ide")))
        (drover-links-follow-front-end "claude-code" "12750ee6-33b0-4b37-a8ab-de48a7b1c0fc"))
      (should (equal (car drover-links-test-resumed)
                     '(claude-code-ide claude-code "12750ee6-33b0-4b37-a8ab-de48a7b1c0fc" "/a/")))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) (ert-fail "No front end to ask for"))))
        (drover-links-follow-front-end "claude-code-ide" "90700b47-15f7-4080-9bcb-c5552cb4a9db")
        (should (equal (car drover-links-test-resumed)
                       '(claude-code-ide claude-code "90700b47-15f7-4080-9bcb-c5552cb4a9db" "/b/")))
        (drover-links-follow-front-end "agent-shell" "c3")
        (should (equal (car drover-links-test-resumed) '(agent-shell claude-code "c3" "/c/")))
        (drover-links-follow-front-end "opencode" "ses_f5aba5415ffe0frMfWJx58erTe")
        (should (equal (car drover-links-test-resumed) '(opencode opencode "ses_f5aba5415ffe0frMfWJx58erTe" nil)))
        (drover-links-follow-front-end "agent-shell" "opencode/ses_x")
        (funcall (car drover-links-test-later) "/d/")
        (should (equal (car drover-links-test-resumed) '(agent-shell opencode "ses_x" "/d/"))))
      ;; A live buffer of another front end does not stand for the one named.
      (let ((session (drover-session-create :backend 'agent-shell :id "shell" :engine 'claude-code
                                            :engine-id "c3"))
            opened)
        (cl-letf (((symbol-function 'drover-known-sessions) (lambda (&rest _) (list session)))
                  ((symbol-function 'drover-session-buffer) (lambda (_) (current-buffer)))
                  ((symbol-function 'drover-open) (lambda (target &rest _) (setq opened target))))
          (drover-links-follow-front-end "claude-code-ide" "c3")
          (should-not opened)
          (drover-links-follow-front-end "agent-shell" "c3")
          (should (eq opened session))))
      (should-error (drover-links-follow-front-end "unknown" "x") :type 'user-error))))

(ert-deftest drover-links-test-register ()
  "Org follows agent-session links and those of the front ends' types, stores only the former."
  (let ((org-link-parameters (copy-tree org-link-parameters))
        (drover-links--registered-types nil))
    (let ((drover-links-front-end-types drover-links-test-front-end-types))
      (drover-links-register)
      (let ((parameters (cdr (assoc "agent-session" org-link-parameters))))
        (should (eq (plist-get parameters :follow) #'drover-links-follow))
        (should (eq (plist-get parameters :store) #'drover-links-store))
        (should (eq (plist-get parameters :export) #'drover-links-export))
        (should (eq (plist-get parameters :complete) #'drover-links-complete)))
      (dolist (type '("claude-code" "claude-code-ide" "agent-shell" "opencode"))
        (let ((parameters (cdr (assoc type org-link-parameters))))
          (should (functionp (plist-get parameters :follow)))
          (should-not (plist-get parameters :store))))
      (should (memq #'drover-links-store (org-store-link-functions))))
    ;; Types dropped from the option go.
    (let ((drover-links-front-end-types '(("claude-code" nil claude-code))))
      (drover-links-register)
      (should (assoc "claude-code" org-link-parameters))
      (should-not (assoc "claude-code-ide" org-link-parameters)))
    (drover-links-unload-function)
    (should-not (assoc "agent-session" org-link-parameters))
    (should-not (assoc "claude-code" org-link-parameters))))

;;;; Exporting and completing

(ert-deftest drover-links-test-export ()
  "A conversation exports as its description, the link a tooltip in HTML."
  (should (equal (drover-links-export "opencode/ses_1" "Fix <it>" 'html)
                 "<span class=\"agent-session\" title=\"agent-session:opencode/ses_1\">Fix &#60;it&#62;</span>"))
  (should (equal (drover-links-export "opencode/ses_1" nil 'latex)
                 "\\texttt{agent-session:opencode/ses\\char`\\_{}1}"))
  (should (equal (drover-links-export "opencode/ses_1" "Fix it" 'ascii) "Fix it"))
  (should (equal (drover-links-export "opencode/ses_1" nil 'md) "agent-session:opencode/ses_1")))

(ert-deftest drover-links-test-complete ()
  "Completion offers the sessions a link names."
  (drover-links-test-with
    (let ((linked (drover-test-session "a" :engine 'claude-code :engine-id "c1"))
          (unlinked (drover-test-session "b" :engine 'fake))
          offered)
      (setq drover-test-sessions (list linked unlinked))
      (cl-letf (((symbol-function 'drover-read-session)
                 (lambda (_prompt sessions) (setq offered sessions) (car sessions))))
        (should (equal (drover-links-complete) "agent-session:claude-code/c1")))
      (should (equal (mapcar #'drover-session-id offered) '("a")))
      (setq drover-test-sessions (list unlinked))
      (should-error (drover-links-complete) :type 'user-error))))

(provide 'drover-links-test)
;;; drover-links-test.el ends here
