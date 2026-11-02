;;; drover-links.el --- Org links to the conversations of coding agents  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; Author: Valentin Leon <valentin@leon.click>
;; Keywords: tools, outlines

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

;; The Org link type agent-session names a conversation of a coding
;; agent, whichever front end shows it: agent-session:ENGINE/ID, ENGINE
;; the engine running it, such as `claude-code', `opencode' or `codex',
;; ID the engine's own identifier of the conversation, which survives
;; the front end's buffer and Emacs.  `org-store-link' stores one from
;; the session of the current buffer, in any front end, or from the
;; session at point in drover's list, once its engine knows the
;; identifier; such a link is described by the buffer's name, or by the
;; session's title in a list.
;;
;; Following a link shows the conversation where drover finds it live,
;; in any front end, and otherwise resumes it in a front end able to,
;; asking which when several are installed (`drover-links-front-ends').
;; Front ends resume conversations through `drover-resume-with', in the
;; directory the engine tells (its :directory, see
;; `drover-register-engine'), or by identifier alone, as opencode.el
;; does.  The types of `drover-links-front-end-types' open their
;; conversations in one front end, without asking, and read the links
;; of earlier configurations, such as claude-code:ID.
;;
;; A link may name a host, agent-session://HOST/ENGINE/ID, for the
;; conversations of other machines: they are read, and refused for now.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'drover-core)
(require 'drover-model)

;; Reloaded into an Emacs running an older drover, load its core anew.
(unless (and (boundp 'drover-core-generation) (>= drover-core-generation 3))
  (load "drover-core" nil t))

(defvar org-link-parameters)
(declare-function org-link-set-parameters "ol" (type &rest parameters))
(declare-function org-link-store-props "ol" (&rest plist))
(declare-function drover-read-session "drover-attention" (prompt sessions))

(defconst drover-links-type "agent-session"
  "The Org link type naming conversations of coding agents.")

(defconst drover-links--front-end-features
  '((opencode . drover-opencode)
    (claude-code-ide . drover-claude-code-ide)
    (agent-shell . drover-agent-shell))
  "The files of drover's backends, by front end: loading one loads its front end.")

(defconst drover-links--front-end-labels
  '((opencode . "opencode.el")
    (claude-code-ide . "claude-code-ide")
    (agent-shell . "agent-shell"))
  "Names of the front ends as the user knows them, by front end.")

(defconst drover-links--engine-features
  '((opencode . drover-opencode)
    (claude-code . drover-claude-code)
    (codex . drover-codex))
  "The files of drover's engines, by engine.")

(defcustom drover-links-front-ends
  '((claude-code claude-code-ide agent-shell)
    (opencode opencode agent-shell)
    (codex agent-shell))
  "The front ends a link may resume a conversation in, by engine.
An alist (ENGINE . FRONT-ENDS), FRONT-ENDS in the order offered: those
installed are offered, loaded or not, then any other front end loaded
that resumes ENGINE's conversations (see `drover-register-resumer')."
  :type '(alist :key-type (symbol :tag "Engine")
                :value-type (repeat (symbol :tag "Front end")))
  :group 'drover)

(defcustom drover-links-ask-front-end t
  "Non-nil to ask which front end resumes a conversation when several can.
Nil resumes it in the one `drover-resume-front-ends' prefers, else the
first of `drover-links-front-ends'."
  :type 'boolean
  :group 'drover)

(defvar drover-links--registered-types nil
  "The link types of `drover-links-front-end-types' registered last.")

(defun drover-links--set-front-end-types (symbol value)
  "Set SYMBOL to VALUE, the link types of front ends, and register them."
  (set-default symbol value)
  (when (featurep 'ol)
    (drover-links-register)))

(defcustom drover-links-front-end-types nil
  "Link types opening conversations in a front end of their own.
Each entry is (TYPE FRONT-END ENGINE): TYPE the link type, a string;
FRONT-END the front end resuming its conversations, such as
`claude-code-ide', `agent-shell' or `opencode', or nil to show them as
agent-session links do, asking for the front end when several can
resume them; ENGINE the engine of a link naming its conversation by
identifier alone, as TYPE:ID.  A link TYPE:ENGINE/ID names its engine.
None stores links: `org-store-link' stores agent-session links.  These
read the links of earlier configurations, for instance:

  ((\"claude-code\" nil claude-code)
   (\"claude-code-ide\" claude-code-ide claude-code)
   (\"agent-shell\" agent-shell claude-code)
   (\"opencode\" opencode opencode))"
  :type '(repeat (list (string :tag "Link type")
                       (choice (const :tag "Ask, as agent-session links" nil)
                               (symbol :tag "Front end"))
                       (symbol :tag "Engine of a bare identifier")))
  :set #'drover-links--set-front-end-types
  :initialize #'custom-initialize-default
  :group 'drover)

;;;; Naming conversations

(defun drover-links-parse (path &optional engine)
  "Return the conversation PATH names, as (:engine ENGINE :id ID :host HOST).
PATH is ENGINE/ID, or //HOST/ENGINE/ID for a conversation of another
machine; HOST is nil otherwise.  With ENGINE, a PATH without one, the
identifier alone, is a conversation of ENGINE.  Return nil when PATH
names none."
  (cond
   ((string-match "\\`//\\([^/]+\\)/\\([^/]+\\)/\\(.+\\)\\'" path)
    (list :engine (intern (match-string 2 path)) :id (match-string 3 path)
          :host (match-string 1 path)))
   ((string-match "\\`\\([^/]+\\)/\\(.+\\)\\'" path)
    (list :engine (intern (match-string 1 path)) :id (match-string 2 path) :host nil))
   ((and engine (not (string-empty-p path)) (not (string-search "/" path)))
    (list :engine engine :id path :host nil))))

(defun drover-links-path (conversation)
  "Return the path of an agent-session link to CONVERSATION.
CONVERSATION is a plist as `drover-links-parse' returns it."
  (map-let (:engine :id :host) conversation
    (concat (and host (concat "//" host "/")) (symbol-name engine) "/" id)))

(defun drover-links-link (conversation)
  "Return an agent-session link to CONVERSATION, without description.
CONVERSATION is a plist as `drover-links-parse' returns it."
  (concat drover-links-type ":" (drover-links-path conversation)))

(defun drover-links-linkable-p (engine)
  "Return non-nil when links name the conversations of ENGINE.
They do when ENGINE tells where its conversations ran (its :directory,
see `drover-register-engine'), so that a front end can resume them."
  (functionp (drover-engine-property engine :directory)))

(defun drover-links-conversation (session)
  "Return the conversation SESSION shows, as `drover-links-parse' returns one.
Nil until the engine of SESSION knows the conversation's identifier, or
when links do not name its engine's conversations (see
`drover-links-linkable-p')."
  (let ((engine (drover-session-engine session))
        (id (drover-session-engine-id session)))
    (when (and engine (stringp id) (not (string-empty-p id)) (drover-links-linkable-p engine))
      (list :engine engine :id id :host nil))))

;;;; Storing

(defun drover-links--description (session)
  "Return the description of a link to SESSION, from where it is stored.
A list names the session by its title, a session's buffer by its name."
  (or (and (drover-session-list-p) (drover-session-title session))
      (buffer-name)))

(defun drover-links-store (&optional _interactive)
  "Store an agent-session link to the session of the current buffer, or at point.
It is the session of a front end's buffer, or the session at point in a
list.  For `org-store-link'; return nil when there is no such session,
or no link to it (see `drover-links-conversation')."
  (when-let* ((session (drover-session-at-point t))
              (conversation (drover-links-conversation session)))
    (org-link-store-props :type drover-links-type
                          :link (drover-links-link conversation)
                          :description (drover-links--description session))))

;;;; Opening

(defun drover-links--load-engine (engine)
  "Load the file of ENGINE, when drover has one and it is not registered."
  (unless (drover-links-linkable-p engine)
    (when-let* ((feature (alist-get engine drover-links--engine-features)))
      (require feature))))

(defun drover-links-load-front-end (front-end)
  "Load drover's backend of FRONT-END, which loads FRONT-END, if not loaded."
  (when-let* ((feature (alist-get front-end drover-links--front-end-features)))
    (require feature)))

(defun drover-links--installed-p (front-end)
  "Return non-nil when FRONT-END, one drover knows, is installed."
  (and (assq front-end drover-links--front-end-features)
       (or (featurep front-end) (locate-library (symbol-name front-end)))
       t))

(defun drover-links-front-ends-for (engine)
  "Return the front ends able to resume a conversation of ENGINE, in order.
Those of `drover-links-front-ends' that are installed come first, then
the other front ends registered (see `drover-register-resumer')."
  (let ((registered (drover-resumer-front-ends engine)))
    (seq-uniq (append (seq-filter (lambda (front-end)
                                    (or (memq front-end registered)
                                        (drover-links--installed-p front-end)))
                                  (alist-get engine drover-links-front-ends))
                      registered))))

(defun drover-links--front-end-label (front-end)
  "Return the name of FRONT-END as the user knows it."
  (or (alist-get front-end drover-links--front-end-labels)
      (drover-backend-label front-end)))

(defun drover-links--preferred (engine candidates)
  "Return the front end of CANDIDATES to resume a conversation of ENGINE in.
It is the first `drover-resume-front-ends' names for ENGINE, else the
first of CANDIDATES."
  (or (seq-find (lambda (front-end) (memq front-end candidates))
                (alist-get engine drover-resume-front-ends))
      (car candidates)))

(defun drover-links-choose-front-end (engine)
  "Return the front end to resume a conversation of ENGINE in.
Ask which when several can (see `drover-links-front-ends-for') and
`drover-links-ask-front-end' is non-nil.  Signal a `user-error' when
none can."
  (let ((candidates (drover-links-front-ends-for engine))
        (label (drover-engine-label engine)))
    (cond
     ((null candidates)
      (user-error "No front end installed resumes %s conversations" label))
     ((or (null (cdr candidates)) (not drover-links-ask-front-end))
      (drover-links--preferred engine candidates))
     (t
      (let* ((names (mapcar (lambda (front-end) (cons (drover-links--front-end-label front-end) front-end))
                            candidates))
             (default (car (rassq (drover-links--preferred engine candidates) names))))
        (cdr (assoc (completing-read (format-prompt "Open %s conversation in" default label)
                                     names nil t nil nil default)
                    names)))))))

(defun drover-links--live (engine id front-end)
  "Return the session showing conversation ID of ENGINE in a live buffer, or nil.
With FRONT-END, a session of that front end only."
  (if front-end
      (let ((key (cons engine id)))
        (seq-find (lambda (session)
                    (and (eq (drover-session-backend session) front-end)
                         (equal (drover-session-engine-key session) key)
                         (drover-session-buffer session)))
                  (drover-known-sessions)))
    (when-let* ((session (drover-find-engine-session engine id))
                ((drover-session-buffer session)))
      session)))

(defun drover-links-directory (engine id then)
  "Call THEN with the directory conversation ID of ENGINE ran in.
The engine tells it (its :directory, see `drover-register-engine'),
possibly later.  Return what THEN returns when the engine tells at
once.  What fails while the command runs signals a `user-error'; what
fails later is reported."
  (let ((function (or (drover-engine-property engine :directory)
                      (user-error "%s does not tell where its conversations ran"
                                  (drover-engine-label engine))))
        (in-command t)
        (value nil))
    (funcall function id
             (lambda (directory)
               (if in-command
                   (setq value (funcall then directory))
                 (drover-with-reporting "agent-session link"
                   (funcall then directory))))
             (lambda (message)
               (if in-command
                   (user-error "%s" (drover-bound-line message))
                 (drover-warn "%s" message))))
    (setq in-command nil)
    value))

(defun drover-links-resume (front-end engine id)
  "Resume conversation ID of ENGINE in FRONT-END, in the selected window.
FRONT-END is loaded first.  A front end finding conversations by
identifier gets no directory; another, the one the engine tells."
  (drover-links-load-front-end front-end)
  (if (drover-resumer-property engine front-end :by-id)
      (drover-resume-with front-end engine id nil)
    (drover-links-directory engine id
                            (lambda (directory)
                              (drover-resume-with front-end engine id directory)))))

(defun drover-links-open (conversation &optional front-end)
  "Show CONVERSATION, a plist as `drover-links-parse' returns it.
A conversation drover shows in a live buffer is displayed there, through
`drover-open'; otherwise it is resumed in a front end, asked for when
several can (see `drover-links-choose-front-end').  With FRONT-END,
only a live buffer of that front end shows it, and it resumes there
without asking."
  (map-let (:engine :id :host) conversation
    (when host
      (user-error "Conversations of other hosts cannot be opened yet: %s" (drover-bound-line host)))
    (drover-links--load-engine engine)
    (if-let* ((session (drover-links--live engine id front-end)))
        (drover-open session)
      (drover-links-resume (or front-end (drover-links-choose-front-end engine)) engine id))))

(defun drover-links-follow (path &optional _arg)
  "Show the conversation the agent-session link PATH names.
See `drover-links-open'.  For Org's :follow."
  (drover-links-open (or (drover-links-parse path)
                         (user-error "Not a link to a conversation: %s:%s"
                                     drover-links-type (drover-bound-line path)))))

(defun drover-links-follow-front-end (type path &optional _arg)
  "Show the conversation the link PATH of TYPE names, in TYPE's front end.
TYPE is a link type of `drover-links-front-end-types'."
  (pcase-let ((`(,_ ,front-end ,engine) (or (assoc type drover-links-front-end-types)
                                            (user-error "Not a link type of drover: %s" type))))
    (drover-links-open (or (drover-links-parse path engine)
                           (user-error "Not a link to a conversation: %s:%s" type
                                       (drover-bound-line path)))
                       front-end)))

;;;; Exporting and completing

(defun drover-links--html (text)
  "Return TEXT with the characters HTML gives a meaning written as references."
  (replace-regexp-in-string "[&<>\"]" (lambda (match) (format "&#%d;" (string-to-char match)))
                            text t t))

(defun drover-links--latex (text)
  "Return TEXT with the characters LaTeX gives a meaning written as characters."
  (replace-regexp-in-string "[][\\{}$&#^_%~]" (lambda (match) (format "\\char`\\%s{}" match))
                            text t t))

(defun drover-links-export (path description backend &optional _info)
  "Return the agent-session link PATH, described by DESCRIPTION, for BACKEND.
A conversation has no address outside Emacs: HTML shows its description
with the link as a tooltip, LaTeX in a typewriter font, other backends
the text alone.  For Org's :export."
  (let ((link (concat drover-links-type ":" path)))
    (pcase backend
      ('html (format "<span class=\"agent-session\" title=\"%s\">%s</span>"
                     (drover-links--html link) (drover-links--html (or description link))))
      ('latex (format "\\texttt{%s}" (drover-links--latex (or description link))))
      (_ (or description link)))))

(defun drover-links-complete (&optional _arg)
  "Read a session of drover and return an agent-session link to it.
Its candidates are the sessions whose conversation a link names.  For
Org's :complete."
  (require 'drover-attention)
  (drover-start)
  (let ((sessions (seq-filter #'drover-links-conversation (drover-known-sessions))))
    (unless sessions
      (user-error "No session's conversation has a link yet"))
    (drover-links-link (drover-links-conversation (drover-read-session "Session: " sessions)))))

;;;; Registering

;;;###autoload
(defun drover-links-register ()
  "Define drover's Org link types.
They are agent-session and those of `drover-links-front-end-types'; the
types dropped from that option since it was last registered go."
  (require 'ol)
  (org-link-set-parameters drover-links-type
                           :follow #'drover-links-follow
                           :store #'drover-links-store
                           :export #'drover-links-export
                           :complete #'drover-links-complete)
  (dolist (type drover-links--registered-types)
    (unless (assoc type drover-links-front-end-types)
      (setq org-link-parameters (assoc-delete-all type org-link-parameters))))
  (pcase-dolist (`(,type . ,_) drover-links-front-end-types)
    (org-link-set-parameters type
                             :follow (lambda (path &optional arg)
                                       (drover-links-follow-front-end type path arg))
                             :store nil))
  (setq drover-links--registered-types (mapcar #'car drover-links-front-end-types)))

;;;###autoload
(with-eval-after-load 'ol (drover-links-register))

(defun drover-links-unload-function ()
  "Forget drover's Org link types."
  (when (boundp 'org-link-parameters)
    (dolist (type (cons drover-links-type drover-links--registered-types))
      (setq org-link-parameters (assoc-delete-all type org-link-parameters))))
  nil)

(provide 'drover-links)
;;; drover-links.el ends here
