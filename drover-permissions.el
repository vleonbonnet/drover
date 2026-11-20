;;; drover-permissions.el --- The permission prompts agents gave the user, on every host  -*- lexical-binding: t; -*-

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

;; The permission prompts agents gave the user over a period, and how
;; the user answered, read from the ledger each host's harnesses append
;; to: one JSON object a line, in `drover-permissions-directory', a
;; file per engine.  An OpenCode line is a request asked, the reply to
;; one, by its request's identifier, or a call a plugin let pass
;; unasked; a Claude Code line is a prompt asked, whose answer its
;; conversation's transcript tells: a refused call's result says the
;; user did not want to proceed.  Each host reads its own, its peers
;; answering through the protocol as for the prompts the user sent
;; (`drover-prompts-gather-hosts'); the AI retro reads them through
;; Anvil's `drover-permissions' tool.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'iso8601)
(require 'drover-core)
(require 'drover-prompts)

(declare-function drover-remote-capable-hosts "drover-remote" (capability))
(declare-function drover-remote-ask "drover-remote" (name op request callback seconds))

(defgroup drover-permissions nil
  "The permission prompts agents gave the user."
  :group 'drover
  :prefix "drover-permissions-")

(defcustom drover-permissions-directory "~/.local/state/agent-permissions/"
  "Directory of this host's ledger of permission prompts.
Each engine's harness appends to ENGINE.jsonl there, rotated to
ENGINE.jsonl.1, one JSON object a line."
  :type 'directory
  :group 'drover-permissions)

(defcustom drover-permissions-engines '(opencode claude-code)
  "Engines whose permission prompts are read."
  :type '(repeat symbol)
  :group 'drover-permissions)

(defcustom drover-permissions-limit 5000
  "Permission prompts a host tells at most, the newest kept."
  :type 'natnum
  :group 'drover-permissions)

(defconst drover-permissions-transcript-bytes (* 64 1024 1024)
  "Bytes of a transcript read at most to learn its answers, its end kept.")

(defconst drover-permissions-replies '("once" "always" "reject" "passed" "allowed" "unknown")
  "The replies a prompt may have.
OpenCode's are once, always and reject; passed is a call a plugin let
go unasked; Claude Code's are allowed and reject, as its transcript
tells; unknown, an OpenCode prompt never answered, or a Claude one
whose call the transcript does not show.")

;;;; Reading the ledger

(defun drover-permissions--time (value)
  "Return VALUE, an ISO 8601 time, as a float time, or nil."
  (and (stringp value)
       (ignore-errors (float-time (encode-time (iso8601-parse value))))))

(defun drover-permissions--line (value limit)
  "Return VALUE when a string, on a line of LIMIT characters at most, else nil."
  (and (stringp value) (not (string-empty-p value)) (drover-bound-line value limit)))

(defun drover-permissions--strings (value count limit)
  "Return the first COUNT strings of VALUE, a list, cut to LIMIT characters."
  (and (listp value)
       (seq-take (seq-keep (lambda (item) (drover-permissions--line item limit)) value) count)))

(defun drover-permissions--rule (rule)
  "Return RULE, as an OpenCode line names the rule that asked, as text, or nil."
  (when (listp rule)
    (if (plist-get rule :default)
        "no rule"
      (let ((action (drover-permissions--line (plist-get rule :action) 120))
            (resource (drover-permissions--line (plist-get rule :resource) 300))
            (effect (drover-permissions--line (plist-get rule :effect) 10)))
        (and action (format "%s %s %s" action (or resource "*") (or effect "ask")))))))

(defun drover-permissions--read-file (file since until)
  "Return the entries of the ledger FILE from SINCE to UNTIL, oldest first.
Each is the line's plist, with its :time a float time; a line that is
not JSON is skipped."
  (let ((entries nil))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((line (buffer-substring-no-properties (point) (line-end-position)))
                 (entry (and (string-prefix-p "{" line)
                             (ignore-errors (json-parse-string line :object-type 'plist :array-type 'list
                                                               :null-object nil :false-object nil))))
                 (time (and (listp entry) (drover-permissions--time (plist-get entry :time)))))
            (when (and time (<= since time) (< time until))
              (push (plist-put entry :time time) entries)))
          (forward-line 1))))
    (nreverse entries)))

(defun drover-permissions--ledger (engine since until)
  "Return the entries ENGINE's ledger holds from SINCE to UNTIL, oldest first."
  (let ((file (expand-file-name (format "%s.jsonl" engine) drover-permissions-directory)))
    (append (drover-permissions--read-file (concat file ".1") since until)
            (drover-permissions--read-file file since until))))

(defun drover-permissions--make (engine entry reply)
  "Return the prompt ENGINE's ledger ENTRY tells, answered REPLY, or nil."
  (when-let* ((session (drover-permissions--line (plist-get entry :session) 200))
              (action (drover-permissions--line (plist-get entry :action) 120)))
    (list :engine engine :id session :time (plist-get entry :time) :action action
          :resources (drover-permissions--strings (plist-get entry :resources) 5 300)
          :rules (seq-keep #'drover-permissions--rule (seq-take (plist-get entry :rules) 5))
          :reply reply
          :message (drover-permissions--line (or (plist-get entry :reason) (plist-get entry :message)) 300)
          :plugin (drover-permissions--line (plist-get entry :plugin) 60)
          :directory (drover-permissions--line (plist-get entry :directory) 1024))))

(defun drover-permissions--opencode (entries)
  "Return the prompts OpenCode's ledger ENTRIES tell, each once, with its reply."
  (let ((replies (make-hash-table :test #'equal))
        (seen (make-hash-table :test #'equal))
        (prompts nil))
    (dolist (entry entries)
      (when (equal (plist-get entry :event) "replied")
        (when-let* ((request (plist-get entry :request))
                    (reply (plist-get entry :reply))
                    ((member reply '("once" "always" "reject"))))
          (puthash request reply replies))))
    (dolist (entry entries)
      (pcase (plist-get entry :event)
        ("asked"
         (when-let* ((request (plist-get entry :request))
                     ((stringp request))
                     ((not (gethash request seen))))
           (puthash request t seen)
           (when-let* ((prompt (drover-permissions--make 'opencode entry (gethash request replies "unknown"))))
             (push prompt prompts))))
        ("passed"
         (let ((key (list "passed" (plist-get entry :session) (plist-get entry :call))))
           (unless (gethash key seen)
             (puthash key t seen)
             (when-let* ((prompt (drover-permissions--make 'opencode entry "passed")))
               (push prompt prompts)))))))
    (nreverse prompts)))

(defconst drover-permissions--refused-regexp
  "doesn't want to proceed\\|tool use was rejected\\|user rejected"
  "What the result of a call the user refused in Claude Code says.")

(defun drover-permissions--transcript-calls (file)
  "Return the calls of Claude Code's transcript FILE, oldest first.
Each is a plist of :id, :name, :time and :reply, `allowed' or `reject'
as its result tells, nil without one."
  (let ((calls nil)
        (results (make-hash-table :test #'equal)))
    (when (and (stringp file) (file-readable-p file))
      (with-temp-buffer
        (let ((size (file-attribute-size (file-attributes file))))
          (insert-file-contents file nil (max 0 (- size drover-permissions-transcript-bytes)) size))
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((line (buffer-substring-no-properties (point) (line-end-position)))
                 (record (and (string-prefix-p "{" line)
                              (ignore-errors (json-parse-string line :object-type 'alist :array-type 'list
                                                                :null-object nil :false-object nil))))
                 (content (and (listp record) (alist-get 'content (alist-get 'message record)))))
            (when (listp content)
              (dolist (part content)
                (when (listp part)
                  (pcase (alist-get 'type part)
                    ("tool_use"
                     (when-let* ((id (alist-get 'id part)))
                       (push (list :id id :name (alist-get 'name part)
                                   :time (drover-permissions--time (alist-get 'timestamp record)))
                             calls)))
                    ("tool_result"
                     (when-let* ((id (alist-get 'tool_use_id part)))
                       (let ((text (format "%s" (alist-get 'content part))))
                         (puthash id (if (string-match-p drover-permissions--refused-regexp text) "reject" "allowed")
                                  results)))))))))
          (forward-line 1))))
    (mapcar (lambda (call) (plist-put call :reply (gethash (plist-get call :id) results)))
            (nreverse calls))))

(defun drover-permissions--claude-reply (entry transcripts)
  "Return the answer to Claude Code's prompt ENTRY, as its transcript tells.
The call is the one the hook named, else the last of its tool begun
before the prompt; TRANSCRIPTS caches the calls of each transcript read."
  (let* ((file (plist-get entry :transcript))
         (calls (and (stringp file)
                     (with-memoization (gethash file transcripts)
                       (drover-permissions--transcript-calls file))))
         (call (or (and (plist-get entry :call)
                        (seq-find (lambda (call) (equal (plist-get call :id) (plist-get entry :call))) calls))
                   (car (last (seq-filter (lambda (call)
                                            (and (equal (plist-get call :name) (plist-get entry :action))
                                                 (plist-get call :time)
                                                 (<= (plist-get call :time) (+ (plist-get entry :time) 1))))
                                          calls))))))
    (or (plist-get call :reply) "unknown")))

(defun drover-permissions--claude-code (entries)
  "Return the prompts Claude Code's ledger ENTRIES tell, each with its answer."
  (let ((transcripts (make-hash-table :test #'equal)))
    (seq-keep (lambda (entry)
                (and (equal (plist-get entry :event) "asked")
                     (drover-permissions--make 'claude-code entry
                                               (drover-permissions--claude-reply entry transcripts))))
              entries)))

(defun drover-permissions-collect (since until callback)
  "Read the permission prompts this host's agents gave from SINCE to UNTIL.
SINCE and UNTIL are float times, the period at most
`drover-prompts-days-limit' days long.  CALLBACK gets, once, a plist:
:permissions, oldest first, each a plist of :engine, :id (its
session), :time, :action, :resources, :rules (those that asked, as
text), :reply (see `drover-permissions-replies'), :message, :plugin and
:directory; :cut, how many of the oldest were left out past
`drover-permissions-limit'; :failures, a list of (ENGINE . MESSAGE).
The ledger is read from a timer.  Return a function stopping it."
  (drover-prompts--check-period since until)
  (let* ((stopped nil)
         (timer (run-at-time
                 0 nil
                 (lambda ()
                   (drover-with-reporting "permissions"
                     (unless stopped
                       (let ((prompts nil)
                             (failures nil))
                         (dolist (engine drover-permissions-engines)
                           (condition-case err
                               (let ((entries (drover-permissions--ledger engine since until)))
                                 (setq prompts (append prompts
                                                       (pcase engine
                                                         ('opencode (drover-permissions--opencode entries))
                                                         ('claude-code (drover-permissions--claude-code entries))))))
                             (error (push (cons engine (drover-error-message err)) failures))))
                         (let* ((sorted (sort prompts :key (lambda (prompt) (plist-get prompt :time)) :in-place t))
                                (cut (max 0 (- (length sorted) drover-permissions-limit))))
                           (funcall callback (list :permissions (nthcdr cut sorted) :cut cut
                                                   :failures (nreverse failures)))))))))))
    (lambda ()
      (setq stopped t)
      (cancel-timer timer))))

;;;; Between hosts

(defun drover-permissions--wire (prompt)
  "Return PROMPT as it travels to a peer."
  (append (list :g (symbol-name (plist-get prompt :engine)) :id (plist-get prompt :id)
                :t (plist-get prompt :time) :a (plist-get prompt :action) :y (plist-get prompt :reply)
                :r (vconcat (plist-get prompt :resources)) :u (vconcat (plist-get prompt :rules)))
          (and (plist-get prompt :message) (list :m (plist-get prompt :message)))
          (and (plist-get prompt :plugin) (list :p (plist-get prompt :plugin)))
          (and (plist-get prompt :directory) (list :d (plist-get prompt :directory)))))

(defun drover-permissions--encode (result)
  "Return RESULT, as `drover-permissions-collect' gives it, as it travels.
Within `drover-prompts-remote-bytes', the oldest left out first,
counted with those cut already."
  (let* ((wires (mapcar #'drover-permissions--wire (plist-get result :permissions)))
         (sizes (mapcar (lambda (wire) (+ 2 (string-bytes (json-serialize wire)))) wires))
         (total (apply #'+ sizes))
         (cut (plist-get result :cut)))
    (while (and wires (> total drover-prompts-remote-bytes))
      (setq total (- total (pop sizes))
            cut (1+ cut))
      (pop wires))
    (list :w (vconcat wires) :cut cut
          :f (vconcat (mapcar (lambda (failure)
                                (list :g (symbol-name (car failure)) :m (drover-bound-line (cdr failure) 300)))
                              (plist-get result :failures))))))

(defun drover-permissions--decode-prompt (wire host)
  "Return the prompt WIRE, as a peer HOST sent it, checked and bounded, or nil."
  (when (listp wire)
    (let ((engine (drover-prompts--decode-engine (plist-get wire :g)))
          (id (drover-permissions--line (plist-get wire :id) 200))
          (time (plist-get wire :t))
          (action (drover-permissions--line (plist-get wire :a) 120))
          (reply (car (member (plist-get wire :y) drover-permissions-replies))))
      (when (and engine id action reply (numberp time) (< 0 time 1e11))
        (list :engine engine :id id :time (float time) :action action :reply reply
              :resources (drover-permissions--strings (append (and (vectorp (plist-get wire :r)) (plist-get wire :r)) nil) 5 300)
              :rules (drover-permissions--strings (append (and (vectorp (plist-get wire :u)) (plist-get wire :u)) nil) 5 450)
              :message (drover-permissions--line (plist-get wire :m) 300)
              :plugin (drover-permissions--line (plist-get wire :p) 60)
              :directory (drover-permissions--line (plist-get wire :d) 1024)
              :host host)))))

(defun drover-permissions--decode (reply host)
  "Return REPLY, the permission prompts a peer HOST sent, checked, as a plist.
Of :permissions, :cut and :failures, as `drover-permissions-collect'
gives them."
  (let ((prompts nil)
        (failures nil)
        (wires (plist-get reply :w))
        (told (plist-get reply :f)))
    (when (vectorp wires)
      (seq-doseq (wire (seq-take wires drover-permissions-limit))
        (when-let* ((prompt (drover-permissions--decode-prompt wire host)))
          (push prompt prompts))))
    (when (vectorp told)
      (seq-doseq (failure (seq-take told 50))
        (when (listp failure)
          (when-let* ((engine (drover-prompts--decode-engine (plist-get failure :g))))
            (push (cons engine (or (drover-prompts--name (plist-get failure :m) 300) "failed")) failures)))))
    (list :permissions (nreverse prompts)
          :cut (let ((cut (plist-get reply :cut))) (if (natnump cut) cut 0))
          :failures (nreverse failures))))

(defun drover-permissions-remote-answer (message reply)
  "Answer a peer's MESSAGE asking this host's permission prompts, through REPLY.
MESSAGE tells the period, :since and :until, float times."
  (condition-case err
      (drover-permissions-collect
       (plist-get message :since) (plist-get message :until)
       (lambda (result) (funcall reply (drover-permissions--encode result))))
    (error (funcall reply nil (drover-error-message err)))))

;;;; Every host's

(defun drover-permissions-gather (since until callback &optional seconds)
  "Read the permission prompts agents gave from SINCE to UNTIL, on every host.
Each host reads its own; a peer has SECONDS, by default
`drover-prompts-timeout', to answer.  CALLBACK gets, once, a plist:
:permissions, every host's, oldest first, each with its :host, nil for
this one; :hosts, how each host answered, as `drover-prompts-gather'
tells.  Return a function stopping the reading."
  (drover-prompts--check-period since until)
  (drover-prompts-gather-hosts
   :permissions #'drover-permissions-collect
   (and (fboundp 'drover-remote-capable-hosts)
        (lambda () (drover-remote-capable-hosts "permissions")))
   (lambda (name request callback seconds)
     (drover-remote-ask name "permissions" request callback seconds))
   #'drover-permissions--decode
   since until callback seconds))

(provide 'drover-permissions)
;;; drover-permissions.el ends here
