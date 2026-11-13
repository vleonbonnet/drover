;;; drover-brief.el --- Briefs in the buffers of sessions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; Author: Valentin Leon <valentin@leon.click>
;; Keywords: tools, convenience

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

;; A session's buffer shows its brief just above its input while it is
;; idle, as Claude Code shows its recap above its prompt: a few
;; sentences a model wrote on where the whole session stands (see
;; `drover-session-brief-session'), so that the last turn, right above,
;; reads in its context, else the line it wrote on the last turn and its
;; recap; lists show the line alone.  The user comes back to a session
;; waiting for an answer to a permission or a question too: the buffer
;; shows its brief then at the end of its transcript, after the request.
;; This host's sessions show them as `drover-summary-mode' writes them,
;; another host's as their owner wrote them.  OpenCode's buffers, of
;; this host's service or another's, show it through
;; `opencode-session-summary-function', as text, agent-shell's through an
;; overlay; Claude Code's terminal shows its own recap, and the mirror of
;; another host's shell the overlay its owner drew.
;;
;; A buffer asks for the brief of its session with
;; `drover-brief-of-buffer', giving the function that shows it anew,
;; which runs whenever drover finds that session changed: a brief
;; written, or gone as the session works again.

;;; Code:

(require 'drover-core)

;; Reloaded into an Emacs running an older drover, load its core anew.
(unless (and (boundp 'drover-core-generation) (>= drover-core-generation 23))
  (load "drover-core" nil t))

(defcustom drover-brief-in-buffers t
  "Whether the buffers of sessions show their briefs above their input.
A brief is what a model wrote on where a session stands (see
`drover-session-brief-session' and `drover-summary-mode'): OpenCode's
and agent-shell's buffers show it while the session is idle, or waits
for its user to answer a permission or a question.  Claude Code's
terminal shows a recap of its own."
  :type 'boolean
  :group 'drover
  :set (lambda (symbol value)
         (set-default-toplevel-value symbol value)
         (when (fboundp 'drover-brief-refresh)
           (drover-brief-refresh t))))

(defvar drover-brief--buffers (make-hash-table :test #'eq :weakness 'key)
  "Buffers showing their session's brief, each mapped to (KEY . REFRESH).
KEY is the key of the session the buffer showed last, or nil; REFRESH,
a symbol, the function showing it anew, called in the buffer.")

(defun drover-brief--of (session)
  "Return the brief of SESSION for its buffer, or nil.
That is what was written of the whole session, a string, else the line
on its last turn and its recap, (LINE . RECAP).  The record drover
lists, whose brief it took in, else SESSION's own."
  (when-let* ((record (or (drover-index-session (drover-session-key session)) session))
              (line (drover-session-brief record)))
    (or (drover-session-brief-session record)
        (cons line (drover-session-brief-recap record)))))

(defun drover-brief-of-buffer (refresh &optional buffer)
  "Return the brief of the session of BUFFER for it to show, or nil.
It is a few sentences on where the whole session stands, a string, else
the line on its last turn and its recap, (LINE . RECAP).
BUFFER defaults to the current buffer, whose session is the one at
point there (see `drover-buffer-session').  REFRESH, a function called
in BUFFER without argument, shows it anew: drover calls it whenever
that session changed, until BUFFER is killed.  Nil when
`drover-brief-in-buffers' is nil."
  (let* ((buffer (or buffer (current-buffer)))
         (session (and drover-brief-in-buffers (drover-buffer-session buffer))))
    (puthash buffer (cons (and session (drover-session-key session)) refresh) drover-brief--buffers)
    (and session (drover-brief--of session))))

(defun drover-brief-forget (buffer)
  "Stop showing anew the brief of BUFFER's session."
  (remhash buffer drover-brief--buffers))

(defun drover-brief-refresh (&optional keys)
  "Show anew the briefs of the buffers of the sessions of KEYS.
KEYS is a list of the keys of sessions, or t for every buffer showing a
brief.  A buffer whose session drover did not know is shown anew too."
  (let ((due nil)
        (dead nil))
    (maphash (lambda (buffer entry)
               (cond ((not (buffer-live-p buffer)) (push buffer dead))
                     ((or (eq keys t) (null (car entry)) (member (car entry) keys))
                      (push (cons buffer (cdr entry)) due))))
             drover-brief--buffers)
    (dolist (buffer dead)
      (remhash buffer drover-brief--buffers))
    (pcase-dolist (`(,buffer . ,refresh) due)
      (drover-with-reporting "session brief"
        (with-current-buffer buffer
          (funcall refresh))))))

(defun drover-brief--changed ()
  "Show anew the briefs of the sessions that changed, for `drover-changed-hook'."
  (drover-brief-refresh drover-changed-sessions))

(add-hook 'drover-changed-hook #'drover-brief--changed)

(defun drover-brief-unload-function ()
  "Stop showing briefs anew."
  (remove-hook 'drover-changed-hook #'drover-brief--changed)
  nil)

(provide 'drover-brief)
;;; drover-brief.el ends here
