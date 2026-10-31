;;; drover-test-helpers.el --- Shared test helpers for drover  -*- lexical-binding: t; -*-

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

;; A fake backend, `fake', serves `drover-test-sessions' and records
;; every action in `drover-test-calls'.  `drover-test-with-backend'
;; isolates the registry, the change hook and its timer.

;;; Code:

(require 'ert)
(require 'drover)

(defvar drover-test-sessions nil
  "Sessions the fake backend serves.")

(defvar drover-test-calls nil
  "Actions the fake backend received, newest first, as (OPERATION ID . ARGUMENTS).")

(defconst drover-test-now 1800000000.0
  "A fixed current time for tests.")

(defun drover-test-session (id &rest properties)
  "Return a fake session ID with PROPERTIES, keyword arguments of `drover-session-create'.
The title defaults to \"Session ID\"."
  (apply #'drover-session-create
         (append properties (list :backend 'fake :id id :title (concat "Session " id)))))

(defun drover-test--recorder (operation)
  "Return a fake backend function recording OPERATION."
  (lambda (session &rest arguments)
    (push (append (list operation (drover-session-id session)) arguments) drover-test-calls)))

(defun drover-test-register ()
  "Register the fake backend."
  (drover-register-backend
   'fake
   :label "Fake"
   :sessions (lambda () drover-test-sessions)
   :start (lambda () (push '(:start nil) drover-test-calls))
   :open (drover-test--recorder :open)
   :respond (drover-test--recorder :respond)
   :interrupt (drover-test--recorder :interrupt)
   :archive (drover-test--recorder :archive)
   :mark-seen (drover-test--recorder :mark-seen)
   :rename (drover-test--recorder :rename)
   :delete (drover-test--recorder :delete)
   :fork (drover-test--recorder :fork)
   :compact (drover-test--recorder :compact)
   :new (lambda (directory) (push (list :new directory) drover-test-calls))
   :preview (lambda (session callback)
              (push (list :preview (drover-session-id session)) drover-test-calls)
              (funcall callback (concat "Preview of " (drover-session-id session))))))

(defmacro drover-test-with-backend (sessions &rest body)
  "Run BODY with the fake backend alone registered, serving SESSIONS."
  (declare (indent 1) (debug t))
  `(let ((drover--backends nil)
         (drover--started nil)
         (drover--running nil)
         (drover--change-timer nil)
         (drover-changed-hook nil)
         (drover-show-children nil)
         (drover-filter nil)
         (drover-group-by 'status)
         (drover-test-sessions ,sessions)
         (drover-test-calls nil))
     (unwind-protect
         (progn (drover-test-register) ,@body)
       (when (timerp drover--change-timer)
         (cancel-timer drover--change-timer)))))

(provide 'drover-test-helpers)
;;; drover-test-helpers.el ends here
