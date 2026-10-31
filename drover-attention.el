;;; drover-attention.el --- Sessions that need you  -*- lexical-binding: t; -*-

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

;; Keeping up with sessions without a dashboard:
;;
;; - `drover-attention-mode' counts the sessions that need you in the
;;   mode line, one count per status, such as "⚑2 ✓1".  mouse-1 on it
;;   visits the most urgent, mouse-3 chooses one.
;; - `drover-next' visits the most urgent session that needs you;
;;   repeating it walks through the others, in the order of the first
;;   press, and wraps around.
;; - `drover-select' chooses any session with completion, grouped by
;;   status, most urgent first.

;;; Code:

(require 'drover)
(require 'drover-core)
(require 'drover-ui)
(require 'seq)
(require 'subr-x)

(defcustom drover-attention-echo t
  "Whether `drover-next' tells which session it visits, and its place in the cycle."
  :type 'boolean
  :group 'drover)

;;;; Mode line

(defvar drover-attention--cycle nil
  "Keys of the sessions `drover-next' walks through, as of its first press.")

(defvar drover-attention--position 0
  "Position in `drover-attention--cycle' of the session last visited.")

(defvar drover-attention-mode-line nil
  "The mode line construct of `drover-attention-mode', or nil.
It is computed when sessions change, so that drawing the mode line
costs nothing.")
;; Without it, the mode line drops the string's faces and mouse bindings.
(put 'drover-attention-mode-line 'risky-local-variable t)

(defvar-keymap drover-attention-mode-line-map
  :doc "Mouse bindings of the session counts in the mode line."
  "<mode-line> <mouse-1>" #'drover-next
  "<mode-line> <mouse-3>" #'drover-select)

(defvar drover-attention--mode-string-origin nil
  "What `global-mode-string' was before the mode made it a list holding its count.
nil when it was already a list, `empty' when it was nil, `string' when
it was a string; disabling the mode restores it.")

(defun drover-attention--counts (sessions)
  "Return an alist (STATUS . COUNT) of SESSIONS that need the user.
Statuses come in `drover-status-order'; those with no session are left
out."
  (delq nil
        (mapcar (lambda (status)
                  (let ((count (seq-count (lambda (session)
                                            (eq (drover-session-status session) status))
                                          sessions)))
                    (and (> count 0) (cons status count))))
                (seq-sort-by #'drover-status-rank #'< drover-attention-statuses))))

(defun drover-attention--describe (session)
  "Return one line describing SESSION, for lists of sessions."
  (let ((location (drover-session-location session))
        (summary (drover-session-summary session)))
    (concat (drover-status-glyph (drover-session-status session)) " "
            (format "%s" (drover-session-display-title session))
            (and location (concat "  " (propertize location 'face 'drover-dim)))
            (and summary (concat "  " summary)))))

(defun drover-attention--help (sessions)
  "Return the help text of the mode line counts of SESSIONS."
  (concat "Sessions that need you:\n"
          (mapconcat #'drover-attention--describe sessions "\n")
          "\n\nmouse-1: visit the most urgent\nmouse-3: choose a session"))

(defun drover-attention--format (sessions)
  "Return the mode line counts of SESSIONS, the sessions that need the user.
Return nil when SESSIONS is empty."
  (when-let* ((counts (drover-attention--counts sessions)))
    (concat " "
            (propertize
             (mapconcat (pcase-lambda (`(,status . ,count))
                          (concat (drover-status-glyph status)
                                  (propertize (number-to-string count)
                                              'face (drover-status-face status))))
                        counts " ")
             'help-echo (drover-attention--help sessions)
             'mouse-face 'mode-line-highlight
             'local-map drover-attention-mode-line-map))))

(defun drover-attention--update ()
  "Compute the mode line counts again and redraw every mode line."
  (setq drover-attention-mode-line (drover-attention--format (drover-attention-sessions)))
  (force-mode-line-update t))

(defun drover-attention--changed ()
  "Follow a change of sessions."
  (drover-attention--update))

(defun drover-attention--show ()
  "Add the counts to `global-mode-string'."
  (unless (and (listp global-mode-string)
               (memq 'drover-attention-mode-line global-mode-string))
    (setq drover-attention--mode-string-origin
          (cond ((null global-mode-string)
                 ;; A list starting with a symbol is a conditional.
                 (setq global-mode-string (list ""))
                 'empty)
                ((stringp global-mode-string)
                 (setq global-mode-string (list global-mode-string))
                 'string)))
    (setq global-mode-string (append global-mode-string '(drover-attention-mode-line)))))

(defun drover-attention--hide ()
  "Remove the counts from `global-mode-string', restoring what it was."
  (when (listp global-mode-string)
    (setq global-mode-string (remq 'drover-attention-mode-line global-mode-string))
    (pcase drover-attention--mode-string-origin
      ('empty (when (equal global-mode-string '(""))
                (setq global-mode-string nil)))
      ('string (when (and (= (length global-mode-string) 1) (stringp (car global-mode-string)))
                 (setq global-mode-string (car global-mode-string))))))
  (setq drover-attention--mode-string-origin nil))

;;;###autoload
(define-minor-mode drover-attention-mode
  "Show in the mode line how many sessions need you.
The mode line holds one count per status of
`drover-attention-statuses', such as \"⚑2 ✓1\", and nothing when no
session needs you.  Its tooltip lists the sessions; clicking it visits
the most urgent (`drover-next'), and clicking it with the right button
chooses one (`drover-select').  Enabling the mode starts the backends."
  :global t
  :group 'drover
  (if drover-attention-mode
      (progn
        (drover-attention--show)
        (add-hook 'drover-changed-hook #'drover-attention--changed)
        (drover-start)
        (drover-attention--update))
    (remove-hook 'drover-changed-hook #'drover-attention--changed)
    (drover-attention--hide)
    (setq drover-attention-mode-line nil
          drover-attention--cycle nil)
    (force-mode-line-update t)))

;;;; Visiting the next session

(defun drover-attention--visit (session position)
  "Display SESSION, at POSITION in the cycle, in the selected window."
  (setq drover-attention--position position)
  (drover-open session)
  (when drover-attention-echo
    (message "%d/%d %s" (1+ position) (length drover-attention--cycle)
             (drover-attention--describe session))))

(defun drover-attention--following ()
  "Return the session after the last one visited, and its position, as a cons.
Sessions gone since the first press are skipped; return nil when every
one is gone."
  (let* ((sessions (drover-sessions))
         (length (length drover-attention--cycle)))
    (seq-some (lambda (step)
                (let* ((position (mod (+ drover-attention--position step) length))
                       (key (nth position drover-attention--cycle)))
                  (when-let* ((session (seq-find (lambda (session)
                                                   (equal (drover-session-key session) key))
                                                 sessions)))
                    (cons session position))))
              (number-sequence 1 length))))

;;;###autoload
(defun drover-next ()
  "Visit the most urgent session that needs you, in the selected window.
Repeating the command visits the following session, and wraps around.
The order is the one of the first press, so that sessions changing
status meanwhile, as the ones visited do, keep their place."
  (interactive)
  (if-let* ((following (and (eq last-command 'drover-next)
                            drover-attention--cycle
                            (drover-attention--following))))
      (drover-attention--visit (car following) (cdr following))
    (let ((sessions (drover-attention-sessions)))
      (setq drover-attention--cycle (mapcar #'drover-session-key sessions))
      (if sessions
          (drover-attention--visit (car sessions) 0)
        (message "No session needs you")))))

;;;; Choosing a session

(defun drover-attention--names (sessions)
  "Return an alist (NAME . SESSION) of SESSIONS, in order, NAMEs unique.
NAME is the session's title; titles shared by several sessions are
followed by the session's identifier, and a counter if still needed."
  (let ((titles (make-hash-table :test #'equal))
        (taken (make-hash-table :test #'equal)))
    (dolist (session sessions)
      (cl-incf (gethash (format "%s" (drover-session-display-title session)) titles 0)))
    (mapcar (lambda (session)
              (let* ((title (format "%s" (drover-session-display-title session)))
                     (base (if (> (gethash title titles) 1)
                               (format "%s (%s)" title (drover-session-id session))
                             title))
                     (name base)
                     (counter 1))
                (while (gethash name taken)
                  (setq counter (1+ counter)
                        name (format "%s <%d>" base counter)))
                (puthash name t taken)
                (cons name session)))
            sessions)))

(defun drover-attention--pad (string width)
  "Return STRING padded with spaces to WIDTH columns."
  (concat string (make-string (max 0 (- width (string-width string))) ?\s)))

(defun drover-attention--annotator (candidates)
  "Return the annotation function of CANDIDATES, an alist (NAME . SESSION).
Annotations line up in columns: status glyph, project and branch, age
of the status or of the last activity, and why the session has its
status."
  (let* ((width (apply #'max 0 (mapcar (lambda (candidate) (string-width (car candidate)))
                                       candidates)))
         (location-width (apply #'max 0 (mapcar (lambda (candidate)
                                                  (string-width (or (drover-session-location (cdr candidate)) "")))
                                                candidates)))
         (now (float-time)))
    (lambda (name)
      (when-let* ((session (cdr (assoc name candidates))))
        (let ((summary (drover-session-summary session)))
          (concat (drover-attention--pad "" (- width (string-width name)))
                  "  " (drover-status-glyph (drover-session-status session)) " "
                  (propertize (drover-attention--pad (or (drover-session-location session) "")
                                                     location-width)
                              'face 'drover-dim)
                  "  "
                  (propertize (string-pad (drover-format-age (or (drover-session-since session)
                                                                 (drover-session-updated session))
                                                             now)
                                          3 nil t)
                              'face 'drover-dim)
                  (and summary (concat "  " (propertize summary 'face 'completions-annotations)))))))))

(defun drover-read-session (prompt sessions)
  "Read one of SESSIONS with completion, PROMPT first, and return it.
Candidates keep the order of SESSIONS, grouped by status label, and
are annotated with each session's status, location, age and summary.
Their completion category is `drover-session'."
  (let* ((candidates (drover-attention--names sessions))
         (names (mapcar #'car candidates))
         (annotate (drover-attention--annotator candidates))
         (group (lambda (name transform)
                  (if transform
                      name
                    (drover-status-label (drover-session-status (cdr (assoc name candidates)))))))
         (table (lambda (string predicate action)
                  (if (eq action 'metadata)
                      `(metadata (category . drover-session)
                                 (display-sort-function . identity)
                                 (cycle-sort-function . identity)
                                 (group-function . ,group)
                                 (annotation-function . ,annotate))
                    (complete-with-action action names string predicate))))
         (name (completing-read prompt table nil t)))
    (or (cdr (assoc name candidates))
        (user-error "No session %s" name))))

;;;###autoload
(defun drover-select (&optional other-window)
  "Choose a session with completion and display it.
Sessions are the visible ones (see `drover-visible-sessions'), most
urgent first.  With a prefix argument OTHER-WINDOW, display it in
another window."
  (interactive "P")
  (let ((sessions (drover-sort (drover-visible-sessions))))
    (unless sessions
      (user-error "No sessions"))
    (drover-open (drover-read-session "Session: " sessions) other-window)))

;;;; Catching up

(defcustom drover-catch-up-statuses '(done failed)
  "Statuses of the sessions `drover-mark-all-seen' marks seen."
  :type '(repeat symbol)
  :group 'drover)

;;;###autoload
(defun drover-mark-all-seen (&optional older-than)
  "Mark seen every session whose turn ended unseen, after confirmation.
Those are the sessions with a status in `drover-catch-up-statuses' whose
backend can mark them seen.  With a numeric prefix argument OLDER-THAN,
only sessions whose status began at least that many hours ago."
  (interactive "P")
  (let* ((hours (and older-than (prefix-numeric-value older-than)))
         (limit (and hours (- (float-time) (* hours 3600))))
         (sessions (seq-filter
                    (lambda (session)
                      (and (memq (drover-session-status session) drover-catch-up-statuses)
                           (drover-supports-p session :mark-seen)
                           (or (null limit)
                               (let ((since (drover-session-since session)))
                                 (and since (<= since limit))))))
                    (drover-sessions))))
    (cond
     ((null sessions) (message "No session to mark seen"))
     ((y-or-n-p (format "Mark %d session%s seen%s? "
                        (length sessions) (if (cdr sessions) "s" "")
                        (if hours (format " (ended %d hour%s ago or more)" hours (if (= hours 1) "" "s")) "")))
      (dolist (session sessions)
        (with-demoted-errors "Drover mark seen: %S"
          (drover-mark-seen session)))
      (message "Marked %d session%s seen" (length sessions) (if (cdr sessions) "s" ""))))))

(defun drover-attention-unload-function ()
  "Disable `drover-attention-mode' before unloading."
  (drover-attention-mode -1)
  nil)

(provide 'drover-attention)
;;; drover-attention.el ends here
