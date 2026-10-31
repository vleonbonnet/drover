;;; drover-sidebar-test.el --- Tests of the sidebar  -*- lexical-binding: t; -*-

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

;; Rendering is tested by drawing the sidebar buffer at a given width
;; and time; windows are tested on the single frame of batch Emacs,
;; where side windows work but redisplay, and so the window change
;; functions, never runs: the tests call those functions themselves.
;;
;; Batch Emacs measures text as a terminal does, one pixel per column,
;; so most tests read widths as columns.  Tests of proportional text
;; measure it with a fake font instead (`drover-sidebar-test-proportional').

;;; Code:

(require 'drover-test-helpers)
(require 'drover-sidebar)
;; Loaded before tests stub its functions, which loading would undo.
(require 'drover-attention)
(require 'ol)

(defmacro drover-sidebar-test-with (sessions &rest body)
  "Run BODY with the fake backend serving SESSIONS and default settings.
Kill the sidebar and the session buffers, and restore one window, after."
  (declare (indent 1) (debug t))
  `(drover-test-with-backend ,sessions
     (let ((drover-sidebar-age-interval nil)
           (drover-sidebar-folded-statuses nil)
           (drover-sidebar-side 'left)
           (drover-sidebar-width 36)
           (drover-sidebar-slot 0)
           (drover-status-order '(waiting failed done retrying working idle))
           (drover-store-link-functions nil)
           ;; Readable glyphs, whatever the defaults.
           (drover-status-glyphs '((waiting . "⚑") (failed . "✗") (done . "✓")
                                   (retrying . "↻") (working . "◐") (idle . "·"))))
       (unwind-protect
           (progn ,@body)
         (drover-sidebar-test--cleanup)))))

(defun drover-sidebar-test--cleanup ()
  "Kill the buffers the tests made and leave one window."
  ;; A test failing within a menu leaves it open, holding the keys.
  (when (bound-and-true-p transient--prefix)
    (ignore-errors (execute-kbd-macro (kbd "C-g"))))
  (dolist (buffer (buffer-list))
    ;; Killing the sidebar kills its measuring buffer too.
    (when (and (buffer-live-p buffer)
               (string-match-p "\\`\\*\\(drover \\|session \\|other\\)" (buffer-name buffer)))
      (kill-buffer buffer)))
  (let ((ignore-window-parameters t))
    (delete-other-windows (seq-find (lambda (window) (not (window-parameter window 'window-side)))
                                    (window-list nil 'nomini))))
  (set-window-dedicated-p (selected-window) nil)
  (switch-to-buffer "*scratch*"))

(defun drover-sidebar-test--type (keys)
  "Type KEYS, a key description, in the selected window.
They run through the command loop, as the user's keys do."
  (execute-kbd-macro (kbd keys)))

(defun drover-sidebar-test--render (&optional width)
  "Draw the sidebar buffer WIDTH columns wide, at `drover-test-now'; return it."
  (let ((buffer (drover-sidebar--buffer)))
    (with-current-buffer buffer
      (drover-sidebar--render (or width 34) drover-test-now))
    buffer))

(defun drover-sidebar-test--lines ()
  "Return the lines of the current buffer, without properties."
  (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n" t))

(defun drover-sidebar-test--goto (id)
  "Move to the line of session ID in the current buffer."
  (goto-char (or (drover-sidebar--find-line
                  'session (lambda (start)
                             (equal (get-text-property start 'drover-sidebar-key) (cons 'fake id))))
                 (error "No line for session %s" id))))

(defun drover-sidebar-test--key ()
  "Return the identifier of the session at point in the current buffer, or nil."
  (cdr (get-text-property (line-beginning-position) 'drover-sidebar-key)))

(defun drover-sidebar-test--key-at (position)
  "Return the identifier of the session at POSITION in the current buffer, or nil."
  (save-excursion
    (goto-char position)
    (drover-sidebar-test--key)))

(defun drover-sidebar-test--session-buffer-name (session)
  "Return the name of the buffer of SESSION."
  (format "*session %s*" (drover-session-id session)))

(defun drover-sidebar-test--register-buffers ()
  "Register the fake backend opening sessions in buffers, as opencode.el does.
Each :open call records the selected window."
  (drover-register-backend
   'fake
   :label "Fake"
   :sessions (lambda () drover-test-sessions)
   :start (lambda () (push '(:start nil) drover-test-calls))
   :open (lambda (session other-window)
           (push (list :open (drover-session-id session) other-window (selected-window))
                 drover-test-calls)
           (let ((buffer (get-buffer-create (drover-sidebar-test--session-buffer-name session))))
             (if other-window (pop-to-buffer buffer) (switch-to-buffer buffer))))
   :buffer (lambda (session)
             (get-buffer (drover-sidebar-test--session-buffer-name session)))
   :respond (drover-test--recorder :respond)))

(defun drover-sidebar-test--main ()
  "Split the selected window and return the two halves, the first selected.
Each shows its own buffer."
  (let* ((first (selected-window))
         (second (split-window first nil 'below)))
    (set-window-buffer first (get-buffer-create "*other 1*"))
    (set-window-buffer second (get-buffer-create "*other 2*"))
    (select-window second)
    (select-window first)
    (list first second)))

(defun drover-sidebar-test--sample ()
  "Return sessions of every status."
  (list (drover-test-session "idle" :status 'idle :updated (- drover-test-now 86400))
        (drover-test-session "work" :status 'working :updated (- drover-test-now 60)
                             :since (- drover-test-now 600))
        (drover-test-session "ask" :status 'waiting :since (- drover-test-now 180)
                             :requests '((:kind question :summary "Which branch?")))
        (drover-test-session "done" :status 'done :since (- drover-test-now 720))
        (drover-test-session "fail" :status 'failed :since (- drover-test-now 30)
                             :detail "Rate limited")))

;;;; Rendering

(ert-deftest drover-sidebar-test-groups-in-status-order ()
  (drover-sidebar-test-with (drover-sidebar-test--sample)
    (with-current-buffer (drover-sidebar-test--render)
      (should (equal (seq-filter (lambda (line) (string-match-p "\\`[▾▸]" line))
                                 (drover-sidebar-test--lines))
                     '("▾ Needs you 1" "▾ Failed 1" "▾ Done 1" "▾ Working 1" "▾ Idle 1"))))))

(ert-deftest drover-sidebar-test-counts-and-empty-groups ()
  (drover-sidebar-test-with (list (drover-test-session "w1" :status 'working :updated 10.0)
                                  (drover-test-session "w2" :status 'working :updated 20.0)
                                  (drover-test-session "i" :status 'idle))
    (with-current-buffer (drover-sidebar-test--render)
      (let ((lines (drover-sidebar-test--lines)))
        (should (equal (length lines) 5))
        (should (equal (nth 0 lines) "▾ Working 2"))
        (should (string-prefix-p "  ◐ Session w2" (nth 1 lines)))
        (should (string-prefix-p "  ◐ Session w1" (nth 2 lines)))
        (should (equal (nth 3 lines) "▾ Idle 1"))))))

(ert-deftest drover-sidebar-test-idle-group-last ()
  (drover-sidebar-test-with (drover-sidebar-test--sample)
    (let ((drover-status-order '(idle waiting failed done retrying working)))
      (with-current-buffer (drover-sidebar-test--render)
        (should (equal (car (last (seq-filter (lambda (line) (string-prefix-p "▾" line))
                                              (drover-sidebar-test--lines))))
                       "▾ Idle 1"))
        (should (equal (car (drover-sidebar-test--lines)) "▾ Needs you 1"))))))

(ert-deftest drover-sidebar-test-heading-faces ()
  (drover-sidebar-test-with (drover-sidebar-test--sample)
    (with-current-buffer (drover-sidebar-test--render)
      (goto-char (point-min))
      (should (eq (get-text-property (point) 'face) 'drover-heading))
      (should (eq (get-text-property (1- (line-end-position)) 'face) 'drover-dim))
      (should (eq (drover-sidebar--line-type) 'heading)))))

(ert-deftest drover-sidebar-test-session-line ()
  (drover-sidebar-test-with (list (drover-test-session "a" :status 'working
                                                       :updated (- drover-test-now 180)))
    (with-current-buffer (drover-sidebar-test--render 30)
      (drover-sidebar-test--goto "a")
      (let ((line (buffer-substring (point) (line-end-position))))
        (should (equal (substring-no-properties line) "  ◐ Session a 3m"))
        ;; The indentation is one blank, as wide as the heading's fold glyph.
        (should (equal (get-text-property 0 'display line) '(space :width (2))))
        (should (eq (next-single-property-change 0 'display line) 2))
        (should (eq (get-text-property 2 'face line) 'drover-working))
        (should (null (get-text-property 4 'face line)))
        ;; The age sits flush right, aligned by display, in pixels.
        (should (equal (get-text-property 13 'display line) '(space :align-to (28))))
        (should (eq (get-text-property 14 'face line) 'drover-dim))
        (should (eq (get-text-property 14 'font-lock-face line) 'drover-dim))
        (should (equal (get-text-property 0 'help-echo line) "Session a"))
        (should (eq (get-text-property 0 'mouse-face line) 'highlight))))))

(ert-deftest drover-sidebar-test-truncates-title ()
  (let ((id "with-a-title-much-too-long-for-the-sidebar"))
    (drover-sidebar-test-with (list (drover-test-session id :status 'done
                                                         :since (- drover-test-now 7200)))
      (dolist (width '(30 40 20))
        (with-current-buffer (drover-sidebar-test--render width)
          (drover-sidebar-test--goto id)
          (let* ((line (buffer-substring-no-properties (point) (line-end-position)))
                 (title (substring line 4 (- (length line) 3))))
            (should (string-suffix-p " 2h" line))
            (should (string-prefix-p "Session with" title))
            (should (string-suffix-p "…" title))
            ;; Margin, glyph, space, title, space and age fill the width.
            (should (= (+ 2 (string-width "✓") 1 (string-width title) 1 2) width))))))))

(ert-deftest drover-sidebar-test-narrow-width ()
  (drover-sidebar-test-with (list (drover-test-session "a" :status 'idle))
    (with-current-buffer (drover-sidebar-test--render 3)
      (drover-sidebar-test--goto "a")
      (should (string-prefix-p "  · " (buffer-substring-no-properties (point) (line-end-position)))))))

(ert-deftest drover-sidebar-test-ages ()
  (drover-sidebar-test-with (drover-sidebar-test--sample)
    (with-current-buffer (drover-sidebar-test--render)
      (let ((age (lambda (id)
                   (drover-sidebar-test--goto id)
                   (car (last (split-string (buffer-substring-no-properties
                                             (point) (line-end-position))))))))
        ;; Sessions needing the user age from when their status began,
        ;; the others from their last activity.
        (should (equal (funcall age "ask") "3m"))
        (should (equal (funcall age "fail") "30s"))
        (should (equal (funcall age "work") "1m"))
        (should (equal (funcall age "idle") "1d"))))))

(ert-deftest drover-sidebar-test-no-age ()
  (drover-sidebar-test-with (list (drover-test-session "a" :status 'idle))
    (with-current-buffer (drover-sidebar-test--render)
      (drover-sidebar-test--goto "a")
      (should (equal (buffer-substring-no-properties (point) (line-end-position)) "  · Session a"))
      (should (eq (get-text-property (+ (point) 4) 'face) 'drover-dim)))))

(ert-deftest drover-sidebar-test-summary-line ()
  (drover-sidebar-test-with (drover-sidebar-test--sample)
    (with-current-buffer (drover-sidebar-test--render)
      (drover-sidebar-test--goto "ask")
      (forward-line 1)
      (should (eq (drover-sidebar--line-type) 'summary))
      (should (equal (buffer-substring-no-properties (point) (line-end-position))
                     "    asks: Which branch?"))
      (should (eq (get-text-property (+ (point) 4) 'face) 'drover-dim))
      (should (equal (drover-sidebar-test--key) "ask"))
      (drover-sidebar-test--goto "fail")
      (forward-line 1)
      (should (equal (buffer-substring-no-properties (point) (line-end-position))
                     "    Rate limited")))))

(ert-deftest drover-sidebar-test-summary-only-when-needed ()
  (drover-sidebar-test-with (list (drover-test-session "done" :status 'done)
                                  (drover-test-session "work" :status 'working :detail "Busy")
                                  (drover-test-session "retry" :status 'retrying :detail "Rate limited"))
    (with-current-buffer (drover-sidebar-test--render)
      (should-not (text-property-any (point-min) (point-max) 'drover-sidebar-line 'summary))
      (should (= (length (drover-sidebar-test--lines)) 6)))))

(ert-deftest drover-sidebar-test-summary-truncated ()
  (drover-sidebar-test-with
      (list (drover-test-session "a" :status 'waiting
                                 :requests '((:kind permission
                                                    :summary "bash: make test && make lint\nand much more"))))
    (with-current-buffer (drover-sidebar-test--render 30)
      (drover-sidebar-test--goto "a")
      (forward-line 1)
      (let ((line (buffer-substring (point) (line-end-position))))
        ;; Batch Emacs draws the flag two columns wide: the summary
        ;; starts under the title, past the glyph and one space.
        (should (equal (get-text-property 0 'display line) '(space :width (5))))
        (should (equal (substring-no-properties line) "    permit: bash: make test…"))
        ;; The cut drops the blank before the ellipsis, so the line may
        ;; end a column short of the width.
        (should (<= (+ 5 (string-width (substring line 4))) 30))
        (should (eq (get-text-property (1- (length line)) 'face line) 'drover-dim))))))

(ert-deftest drover-sidebar-test-help-echo ()
  (drover-sidebar-test-with (list (drover-test-session "a" :status 'waiting :project "drover"
                                                       :branch "main" :agent "build" :model "opus"
                                                       :requests '((:kind question :summary "Go?"))))
    (with-current-buffer (drover-sidebar-test--render)
      (drover-sidebar-test--goto "a")
      (should (equal (get-text-property (point) 'help-echo)
                     "Session a\ndrover:main\nbuild (opus)\nasks: Go?")))))

(ert-deftest drover-sidebar-test-empty-state ()
  (drover-sidebar-test-with nil
    (with-current-buffer (drover-sidebar-test--render)
      (should (equal (drover-sidebar-test--lines) '("No sessions.")))
      (should (eq (get-text-property (point-min) 'face) 'drover-dim))
      (should-not (drover-sidebar--session-at-point))
      (should-error (drover-session-at-point) :type 'user-error)
      (should-not (drover-sidebar-next))
      (should (= (point) (point-min))))))

(ert-deftest drover-sidebar-test-empty-state-without-hint ()
  (drover-sidebar-test-with nil
    (with-current-buffer (drover-sidebar-test--render)
      (should-not (text-property-any (point-min) (point-max) 'drover-sidebar-line 'hint)))))

;;;; Navigation and the session at point

(ert-deftest drover-sidebar-test-next-previous-skip-headings ()
  (drover-sidebar-test-with (drover-sidebar-test--sample)
    (with-current-buffer (drover-sidebar-test--render)
      (goto-char (point-min))
      (let ((visited nil))
        (dotimes (_ 5)
          (drover-sidebar-next)
          (should (eq (drover-sidebar--line-type) 'session))
          (should (= (point) (line-beginning-position)))
          (push (drover-sidebar-test--key) visited))
        (should (equal (nreverse visited) '("ask" "fail" "done" "work" "idle"))))
      (should-not (drover-sidebar-next))
      (should (equal (drover-sidebar-test--key) "idle"))
      (drover-sidebar-previous 2)
      (should (equal (drover-sidebar-test--key) "done"))
      (drover-sidebar-previous 10)
      (should (equal (drover-sidebar-test--key) "ask"))
      (should-not (drover-sidebar-previous))
      (should (equal (drover-sidebar-test--key) "ask")))))

(ert-deftest drover-sidebar-test-previous-from-summary ()
  (drover-sidebar-test-with (drover-sidebar-test--sample)
    (with-current-buffer (drover-sidebar-test--render)
      (drover-sidebar-test--goto "fail")
      (forward-line 1)
      (drover-sidebar-previous)
      (should (eq (drover-sidebar--line-type) 'session))
      (should (equal (drover-sidebar-test--key) "fail"))
      (forward-line 1)
      (drover-sidebar-next)
      (should (equal (drover-sidebar-test--key) "done")))))

(ert-deftest drover-sidebar-test-group-navigation ()
  (drover-sidebar-test-with (drover-sidebar-test--sample)
    (with-current-buffer (drover-sidebar-test--render)
      (drover-sidebar-test--goto "done")
      (drover-sidebar-next-group)
      (should (eq (drover-sidebar--line-type) 'heading))
      (should (equal (get-text-property (point) 'drover-sidebar-group) '(status . working)))
      (drover-sidebar-previous-group 2)
      (should (equal (get-text-property (point) 'drover-sidebar-group) '(status . failed))))))

(ert-deftest drover-sidebar-test-session-at-point-is-fresh ()
  (drover-sidebar-test-with (drover-sidebar-test--sample)
    (with-current-buffer (drover-sidebar-test--render)
      (goto-char (point-min))
      (should-not (drover-sidebar--session-at-point))
      (drover-sidebar-test--goto "work")
      (let ((fresh (drover-test-session "work" :status 'done)))
        (setq drover-test-sessions
              (cons fresh (seq-remove (lambda (session) (equal (drover-session-id session) "work"))
                                      drover-test-sessions)))
        (should (eq (drover-session-at-point) fresh)))
      (setq drover-test-sessions nil)
      (should-not (drover-sidebar--session-at-point)))))

(ert-deftest drover-sidebar-test-session-at-point-on-summary ()
  (drover-sidebar-test-with (drover-sidebar-test--sample)
    (with-current-buffer (drover-sidebar-test--render)
      (drover-sidebar-test--goto "ask")
      (forward-line 1)
      (end-of-line)
      (should (equal (drover-session-id (drover-session-at-point)) "ask")))))

;;;; Folding

(ert-deftest drover-sidebar-test-fold-group ()
  (drover-sidebar-test-with (append (list (drover-test-session "idle2" :status 'idle))
                                    (drover-sidebar-test--sample))
    (with-current-buffer (drover-sidebar-test--render)
      (drover-sidebar-test--goto "idle2")
      (drover-sidebar-toggle-group)
      (should (eq (drover-sidebar--line-type) 'heading))
      (should (equal (buffer-substring-no-properties (point) (line-end-position)) "▸ Idle 2"))
      (should-not (string-search "Session idle" (buffer-string)))
      (drover-sidebar-test--goto "work")
      (should-not (drover-sidebar-next))
      ;; The fold outlives redraws.
      (drover-sidebar--redraw)
      (should-not (string-search "Session idle" (buffer-string)))
      (goto-char (point-max))
      (drover-sidebar-previous-group)
      (drover-sidebar-toggle-group)
      (should (equal (buffer-substring-no-properties (point) (line-end-position)) "▾ Idle 2"))
      (should (string-search "Session idle2" (buffer-string))))))

(ert-deftest drover-sidebar-test-fold-with-ret-and-keys ()
  (drover-sidebar-test-with (drover-sidebar-test--sample)
    (with-current-buffer (drover-sidebar-test--render)
      (goto-char (point-min))
      (call-interactively (key-binding (kbd "RET")))
      (should (equal (buffer-substring-no-properties (point) (line-end-position)) "▸ Needs you 1"))
      (call-interactively (key-binding (kbd "TAB")))
      (should (equal (buffer-substring-no-properties (point) (line-end-position)) "▾ Needs you 1")))))

(ert-deftest drover-sidebar-test-ret-folds-only-on-headings ()
  "RET folds a group on its heading, and opens the session anywhere else."
  (drover-sidebar-test-with (drover-sidebar-test--sample)
    (with-current-buffer (drover-sidebar-test--render)
      (dolist (heading (drover-sidebar--heading-starts))
        (goto-char heading)
        (should (eq (key-binding (kbd "RET")) #'drover-sidebar-toggle-group))
        (end-of-line)
        (should (eq (key-binding (kbd "RET")) #'drover-sidebar-toggle-group))
        ;; The line after a heading is a session's, from its start.
        (forward-line 1)
        (should (eq (drover-sidebar--line-type) 'session))
        (should (eq (key-binding (kbd "RET")) #'drover-open-at-point)))
      ;; A session's summary line opens it too.
      (drover-sidebar-test--goto "ask")
      (forward-line 1)
      (should (eq (key-binding (kbd "RET")) #'drover-open-at-point))
      (call-interactively (key-binding (kbd "RET")))
      (should (equal drover-test-calls '((:open "ask" nil)))))))

(ert-deftest drover-sidebar-test-fold-from-rolled-up-parent ()
  "Folding from a session folds the group it is listed in, not its status's."
  (drover-sidebar-test-with (drover-sidebar-test--asking-child)
    (with-current-buffer (drover-sidebar-test--render)
      ;; The parent is done, but listed under "Needs you" for its subagent.
      (drover-sidebar-test--goto "parent")
      (drover-sidebar-toggle-group)
      (should (equal (buffer-substring-no-properties (point) (line-end-position)) "▸ Needs you 2"))
      (should-not (string-search "Done" (buffer-string))))))

(ert-deftest drover-sidebar-test-fold-all ()
  (drover-sidebar-test-with (drover-sidebar-test--sample)
    (with-current-buffer (drover-sidebar-test--render)
      (drover-sidebar-toggle-all-groups)
      (should (equal (drover-sidebar-test--lines)
                     '("▸ Needs you 1" "▸ Failed 1" "▸ Done 1" "▸ Working 1" "▸ Idle 1")))
      (drover-sidebar-toggle-all-groups)
      (should (= (length (drover-sidebar-test--lines)) 12)))))

(ert-deftest drover-sidebar-test-initially-folded ()
  (drover-sidebar-test-with (drover-sidebar-test--sample)
    (let ((drover-sidebar-folded-statuses '(idle)))
      (with-current-buffer (drover-sidebar-test--render)
        (should (equal (car (last (drover-sidebar-test--lines))) "▸ Idle 1"))))))

(ert-deftest drover-sidebar-test-toggle-group-off-group ()
  (drover-sidebar-test-with nil
    (with-current-buffer (drover-sidebar-test--render)
      (should-error (drover-sidebar-toggle-group) :type 'user-error))))

;;;; Subagents

(defun drover-sidebar-test--next ()
  "Move to the next session; return non-nil when point moved."
  (let ((before (point)))
    (drover-sidebar-next)
    (/= before (point))))

(defun drover-sidebar-test--indent ()
  "Return the width, in pixels, of the indentation of the line at point."
  (pcase (get-text-property (line-beginning-position) 'display)
    (`(space :width (,pixels)) pixels)))

(defun drover-sidebar-test--family ()
  "Return three generations of working or idle sessions, and a stranger."
  (list (drover-test-session "grand" :parent "child" :status 'idle :updated (- drover-test-now 300))
        (drover-test-session "root" :status 'working :updated (- drover-test-now 60))
        (drover-test-session "other" :status 'working :updated (- drover-test-now 30))
        (drover-test-session "child" :parent "root" :status 'working :updated (- drover-test-now 120))))

(defun drover-sidebar-test--asking-child ()
  "Return a parent done, its subagent asking, and two other sessions."
  (list (drover-test-session "busy" :status 'working)
        (drover-test-session "parent" :status 'done :since (- drover-test-now 50))
        (drover-test-session "asking" :parent "parent" :status 'waiting :since (- drover-test-now 30)
                             :requests '((:kind question :summary "Which file?")))
        (drover-test-session "plain" :status 'waiting :since (- drover-test-now 40))))

(ert-deftest drover-sidebar-test-subagents-under-parent ()
  (drover-sidebar-test-with (drover-sidebar-test--family)
    (let ((drover-show-children t))
      (with-current-buffer (drover-sidebar-test--render)
        (should (equal (drover-sidebar-test--lines)
                       '("▾ Working 2"
                         "  ◐ Session other 30s"
                         "  ◐ Session root 1m"
                         "    ◐ Session child 2m"
                         "      · Session grand 5m")))
        ;; Each glyph sits under the title above it: past the glyph above
        ;; and one space.
        (should (equal (mapcar (lambda (id)
                                 (drover-sidebar-test--goto id)
                                 (drover-sidebar-test--indent))
                               '("other" "root" "child" "grand"))
                       '(2 2 4 6)))
        ;; Every session keeps its own glyph and age.
        (drover-sidebar-test--goto "grand")
        (should (eq (get-text-property (+ (point) 6) 'face) 'drover-idle))
        (should (eq (get-text-property (+ (point) 8) 'face) 'drover-dim))))))

(ert-deftest drover-sidebar-test-subagent-under-wide-glyph ()
  "A subagent starts under its parent's title, however wide its glyph."
  (drover-sidebar-test-with (list (drover-test-session "parent" :status 'waiting)
                                  (drover-test-session "child" :parent "parent" :status 'idle))
    (let ((drover-show-children t))
      (with-current-buffer (drover-sidebar-test--render)
        (drover-sidebar-test--goto "child")
        ;; Batch Emacs draws the flag two columns wide.
        (should (= (drover-sidebar-test--indent) 5))
        (should (equal (buffer-substring-no-properties (point) (line-end-position))
                       "    · Session child"))))))

(ert-deftest drover-sidebar-test-nesting-depth-capped ()
  (drover-sidebar-test-with
      (cons (drover-test-session "d0" :status 'idle)
            (mapcar (lambda (depth)
                      (drover-test-session (format "d%d" depth) :status 'idle
                                           :parent (format "d%d" (1- depth))))
                    (number-sequence 1 5)))
    (let ((drover-show-children t))
      (with-current-buffer (drover-sidebar-test--render)
        (should (equal (mapcar (lambda (depth)
                                 (drover-sidebar-test--goto (format "d%d" depth))
                                 (list (drover-sidebar-test--indent)
                                       (string-search "·" (buffer-substring-no-properties
                                                           (point) (line-end-position)))))
                               (number-sequence 0 5))
                       '((2 2) (4 4) (6 6) (8 8) (8 8) (8 8))))))))

(ert-deftest drover-sidebar-test-parent-rolls-up ()
  "A parent whose subagent asks shows with the sessions needing the user."
  (drover-sidebar-test-with (drover-sidebar-test--asking-child)
    (with-current-buffer (drover-sidebar-test--render)
      (should (equal (drover-sidebar-test--lines)
                     '("▾ Needs you 2"
                       "  ⚑ Session plain 40s"
                       "  ✓ Session parent 50s"
                       "    ⚑ Session asking 30s"
                       "      asks: Which file?"
                       "▾ Working 1"
                       "  ◐ Session busy")))
      ;; The parent keeps its own glyph; the subagent's summary starts
      ;; under its title.
      (drover-sidebar-test--goto "parent")
      (should (eq (get-text-property (+ (point) 2) 'face) 'drover-done))
      (drover-sidebar-test--goto "asking")
      (forward-line 1)
      (should (eq (drover-sidebar--line-type) 'summary))
      (should (= (drover-sidebar-test--indent) (+ 4 2 1)))
      (should (equal (drover-sidebar-test--key) "asking")))))

(ert-deftest drover-sidebar-test-orphan-subagent ()
  (drover-sidebar-test-with (list (drover-test-session "orphan" :parent "gone" :status 'idle
                                                       :updated (- drover-test-now 60))
                                  (drover-test-session "root" :status 'idle :updated (- drover-test-now 30)))
    (with-current-buffer (drover-sidebar-test--render)
      ;; Its parent is not listed: it shows at the top level, marked.
      (should (equal (drover-sidebar-test--lines)
                     '("▾ Idle 2" "  · Session root 30s" "  · ↳ Session orphan 1m")))
      (drover-sidebar-test--goto "orphan")
      (should (= (drover-sidebar-test--indent) 2))
      (should (eq (get-text-property (+ (point) 4) 'face) 'drover-dim))
      (should-not (text-property-any (point-min) (point-max) 'drover-sidebar-line 'hint)))))

(ert-deftest drover-sidebar-test-toggle-subagents ()
  (drover-sidebar-test-with (list (drover-test-session "root" :status 'idle)
                                  (drover-test-session "quiet" :parent "root" :status 'idle)
                                  (drover-test-session "asking" :parent "root" :status 'waiting))
    (with-current-buffer (window-buffer (drover-sidebar-select))
      ;; A subagent needing the user shows under its parent; the others
      ;; are counted by the hint closing the list, which names the key.
      (should (equal (drover-sidebar-test--lines)
                     '("▾ Needs you 1" "  · Session root" "    ⚑ Session asking"
                       "U shows 1 subagent.")))
      (goto-char (point-max))
      (forward-line -1)
      (should (eq (drover-sidebar--line-type) 'hint))
      (should (eq (get-text-property (point) 'face) 'drover-dim))
      (drover-sidebar-test--goto "asking")
      (drover-sidebar-test--type "U")
      ;; Every view follows: the setting is global.
      (should (default-value 'drover-show-children))
      (should-not (local-variable-p 'drover-show-children))
      (should (equal (drover-sidebar-test--lines)
                     '("▾ Needs you 1" "  · Session root" "    ⚑ Session asking"
                       "    · Session quiet")))
      (should (equal (drover-sidebar-test--key) "asking"))
      (should (equal (drover-sidebar--mode-line-flags) " +sub"))
      ;; Point on a subagent the toggle hides moves to its parent.
      (drover-sidebar-test--goto "quiet")
      (drover-sidebar-test--type "U")
      (should-not drover-show-children)
      (should-not (string-search "Session quiet" (buffer-string)))
      (should (equal (drover-sidebar-test--key) "root"))
      (should (equal (drover-sidebar-test--key-at (window-point (selected-window))) "root")))))

(ert-deftest drover-sidebar-test-hint-counts-hidden-subagents ()
  (drover-sidebar-test-with (append (list (drover-test-session "a" :parent "root")
                                          (drover-test-session "b" :parent "a"))
                                    (drover-sidebar-test--family))
    (with-current-buffer (drover-sidebar-test--render)
      ;; child, grand, a and b, all below root, none needing the user.
      (should (equal (car (last (drover-sidebar-test--lines))) "U shows 4 subagents."))
      ;; Moving never lands on the hint.
      (goto-char (point-min))
      (let ((moves 0))
        (while (drover-sidebar-test--next)
          (setq moves (1+ moves))
          (should (eq (drover-sidebar--line-type) 'session)))
        (should (= moves 2)))
      (should (equal (drover-sidebar-test--key) "root")))))

(ert-deftest drover-sidebar-test-navigation-over-nested-lines ()
  (drover-sidebar-test-with (drover-sidebar-test--asking-child)
    (with-current-buffer (drover-sidebar-test--render)
      (goto-char (point-min))
      (let ((visited nil))
        (while (drover-sidebar-test--next)
          (should (= (point) (line-beginning-position)))
          (push (drover-sidebar-test--key) visited))
        (should (equal (nreverse visited) '("plain" "parent" "asking" "busy"))))
      ;; From a subagent's summary, back to the subagent, then its parent.
      (drover-sidebar-test--goto "asking")
      (forward-line 1)
      (drover-sidebar-previous)
      (should (equal (drover-sidebar-test--key) "asking"))
      (drover-sidebar-previous)
      (should (equal (drover-sidebar-test--key) "parent"))
      (should (equal (drover-session-id (drover-session-at-point)) "parent")))))

(ert-deftest drover-sidebar-test-point-stays-on-nested-session ()
  (drover-sidebar-test-with (drover-sidebar-test--asking-child)
    (with-current-buffer (drover-sidebar-test--render)
      (setq drover-show-children t)
      (drover-sidebar--redraw)
      (drover-sidebar-test--goto "asking")
      ;; The subagent stops waiting: its tree moves to the parent's group.
      (setf (drover-session-status (drover-find 'fake "asking")) 'working
            (drover-session-requests (drover-find 'fake "asking")) nil)
      (drover-sidebar--redraw)
      (should (equal (drover-sidebar-test--key) "asking"))
      (should (eq (drover-session-status (get-text-property (point) 'drover-sidebar-session)) 'working))
      (should (equal (drover-sidebar--search -1 '(heading))
                     (drover-sidebar--find-line
                      'heading (lambda (start)
                                 (equal (get-text-property start 'drover-sidebar-group) '(status . done))))))
      ;; Hidden once subagents are, it leaves point on its parent.
      (setq drover-show-children nil)
      (drover-sidebar--redraw)
      (should (equal (drover-sidebar-test--key) "parent")))))

(ert-deftest drover-sidebar-test-fold-hides-subagents ()
  (drover-sidebar-test-with (drover-sidebar-test--asking-child)
    (with-current-buffer (drover-sidebar-test--render)
      (goto-char (point-min))
      (drover-sidebar-toggle-group)
      (should (equal (drover-sidebar-test--lines) '("▸ Needs you 2" "▾ Working 1" "  ◐ Session busy")))
      (goto-char (point-min))
      (drover-sidebar-next)
      (should (equal (drover-sidebar-test--key) "busy")))))

;;;; Grouping and filtering

(defun drover-sidebar-test--projects ()
  "Return sessions of two projects and of none, with a hidden subagent."
  (list (drover-test-session "a-idle" :project "alpha" :status 'idle :updated (- drover-test-now 500))
        (drover-test-session "b-work" :project "beta" :status 'working :updated (- drover-test-now 60))
        (drover-test-session "a-ask" :project "alpha" :status 'waiting :since (- drover-test-now 100)
                             :requests '((:kind question :summary "Which?")))
        (drover-test-session "none" :status 'done :since (- drover-test-now 50))
        (drover-test-session "b-kid" :parent "b-work" :project "beta" :status 'idle)))

(defun drover-sidebar-test--headings ()
  "Return the heading lines of the current buffer, without properties."
  (mapcar (lambda (start)
            (buffer-substring-no-properties start (save-excursion (goto-char start) (line-end-position))))
          (drover-sidebar--heading-starts)))

(defun drover-sidebar-test--order ()
  "Return the identifiers of the sessions listed in the current buffer, in order."
  (save-excursion
    (goto-char (point-min))
    (let (ids)
      (while (not (eobp))
        (when (eq (drover-sidebar--line-type) 'session)
          (push (drover-sidebar-test--key) ids))
        (forward-line 1))
      (nreverse ids))))

(defun drover-sidebar-test--line ()
  "Return the line at point, without properties."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun drover-sidebar-test--on (id)
  "Return non-nil when point, and the selected window's, are on session ID."
  (and (equal (drover-sidebar-test--key) id)
       (equal (drover-sidebar-test--key-at (window-point (selected-window))) id)))

(ert-deftest drover-sidebar-test-group-by-project ()
  (drover-sidebar-test-with (drover-sidebar-test--projects)
    (with-current-buffer (window-buffer (drover-sidebar-select))
      (should (equal (drover-sidebar-test--headings)
                     '("▾ Needs you 1" "▾ Done 1" "▾ Working 1" "▾ Idle 1")))
      (drover-sidebar-test--goto "b-work")
      (drover-set-group-by 'project)
      ;; Projects come in the order of their most urgent tree.
      (should (equal (drover-sidebar-test--headings) '("▾ alpha 2" "▾ No project 1" "▾ beta 1")))
      (should (equal (drover-sidebar-test--order) '("a-ask" "a-idle" "none" "b-work")))
      (should (equal (car (last (drover-sidebar-test--lines))) "U shows 1 subagent."))
      (should (drover-sidebar-test--on "b-work"))
      (goto-char (point-min))
      (should (equal (drover-sidebar-test--line) "▾ alpha 2"))
      (should (eq (get-text-property (point) 'face) 'drover-heading))
      (drover-sidebar-test--goto "a-ask")
      (should (string-prefix-p "  ⚑ Session a-ask" (drover-sidebar-test--line)))
      ;; Moving between sessions crosses groups; between groups too.
      (drover-sidebar-test--type "n")
      (should (drover-sidebar-test--on "a-idle"))
      (drover-sidebar-test--type "n")
      (should (drover-sidebar-test--on "none"))
      (drover-sidebar-test--type "M-n")
      (should (equal (drover-sidebar-test--line) "▾ beta 1"))
      (drover-sidebar-test--type "p")
      (should (drover-sidebar-test--on "none"))
      ;; Folding a project, from one of its sessions or its heading.
      (drover-sidebar-test--goto "a-idle")
      (drover-sidebar-test--type "TAB")
      (should (equal (drover-sidebar-test--line) "▸ alpha 2"))
      (should (equal (drover-sidebar-test--order) '("none" "b-work")))
      (drover-sidebar-test--type "n")
      (should (drover-sidebar-test--on "none"))
      (drover-sidebar-test--type "M-p")
      (drover-sidebar-test--type "M-p")
      (should (equal (drover-sidebar-test--line) "▸ alpha 2"))
      (drover-sidebar-test--type "RET")
      (should (equal (drover-sidebar-test--line) "▾ alpha 2"))
      (drover-sidebar-test--type "<backtab>")
      (should (equal (drover-sidebar-test--headings) '("▸ alpha 2" "▸ No project 1" "▸ beta 1")))
      (drover-sidebar-test--type "<backtab>")
      (should (equal (drover-sidebar-test--headings) '("▾ alpha 2" "▾ No project 1" "▾ beta 1"))))))

(ert-deftest drover-sidebar-test-folds-belong-to-their-grouping ()
  (drover-sidebar-test-with (drover-sidebar-test--projects)
    (with-current-buffer (window-buffer (drover-sidebar-select))
      (drover-sidebar-test--goto "a-idle")
      (drover-sidebar-test--type "TAB")
      (should (equal (drover-sidebar-test--line) "▸ Idle 1"))
      (drover-sidebar-test--goto "a-ask")
      (drover-set-group-by 'project)
      (should (equal (drover-sidebar-test--headings) '("▾ alpha 2" "▾ No project 1" "▾ beta 1")))
      (should (drover-sidebar-test--on "a-ask"))
      (drover-sidebar-test--type "TAB")
      (drover-sidebar-test--goto "none")
      (drover-set-group-by 'status)
      (should (equal (drover-sidebar-test--headings)
                     '("▾ Needs you 1" "▾ Done 1" "▾ Working 1" "▸ Idle 1")))
      (should (drover-sidebar-test--on "none"))
      (drover-set-group-by 'project)
      (should (equal (drover-sidebar-test--headings) '("▸ alpha 2" "▾ No project 1" "▾ beta 1")))
      (should (drover-sidebar-test--on "none")))))

(ert-deftest drover-sidebar-test-no-grouping ()
  (drover-sidebar-test-with (drover-sidebar-test--projects)
    (with-current-buffer (window-buffer (drover-sidebar-select))
      (drover-sidebar-test--goto "b-work")
      (drover-set-group-by nil)
      (should-not (drover-sidebar-test--headings))
      ;; Trees most urgent first, the top level against the edge.
      (should (equal (drover-sidebar-test--order) '("a-ask" "none" "b-work" "a-idle")))
      (should (drover-sidebar-test--on "b-work"))
      (should (equal (mapcar (lambda (line) (replace-regexp-in-string " [0-9]+[smhd]\\'" "" line))
                             (drover-sidebar-test--lines))
                     '("⚑ Session a-ask" "  asks: Which?" "✓ Session none"
                       "◐ Session b-work" "· Session a-idle" "U shows 1 subagent.")))
      (goto-char (point-min))
      (should (eq (key-binding (kbd "RET")) #'drover-open-at-point))
      (should-error (drover-sidebar-test--type "TAB") :type 'user-error)
      (should-error (drover-sidebar-test--type "<backtab>") :type 'user-error)
      (drover-sidebar-test--type "n")
      (should (drover-sidebar-test--on "none"))
      (drover-sidebar-test--type "n")
      (drover-sidebar-test--type "n")
      (should (drover-sidebar-test--on "a-idle"))
      (should-not (drover-sidebar-next))
      (drover-sidebar-test--type "p")
      (should (drover-sidebar-test--on "b-work"))
      ;; A subagent still starts under its parent's title.
      (drover-sidebar-test--type "U")
      (should (drover-sidebar-test--on "b-work"))
      (drover-sidebar-test--goto "b-kid")
      (should (equal (drover-sidebar-test--line) "  · Session b-kid"))
      (should (= (drover-sidebar-test--indent) 2))
      (drover-set-group-by 'status)
      (should (drover-sidebar-test--on "b-kid"))
      (should (equal (drover-sidebar-test--line) "    · Session b-kid")))))

(ert-deftest drover-sidebar-test-filter-line ()
  (drover-sidebar-test-with (drover-sidebar-test--projects)
    (let ((drover-filter '(:project "alpha")))
      (with-current-buffer (drover-sidebar-test--render)
        (should (equal (car (drover-sidebar-test--lines)) "Filter: project alpha"))
        (goto-char (point-min))
        (should (eq (drover-sidebar--line-type) 'filter))
        (should (eq (get-text-property (point) 'face) 'drover-dim))
        (should (string-search "F c clears the filter." (get-text-property (point) 'help-echo)))
        (should (equal (drover-sidebar-test--headings) '("▾ Needs you 1" "▾ Idle 1")))
        (should (equal (drover-sidebar-test--order) '("a-ask" "a-idle")))
        ;; Moving skips it, and no session is there.
        (should-error (drover-session-at-point) :type 'user-error)
        (drover-sidebar-next)
        (should (equal (drover-sidebar-test--key) "a-ask"))
        (should-not (drover-sidebar-previous))
        (should (equal (drover-sidebar-test--key) "a-ask"))))))

(ert-deftest drover-sidebar-test-filter-shows-matching-subagent ()
  (drover-sidebar-test-with (drover-sidebar-test--projects)
    (let ((drover-filter '(:search "b-kid")))
      (with-current-buffer (drover-sidebar-test--render)
        (should (equal (drover-sidebar-test--lines)
                       '("Filter: \"b-kid\"" "▾ Working 1" "  ◐ Session b-work 1m"
                         "    · Session b-kid")))))))

(ert-deftest drover-sidebar-test-filter-matches-nothing ()
  (drover-sidebar-test-with (drover-sidebar-test--projects)
    (let ((drover-filter '(:project "gamma" :statuses (failed))))
      (with-current-buffer (drover-sidebar-test--render)
        (should (equal (drover-sidebar-test--lines)
                       '("Filter: project gamma, failed" "No session matches."
                         "F c clears the filter.")))
        (forward-line 1)
        (should (eq (drover-sidebar--line-type) 'empty))
        (should (eq (get-text-property (point) 'face) 'drover-dim))
        (forward-line 1)
        (should (eq (drover-sidebar--line-type) 'hint))
        (should-not (drover-sidebar-next))
        (should-error (drover-session-at-point) :type 'user-error)))
    ;; Without sessions at all, the filter is not to blame.
    (setq drover-test-sessions nil)
    (let ((drover-filter '(:project "gamma")))
      (with-current-buffer (drover-sidebar-test--render)
        (should (equal (drover-sidebar-test--lines) '("Filter: project gamma" "No sessions.")))))))

(ert-deftest drover-sidebar-test-filter-redraws ()
  (drover-sidebar-test-with (drover-sidebar-test--projects)
    (with-current-buffer (window-buffer (drover-sidebar-select))
      (drover-sidebar-test--goto "a-idle")
      (drover-set-filter :project "alpha")
      (should (equal (car (drover-sidebar-test--lines)) "Filter: project alpha"))
      (should (drover-sidebar-test--on "a-idle"))
      (drover-set-filter :search "a-")
      (should (equal (car (drover-sidebar-test--lines)) "Filter: project alpha, \"a-\""))
      (should (drover-sidebar-test--on "a-idle"))
      (drover-set-filter :statuses '(failed))
      (should (equal (cdr (drover-sidebar-test--lines)) '("No session matches." "F c clears the filter.")))
      (drover-sidebar-test--type "F c C-g")
      (should-not drover-filter)
      (should-not (string-search "Filter" (buffer-string)))
      (should (equal (drover-sidebar-test--order) '("a-ask" "none" "b-work" "a-idle"))))))

;;;; Proportional text

(defun drover-sidebar-test--fake-width (char)
  "Return the width of CHAR, in pixels, in a fake proportional font."
  (cond ((memq char '(?i ?l ?\s ?. ?: ?…)) 2)
        ((memq char '(?m ?w)) 8)
        ((<= ?A char ?Z) 7)
        ((< char 128) 5)
        (t 6)))

(defun drover-sidebar-test--fake-pixels (string)
  "Return the width of STRING, in pixels, in the fake proportional font."
  (apply #'+ (mapcar #'drover-sidebar-test--fake-width string)))

(defvar drover-sidebar-test--measured nil
  "Face remappings and windows text was measured with, newest first.")

(defmacro drover-sidebar-test-proportional (&rest body)
  "Run BODY measuring text in a fake proportional font.
Each measure records its buffer's face remapping and its window in
`drover-sidebar-test--measured'."
  (declare (indent 0) (debug t))
  `(let ((drover-sidebar-test--measured nil))
     (cl-letf (((symbol-function 'buffer-text-pixel-size)
                (lambda (&optional _buffer window &rest _)
                  (push (list face-remapping-alist window) drover-sidebar-test--measured)
                  (cons (drover-sidebar-test--fake-pixels (buffer-string)) 1))))
       ,@body)))

(ert-deftest drover-sidebar-test-variable-pitch ()
  (should (eq (face-attribute 'drover-sidebar :inherit) 'variable-pitch))
  (with-temp-buffer
    (drover-sidebar-mode)
    (should (memq 'drover-sidebar (assq 'default face-remapping-alist)))
    (should (memq #'drover-sidebar--text-scaled text-scale-mode-hook))))

(ert-deftest drover-sidebar-test-measures-with-sidebar-faces ()
  (drover-sidebar-test-with (drover-sidebar-test--sample)
    (drover-sidebar-test-proportional
      (drover-sidebar-test--render 200)
      (should drover-sidebar-test--measured)
      (dolist (measure drover-sidebar-test--measured)
        (should (memq 'drover-sidebar (assq 'default (car measure))))))))

(ert-deftest drover-sidebar-test-measures-in-sidebar-window ()
  (drover-sidebar-test-with (drover-sidebar-test--sample)
    (drover-sidebar-test--main)
    (let ((sidebar (drover-sidebar--show)))
      (drover-sidebar-test-proportional
        (drover-sidebar-refresh)
        (should drover-sidebar-test--measured)
        (dolist (measure drover-sidebar-test--measured)
          (should (eq (cadr measure) sidebar)))))))

(ert-deftest drover-sidebar-test-proportional-truncation ()
  (let ((title "Session with a title much too wide for the window"))
    (drover-sidebar-test-with (list (drover-session-create :backend 'fake :id "long" :title title
                                                           :status 'done :since (- drover-test-now 7200))
                                    (drover-session-create :backend 'fake :id "kid" :parent "long"
                                                           :title title :status 'waiting
                                                           :requests '((:kind question
                                                                        :summary "Which of the many files should I edit before the tests run?"))))
      (drover-sidebar-test-proportional
        (with-current-buffer (drover-sidebar-test--render 150)
          ;; Fold glyph and space are 6 + 2 pixels, glyphs 6, spaces 2.
          (drover-sidebar-test--goto "long")
          (let* ((start (point))
                 (age-blank (text-property-search-forward 'display nil
                                                          (lambda (_ value)
                                                            (eq (car-safe (cdr-safe value)) :align-to))))
                 (shown (buffer-substring-no-properties (+ start 4) (prop-match-beginning age-blank))))
            (goto-char start)
            (should (= (drover-sidebar-test--indent) 8))
            (should (equal (buffer-substring-no-properties (+ start 2) (+ start 4)) "✓ "))
            ;; The age, "2h", is 10 pixels: flush right at 150.
            (should (equal (prop-match-value age-blank) '(space :align-to (140))))
            (should (equal (buffer-substring-no-properties (prop-match-end age-blank) (line-end-position))
                           "2h"))
            ;; The title ends in an ellipsis before the age, with a space
            ;; to spare, and one more character would not fit.
            (should (string-suffix-p "…" shown))
            (let ((room (- 140 (+ 8 6 2) 2))
                  (kept (substring shown 0 -1)))
              (should (<= (drover-sidebar-test--fake-pixels shown) room))
              (should (> (drover-sidebar-test--fake-pixels
                          (concat (substring title 0 (1+ (length kept))) "…"))
                         room))
              (should (string-prefix-p kept title))))
          ;; The subagent starts under the title, and so does its summary
          ;; under its own title.
          (drover-sidebar-test--goto "kid")
          (should (= (drover-sidebar-test--indent) (+ 8 6 2)))
          (forward-line 1)
          (should (= (drover-sidebar-test--indent) (+ 8 6 2 6 2)))
          (let ((summary (buffer-substring-no-properties
                          (next-single-property-change (point) 'display) (line-end-position))))
            (should (string-suffix-p "…" summary))
            (should (<= (drover-sidebar-test--fake-pixels summary) (- 150 24)))))))))

(ert-deftest drover-sidebar-test-truncate ()
  (drover-sidebar-test-with nil
    (drover-sidebar-test-proportional
      (with-temp-buffer
        (let ((layout (drover-sidebar--layout 100 nil))
              (text (concat "Wide" (propertize " dim tail" 'face 'drover-dim))))
          ;; "Wide dim tail" is 7+2+5+5 + 2 + 5+2+8 + 2 + 5+5+2+2 = 52 pixels.
          (should (eq (drover-sidebar--truncate layout text 52) text))
          (let ((cut (drover-sidebar--truncate layout text 30)))
            (should (equal (substring-no-properties cut) "Wide di…"))
            (should (eq (get-text-property (1- (length cut)) 'face cut) 'drover-dim)))
          (should (equal (drover-sidebar--truncate layout text 2) "…"))
          (should (equal (drover-sidebar--truncate layout text 1) "")))))))

(ert-deftest drover-sidebar-test-reuses-widths ()
  (drover-sidebar-test-with (drover-sidebar-test--sample)
    (drover-sidebar-test-proportional
      (with-current-buffer (drover-sidebar-test--render 120)
        (should drover-sidebar-test--measured)
        (let ((lines (buffer-string)))
          ;; Nothing changed: the second drawing measures nothing.
          (setq drover-sidebar-test--measured nil)
          (drover-sidebar-test--render 120)
          (should-not drover-sidebar-test--measured)
          (should (equal-including-properties (buffer-string) lines))
          ;; A new width reuses widths but truncates anew.
          (drover-sidebar-test--render 60)
          (should-not (equal (buffer-string) lines))
          ;; Text of another size, or a refresh, measures again.
          (setq drover-sidebar-test--measured nil)
          (face-remap-add-relative 'default :height 2.0)
          (drover-sidebar-test--render 60)
          (should drover-sidebar-test--measured)
          (setq drover-sidebar-test--measured nil)
          (revert-buffer)
          (should drover-sidebar-test--measured))))))

(ert-deftest drover-sidebar-test-one-space-after-glyph ()
  (drover-sidebar-test-with (append (drover-sidebar-test--sample)
                                    (list (drover-test-session "kid" :parent "work" :status 'retrying)))
    (let ((drover-show-children t))
      (with-current-buffer (drover-sidebar-test--render)
        (dolist (id '("idle" "work" "ask" "done" "fail" "kid"))
          (drover-sidebar-test--goto id)
          (let ((glyph (next-single-property-change (point) 'display)))
            (should (equal (string (char-after glyph))
                           (alist-get (drover-session-status (drover-find 'fake id)) drover-status-glyphs)))
            ;; One plain space, then the title.
            (should (eq (char-after (1+ glyph)) ?\s))
            (should-not (get-text-property (1+ glyph) 'display))
            (should (equal (buffer-substring-no-properties (+ glyph 2) (+ glyph 10 (length id)))
                           (concat "Session " id)))))))))

;;;; Keys

(defun drover-sidebar-test--own-bindings (map)
  "Return the bindings MAP makes itself, not through its parent.
The value is a list of (KEY . COMMAND), KEY a key description."
  (let (bindings)
    (cl-labels ((walk (map prefix)
                  (map-keymap-internal
                   (lambda (event binding)
                     (let ((key (vconcat prefix (vector event))))
                       (if (keymapp binding)
                           (walk binding key)
                         (push (cons (key-description key) binding) bindings))))
                   map)))
      (walk map []))
    (sort bindings (lambda (a b) (string< (car a) (car b))))))

(defun drover-sidebar-test--session-bindings ()
  "Return the bindings of `drover-session-map' as (KEY . COMMAND), sorted by key."
  (let (bindings)
    (map-keymap (lambda (event command) (push (cons (key-description (vector event)) command) bindings))
                drover-session-map)
    (sort bindings (lambda (a b) (string< (car a) (car b))))))

(ert-deftest drover-sidebar-test-view ()
  "The sidebar is one of drover's views, so grouping and filtering apply."
  (with-temp-buffer
    (drover-sidebar-mode)
    (should drover-view-p)
    (should (drover-session-list-p))
    (should (plist-get (drover--context) :view))
    (should (eq drover-session-at-point-function #'drover-sidebar--session-at-point))
    (should (eq drover-main-window-function #'drover-sidebar--main-window))
    (should-not (local-variable-p 'drover-show-children))
    (should truncate-lines)
    (should buffer-read-only)
    (should (memq #'drover-sidebar--size-changed window-size-change-functions))))

(ert-deftest drover-sidebar-test-keymap ()
  "The sidebar binds the session keys, and only standard keys of its own."
  (should (memq drover-session-map (keymap-parent drover-sidebar-mode-map)))
  (should (equal (drover-sidebar-test--own-bindings drover-sidebar-mode-map)
                 '(("<backtab>" . drover-sidebar-toggle-all-groups)
                   ("<mouse-2>" . drover-sidebar-mouse-open)
                   ("C-o" . drover-sidebar-display)
                   ("M-n" . drover-sidebar-next-group)
                   ("M-p" . drover-sidebar-previous-group)
                   ("RET" . drover-open-at-point)
                   ("TAB" . drover-sidebar-toggle-group)
                   ("n" . drover-sidebar-next)
                   ("p" . drover-sidebar-previous)
                   ("q" . drover-sidebar-hide))))
  (should (equal (drover-sidebar-test--own-bindings drover-sidebar-heading-map)
                 '(("RET" . drover-sidebar-toggle-group))))
  (with-temp-buffer
    (drover-sidebar-mode)
    ;; No key of its own shadows a session key.
    (pcase-dolist (`(,key . ,command) (drover-sidebar-test--session-bindings))
      (should (eq (key-binding (kbd key)) command)))
    ;; The keys of `special-mode' stay, but where the sidebar has its own.
    (dolist (binding '(("SPC" . scroll-up-command) ("DEL" . scroll-down-command)
                       ("?" . describe-mode) ("h" . describe-mode)
                       ("<" . beginning-of-buffer) (">" . end-of-buffer)
                       ("q" . drover-sidebar-hide) ("g" . drover-refresh)))
      (should (eq (key-binding (kbd (car binding))) (cdr binding))))))

(ert-deftest drover-sidebar-test-session-keys ()
  "Every key of `drover-session-map' reaches its command from the sidebar.
Keys go through the command loop, with the sidebar selected, as the
user types them; the sidebar is selected again after keys opening a
session, which may select the window showing it."
  (drover-sidebar-test-with (list (drover-test-session "root" :status 'working :directory "/work/root/"
                                                       :updated (- drover-test-now 60))
                                  (drover-test-session "kid" :parent "root" :status 'waiting
                                                       :since (- drover-test-now 30)
                                                       :requests '((:kind question :summary "Go?")))
                                  (drover-test-session "quiet" :parent "root" :status 'idle))
    (let* ((org-stored-links nil)
           (commands (mapcar #'cdr (drover-sidebar-test--session-bindings)))
           (ran nil)
           (record (lambda ()
                     (when (memq this-command commands)
                       (push (cons (key-description (this-command-keys-vector)) this-command) ran)))))
      (add-hook 'drover-store-link-functions
                (lambda (session) (list (concat "drover:" (drover-session-id session)) "Link")))
      (drover-sidebar-test--main)
      (add-hook 'post-command-hook record)
      (unwind-protect
          (let ((sidebar (drover-sidebar-select)))
            (cl-flet ((type (keys)
                        (drover-sidebar-test--type keys)
                        (select-window sidebar)
                        (set-buffer (window-buffer sidebar))))
              (setq drover-test-calls nil)
              (drover-sidebar-test--goto "root")
              (cl-letf (((symbol-function 'y-or-n-p) #'always)
                        ((symbol-function 'yes-or-no-p) #'always)
                        ((symbol-function 'read-string) (lambda (&rest _) "Renamed"))
                        ((symbol-function 'drover-read-session)
                         (lambda (_prompt sessions)
                           (seq-find (lambda (session) (equal (drover-session-id session) "quiet"))
                                     sessions))))
                (dolist (key '("A" "k" "s" "c" "o" "r" "x" "z" "l" "+" "v"))
                  (type key)
                  (should (drover-sidebar-test--on "root"))))
              (drover-sidebar-test--goto "kid")
              (type "u")
              (should (equal (mapcar (lambda (call) (seq-take call 2)) (reverse drover-test-calls))
                             '((:respond "root") (:interrupt "root") (:mark-seen "root")
                               (:open "quiet") (:open "root") (:rename "root") (:delete "root")
                               (:archive "root") (:new "/work/root/") (:preview "root")
                               (:open "root"))))
              (should (member '(:rename "root" "Renamed") drover-test-calls))
              (should (equal org-stored-links '(("drover:root" "Link"))))
              (should (with-current-buffer drover-peek-buffer
                        (string-search "Preview of root" (buffer-string))))
              (setq drover-test-calls nil)
              ;; g redraws from what the backend reports, point staying put.
              (push (drover-test-session "fresh" :status 'working) drover-test-sessions)
              (type "g")
              (should (string-search "Session fresh" (buffer-string)))
              (should (drover-sidebar-test--on "kid"))
              (type "U")
              (should drover-show-children)
              (should (string-search "Session quiet" (buffer-string)))
              (should (drover-sidebar-test--on "kid"))
              (type "V")
              (should drover-follow-mode)
              (should (equal drover-test-calls '((:open "kid" nil))))
              (type "V")
              (should-not drover-follow-mode)
              (type "G p C-g")
              (should (eq drover-group-by 'project))
              (should (drover-sidebar-test--on "kid"))
              (drover-set-filter :search "kid")
              (should (string-prefix-p "Filter: " (car (drover-sidebar-test--lines))))
              (type "F c C-g")
              (should-not drover-filter)
              (should (string-search "Session fresh" (buffer-string)))
              (should (drover-sidebar-test--on "kid"))
              (should-not (bound-and-true-p transient--prefix))))
        (remove-hook 'post-command-hook record))
      (should (equal (sort (delete-dups ran) (lambda (a b) (string< (car a) (car b))))
                     (drover-sidebar-test--session-bindings))))))

(ert-deftest drover-sidebar-test-declined-confirmations ()
  (drover-sidebar-test-with (drover-sidebar-test--sample)
    (with-current-buffer (window-buffer (drover-sidebar-select))
      (setq drover-test-calls nil)
      (drover-sidebar-test--goto "work")
      (cl-letf (((symbol-function 'y-or-n-p) #'ignore)
                ((symbol-function 'yes-or-no-p) #'ignore))
        (dolist (key '("k" "x" "z"))
          (should (equal (should-error (drover-sidebar-test--type key) :type 'user-error)
                         '(user-error "Abort")))))
      (should-not drover-test-calls))))

(ert-deftest drover-sidebar-test-action-off-session ()
  (drover-sidebar-test-with (drover-sidebar-test--sample)
    (with-current-buffer (window-buffer (drover-sidebar-select))
      (setq drover-test-calls nil)
      (goto-char (point-min))
      (should (eq (drover-sidebar--line-type) 'heading))
      (should-error (drover-sidebar-test--type "A") :type 'user-error)
      (should-not drover-test-calls))))

(ert-deftest drover-sidebar-test-peek ()
  (drover-sidebar-test-with (drover-sidebar-test--sample)
    (with-current-buffer (window-buffer (drover-sidebar-select))
      (setq drover-test-calls nil)
      (drover-sidebar-test--goto "done")
      (drover-sidebar-test--type "v")
      (should (equal drover-test-calls '((:preview "done"))))
      (with-current-buffer drover-peek-buffer
        (should (string-search "Preview of done" (buffer-string)))))))

;;;; Showing and hiding

(ert-deftest drover-sidebar-test-toggle ()
  (drover-sidebar-test-with (drover-sidebar-test--sample)
    (let ((origin (selected-window)))
      (drover-sidebar)
      (let ((window (drover-sidebar--window)))
        (should (window-live-p window))
        (should (eq (selected-window) origin))
        (should (equal (buffer-name (window-buffer window)) drover-sidebar-buffer-name))
        (should (eq (window-parameter window 'window-side) 'left))
        (should (eql (window-parameter window 'window-slot) 0))
        (should (window-parameter window 'no-delete-other-windows))
        (should (window-parameter window 'no-other-window))
        (should (eq (window-dedicated-p window) t))
        (should (= (window-total-width window) 36))
        (should (window-preserved-size window t))
        (should (member '(:start nil) drover-test-calls))
        (with-current-buffer (window-buffer window)
          (should (eq major-mode 'drover-sidebar-mode))
          (should (equal (car (drover-sidebar-test--lines)) "▾ Needs you 1"))
          ;; Point starts on the first session.
          (should (equal (drover-sidebar-test--key) "ask"))
          (should-not drover-sidebar--stale)))
      (drover-sidebar)
      (should-not (drover-sidebar--window))
      (should (get-buffer drover-sidebar-buffer-name))
      (should (eq (selected-window) origin)))))

(ert-deftest drover-sidebar-test-right-side ()
  (drover-sidebar-test-with nil
    (let ((drover-sidebar-side 'right)
          (drover-sidebar-width 30))
      (drover-sidebar)
      (should (eq (window-parameter (drover-sidebar--window) 'window-side) 'right))
      (should (= (window-total-width (drover-sidebar--window)) 30)))))

(ert-deftest drover-sidebar-test-survives-delete-other-windows ()
  (drover-sidebar-test-with nil
    (drover-sidebar-test--main)
    (drover-sidebar)
    (delete-other-windows)
    (should (window-live-p (drover-sidebar--window)))
    (should (= (length (window-list nil 'nomini)) 2))))

(ert-deftest drover-sidebar-test-not-replaced-by-switch-to-buffer ()
  (drover-sidebar-test-with nil
    (let ((sidebar (drover-sidebar-select)))
      (switch-to-buffer (get-buffer-create "*other 1*"))
      (should (equal (buffer-name (window-buffer sidebar)) drover-sidebar-buffer-name))
      (should-not (eq (get-buffer-window "*other 1*") sidebar)))))

(ert-deftest drover-sidebar-test-prefix-selects ()
  (drover-sidebar-test-with (drover-sidebar-test--sample)
    (drover-sidebar '(4))
    (should (eq (selected-window) (drover-sidebar--window)))
    (should (eq (current-buffer) (get-buffer drover-sidebar-buffer-name)))))

(ert-deftest drover-sidebar-test-select-back-and-forth ()
  (drover-sidebar-test-with (drover-sidebar-test--sample)
    (pcase-let ((`(,first ,_second) (drover-sidebar-test--main)))
      (drover-sidebar-select)
      (should (eq (selected-window) (drover-sidebar--window)))
      (drover-sidebar-select)
      (should (eq (selected-window) first))
      (drover-sidebar-select)
      (should (eq (selected-window) (drover-sidebar--window)))
      (call-interactively (key-binding (kbd "q")))
      (should-not (drover-sidebar--window))
      (should (eq (selected-window) first)))))

(ert-deftest drover-sidebar-test-watch-and-unwatch ()
  (drover-sidebar-test-with nil
    (let ((drover-sidebar-age-interval 1000))
      (drover-sidebar)
      (should (memq #'drover-sidebar--changed drover-changed-hook))
      (should (memq #'drover-sidebar--windows-changed (default-value 'window-buffer-change-functions)))
      (should (timerp drover-sidebar--timer))
      ;; Only the sidebar buffer itself stops the watch.
      (with-temp-buffer (drover-sidebar-mode))
      (should (memq #'drover-sidebar--changed drover-changed-hook))
      (kill-buffer drover-sidebar-buffer-name)
      (should-not (memq #'drover-sidebar--changed drover-changed-hook))
      (should-not (memq #'drover-sidebar--windows-changed
                        (default-value 'window-buffer-change-functions)))
      (should-not (memq #'drover-sidebar--windows-changed
                        (default-value 'window-selection-change-functions)))
      (should-not drover-sidebar--timer))))

;;;; The main window

(ert-deftest drover-sidebar-test-main-window-most-recent ()
  (drover-sidebar-test-with nil
    (pcase-let ((`(,first ,second) (drover-sidebar-test--main)))
      (drover-sidebar-select)
      (should (eq (drover-sidebar--main-window) first))
      (select-window second)
      (drover-sidebar-select)
      (should (eq (drover-sidebar--main-window) second)))))

(ert-deftest drover-sidebar-test-main-window-avoids-dedicated ()
  (drover-sidebar-test-with nil
    (pcase-let ((`(,first ,second) (drover-sidebar-test--main)))
      (set-window-dedicated-p first t)
      (drover-sidebar-select)
      (should (eq (drover-sidebar--main-window) second))
      (set-window-dedicated-p second t)
      (should (eq (drover-sidebar--main-window) first))
      (set-window-dedicated-p second nil))))

;; Opening a session from the sidebar, with RET, o or a click, must not
;; replace the sidebar in its dedicated side window: `drover-open' opens
;; sessions in the window `drover-main-window-function' returns, the
;; main window.

(ert-deftest drover-sidebar-test-ret-opens-in-main-window ()
  "RET opens the session at point in the main window, and selects it."
  (drover-sidebar-test-with (drover-sidebar-test--sample)
    (drover-sidebar-test--register-buffers)
    (pcase-let ((`(,first ,second) (drover-sidebar-test--main)))
      (let ((sidebar (drover-sidebar-select)))
        (drover-sidebar-test--goto "done")
        (drover-sidebar-test--type "RET")
        (should (equal (car drover-test-calls) (list :open "done" nil first)))
        (should (eq (selected-window) first))
        (should (equal (buffer-name (window-buffer first)) "*session done*"))
        (should (equal (buffer-name (window-buffer second)) "*other 2*"))
        (should (equal (buffer-name (window-buffer sidebar)) drover-sidebar-buffer-name))))))

(ert-deftest drover-sidebar-test-o-opens-in-main-window ()
  "In the sidebar, the other window o opens a session in is the main window."
  (drover-sidebar-test-with (drover-sidebar-test--sample)
    (drover-sidebar-test--register-buffers)
    (pcase-let ((`(,first ,second) (drover-sidebar-test--main)))
      (let ((sidebar (drover-sidebar-select)))
        (drover-sidebar-test--goto "done")
        (drover-sidebar-test--type "o")
        (should (equal (car drover-test-calls) (list :open "done" nil first)))
        (should (eq (selected-window) first))
        (should (equal (buffer-name (window-buffer first)) "*session done*"))
        (should (equal (buffer-name (window-buffer second)) "*other 2*"))
        (should (equal (buffer-name (window-buffer sidebar)) drover-sidebar-buffer-name))
        (should (= (length (window-list nil 'nomini)) 3))))))

(ert-deftest drover-sidebar-test-mouse-opens-in-main-window ()
  "Clicking a session opens it as RET does."
  (drover-sidebar-test-with (drover-sidebar-test--sample)
    (drover-sidebar-test--register-buffers)
    (pcase-let ((`(,first ,_second) (drover-sidebar-test--main)))
      (let* ((sidebar (drover-sidebar--show))
             (position (with-current-buffer (window-buffer sidebar)
                         (drover-sidebar-test--goto "ask")
                         (point))))
        (drover-sidebar-mouse-open
         `(mouse-2 (,sidebar ,position (0 . 0) 0 nil ,position (0 . 0) nil (0 . 0) (1 . 1))))
        (should (eq (selected-window) first))
        (should (equal (buffer-name (window-buffer first)) "*session ask*"))))))

(ert-deftest drover-sidebar-test-mouse-folds-group ()
  (drover-sidebar-test-with (drover-sidebar-test--sample)
    (drover-sidebar-test--main)
    (let* ((sidebar (drover-sidebar--show))
           (position (with-current-buffer (window-buffer sidebar)
                       (drover-sidebar--find-line 'heading #'always))))
      (drover-sidebar-mouse-open
       `(mouse-2 (,sidebar ,position (0 . 0) 0 nil ,position (0 . 0) nil (0 . 0) (1 . 1))))
      (should (eq (selected-window) sidebar))
      (should (equal (car (drover-sidebar-test--lines)) "▸ Needs you 1"))
      (should-not (assq :open drover-test-calls)))))

(ert-deftest drover-sidebar-test-display-keeps-sidebar-selected ()
  (drover-sidebar-test-with (drover-sidebar-test--sample)
    (drover-sidebar-test--register-buffers)
    (pcase-let ((`(,first ,second) (drover-sidebar-test--main)))
      (let ((sidebar (drover-sidebar-select)))
        (drover-sidebar-test--goto "work")
        (drover-sidebar-test--type "C-o")
        (should (equal (car drover-test-calls) (list :open "work" nil first)))
        (should (eq (selected-window) sidebar))
        (should (eq (current-buffer) (window-buffer sidebar)))
        (should (equal (buffer-name (window-buffer first)) "*session work*"))
        (should (equal (buffer-name (window-buffer second)) "*other 2*"))))))

(ert-deftest drover-sidebar-test-open-unsupported ()
  (drover-sidebar-test-with nil
    (drover-register-backend 'bare :label "Bare"
                             :sessions (lambda () (list (drover-session-create :backend 'bare :id "x"))))
    (drover-sidebar-test--main)
    (let ((sidebar (drover-sidebar-select)))
      (drover-sidebar-next)
      (should (eq (drover-sidebar--line-type) 'session))
      (should-error (drover-sidebar-test--type "RET") :type 'user-error)
      (should-error (drover-sidebar-test--type "C-o") :type 'user-error)
      (should (eq (selected-window) sidebar)))))

(ert-deftest drover-sidebar-test-follow-mode ()
  "Follow mode shows the session at point in the main window as point moves."
  (drover-sidebar-test-with (drover-sidebar-test--sample)
    (drover-sidebar-test--register-buffers)
    (pcase-let ((`(,first ,second) (drover-sidebar-test--main)))
      (let ((sidebar (drover-sidebar-select))
            (shown (lambda () (buffer-name (window-buffer first))))
            (opened (lambda () (length (seq-filter (lambda (call) (eq (car call) :open))
                                                   drover-test-calls)))))
        (should (equal (drover-sidebar-test--key) "ask"))
        (drover-sidebar-test--type "n")
        (should (equal (funcall shown) "*other 1*"))
        (drover-sidebar-test--type "p")
        ;; Turning it on shows the session at point at once.
        (drover-sidebar-test--type "V")
        (should drover-follow-mode)
        (should (equal (drover-sidebar--mode-line-flags) " Follow"))
        (should (equal (car drover-test-calls) (list :open "ask" nil first)))
        (should (equal (funcall shown) "*session ask*"))
        (drover-sidebar-test--type "n")
        (should (equal (funcall shown) "*session fail*"))
        ;; Any motion counts; within a session, nothing is shown again.
        (drover-sidebar-test--type "C-n")
        (should (eq (drover-sidebar--line-type) 'summary))
        (should (= (funcall opened) 2))
        ;; A heading shows nothing.
        (drover-sidebar-test--type "C-n")
        (should (eq (drover-sidebar--line-type) 'heading))
        (should (= (funcall opened) 2))
        (drover-sidebar-test--type "C-n")
        (should (equal (funcall shown) "*session done*"))
        (drover-sidebar-test--type "p")
        (should (equal (funcall shown) "*session fail*"))
        (should (eq (selected-window) sidebar))
        (should (equal (buffer-name (window-buffer second)) "*other 2*"))
        ;; The main window stays the same, and its session is highlighted.
        (should (eq (drover-sidebar--main-window) first))
        (drover-sidebar--windows-changed)
        (should (equal (drover-sidebar-test--key-at (overlay-start (car drover-sidebar--current-overlays)))
                       "fail"))
        (drover-sidebar-test--type "V")
        (should-not drover-follow-mode)
        (should (equal (drover-sidebar--mode-line-flags) ""))
        (drover-sidebar-test--type "n")
        (should (equal (funcall shown) "*session fail*"))))))

;;;; The current session

(ert-deftest drover-sidebar-test-highlights-current-session ()
  (drover-sidebar-test-with (drover-sidebar-test--sample)
    (drover-sidebar-test--register-buffers)
    (pcase-let ((`(,first ,_second) (drover-sidebar-test--main)))
      (let ((sidebar (drover-sidebar--show)))
        (with-current-buffer (window-buffer sidebar)
          (should-not drover-sidebar--current-overlays)
          (set-window-buffer first (get-buffer-create "*session work*"))
          (drover-sidebar--windows-changed)
          (let ((overlay (car drover-sidebar--current-overlays)))
            (should (= (length drover-sidebar--current-overlays) 1))
            (should (eq (overlay-get overlay 'face) 'drover-sidebar-current))
            (should (eq (overlay-get overlay 'window) sidebar))
            (should (equal (cdr (get-text-property (overlay-start overlay) 'drover-sidebar-key))
                           "work"))
            (should (= (overlay-end overlay)
                       (save-excursion (goto-char (overlay-start overlay)) (forward-line 1) (point)))))
          ;; A session with a second line is highlighted whole.
          (set-window-buffer first (get-buffer-create "*session ask*"))
          (drover-sidebar--windows-changed)
          (let ((overlay (car drover-sidebar--current-overlays)))
            (should (equal (cdr (get-text-property (overlay-start overlay) 'drover-sidebar-key))
                           "ask"))
            (should (= (overlay-end overlay)
                       (save-excursion (goto-char (overlay-start overlay)) (forward-line 2) (point)))))
          ;; The highlight survives redraws.
          (drover-sidebar--redraw)
          (should (= (length drover-sidebar--current-overlays) 1))
          (set-window-buffer first (get-buffer "*other 1*"))
          (drover-sidebar--windows-changed)
          (should-not drover-sidebar--current-overlays))))))

(ert-deftest drover-sidebar-test-highlights-nested-session ()
  (drover-sidebar-test-with (drover-sidebar-test--asking-child)
    (drover-sidebar-test--register-buffers)
    (pcase-let ((`(,first ,_second) (drover-sidebar-test--main)))
      (with-current-buffer (window-buffer (drover-sidebar--show))
        (let ((line-start (lambda (id) (save-excursion (drover-sidebar-test--goto id) (point)))))
          ;; The parent alone, not the subagent below it.
          (set-window-buffer first (get-buffer-create "*session parent*"))
          (drover-sidebar--windows-changed)
          (let ((overlay (car drover-sidebar--current-overlays)))
            (should (= (overlay-start overlay) (funcall line-start "parent")))
            (should (= (overlay-end overlay) (funcall line-start "asking"))))
          ;; The subagent with its summary.
          (set-window-buffer first (get-buffer-create "*session asking*"))
          (drover-sidebar--windows-changed)
          (let ((overlay (car drover-sidebar--current-overlays)))
            (should (= (overlay-start overlay) (funcall line-start "asking")))
            (should (= (overlay-end overlay)
                       (save-excursion (goto-char (overlay-start overlay)) (forward-line 2) (point))))))))))

(ert-deftest drover-sidebar-test-no-highlight-without-buffers ()
  (drover-sidebar-test-with (drover-sidebar-test--sample)
    (drover-sidebar-test--main)
    (with-current-buffer (window-buffer (drover-sidebar--show))
      (drover-sidebar--windows-changed)
      (should-not drover-sidebar--current-overlays))))

;;;; Refreshing

(ert-deftest drover-sidebar-test-refresh-keeps-point-on-session ()
  (drover-sidebar-test-with (drover-sidebar-test--sample)
    (pcase-let ((`(,first ,_second) (drover-sidebar-test--main)))
      (with-current-buffer (window-buffer first)
        (insert "some text\nmore text\n")
        (set-window-point first 5))
      (let ((sidebar (drover-sidebar--show)))
        (with-selected-window sidebar
          (drover-sidebar-test--goto "work"))
        ;; "work" changes group and a session joins above it.
        (setf (drover-session-status (drover-find 'fake "work")) 'done
              (drover-session-since (drover-find 'fake "work")) (- drover-test-now 10000))
        (push (drover-test-session "new" :status 'waiting :since (- drover-test-now 5)) drover-test-sessions)
        (drover-backend-changed 'fake)
        (drover-changed-now)
        (with-current-buffer (window-buffer sidebar)
          (should (string-search "Session new" (buffer-string)))
          (goto-char (window-point sidebar))
          (should (equal (drover-sidebar-test--key) "work"))
          (should (equal (get-text-property (point) 'drover-sidebar-group) '(status . done))))
        (should (= (window-point first) 5))
        (should (eq (selected-window) first))))))

(ert-deftest drover-sidebar-test-refresh-when-session-gone ()
  (drover-sidebar-test-with (drover-sidebar-test--sample)
    (let ((sidebar (drover-sidebar-select)))
      (drover-sidebar-test--goto "done")
      (setq drover-test-sessions (seq-remove (lambda (session) (equal (drover-session-id session) "done"))
                                             drover-test-sessions))
      (drover-sidebar-refresh)
      (should (eq (window-buffer sidebar) (current-buffer)))
      ;; Point stays on the same line, now the next session's.
      (should (= (line-number-at-pos (window-point sidebar)) 8))
      (should (equal (drover-sidebar-test--key) "work")))))

(ert-deftest drover-sidebar-test-refresh-keeps-window-start ()
  (drover-sidebar-test-with (mapcar (lambda (n)
                                      (drover-test-session (format "s%02d" n) :status 'idle
                                                           :updated (- drover-test-now n)))
                                    (number-sequence 1 60))
    (let ((sidebar (drover-sidebar--show)))
      (with-current-buffer (window-buffer sidebar)
        (let ((start (save-excursion (goto-char (point-min)) (forward-line 20) (point))))
          (set-window-start sidebar start)
          (drover-sidebar-test--goto "s25")
          (set-window-point sidebar (point))))
      (setq drover-test-sessions
            (append drover-test-sessions
                    (list (drover-test-session "s99" :status 'idle :updated 1.0))))
      (drover-backend-changed 'fake)
      (drover-changed-now)
      (with-current-buffer (window-buffer sidebar)
        (should (string-search "Session s99" (buffer-string)))
        (should (= (line-number-at-pos (window-start sidebar)) 21))
        (goto-char (window-point sidebar))
        (should (equal (drover-sidebar-test--key) "s25"))))))

(ert-deftest drover-sidebar-test-stale-while-hidden ()
  (drover-sidebar-test-with (drover-sidebar-test--sample)
    (drover-sidebar)
    (drover-sidebar)
    (should-not (drover-sidebar--window))
    (push (drover-test-session "new" :status 'working) drover-test-sessions)
    (drover-backend-changed 'fake)
    (drover-changed-now)
    (with-current-buffer drover-sidebar-buffer-name
      (should drover-sidebar--stale)
      (should-not (string-search "Session new" (buffer-string))))
    (drover-sidebar)
    (with-current-buffer drover-sidebar-buffer-name
      (should-not drover-sidebar--stale)
      (should (string-search "Session new" (buffer-string))))))

(ert-deftest drover-sidebar-test-shown-again-without-changes ()
  (drover-sidebar-test-with (drover-sidebar-test--sample)
    (drover-sidebar)
    (with-current-buffer drover-sidebar-buffer-name
      (set-window-point (drover-sidebar--window)
                        (save-excursion (drover-sidebar-test--goto "work") (point))))
    (drover-sidebar)
    (drover-sidebar)
    (with-current-buffer drover-sidebar-buffer-name
      (should (equal (drover-sidebar-test--key) "work")))))

(ert-deftest drover-sidebar-test-refresh-on-width-change ()
  (drover-sidebar-test-with (list (drover-test-session "a" :status 'idle :updated (- drover-test-now 60)))
    (let* ((sidebar (drover-sidebar--show))
           (before (window-max-chars-per-line sidebar)))
      (with-current-buffer (window-buffer sidebar)
        (should (= drover-sidebar--width before))
        (window-resize sidebar 6 t nil t)
        (should (= (window-max-chars-per-line sidebar) (+ before 6)))
        (drover-sidebar--size-changed sidebar)
        (should (= drover-sidebar--width (+ before 6)))
        (drover-sidebar-test--goto "a")
        (should (equal (get-text-property (- (line-end-position) 3) 'display)
                       `(space :align-to (,(+ before 4)))))))))

(ert-deftest drover-sidebar-test-redraw-on-text-scale ()
  (drover-sidebar-test-with (drover-sidebar-test--sample)
    (drover-sidebar)
    (with-current-buffer drover-sidebar-buffer-name
      (setq drover-sidebar--stale t)
      (text-scale-increase 1)
      (should-not drover-sidebar--stale)
      (should (string-search "Session ask" (buffer-string))))
    ;; A hidden sidebar redraws once shown.
    (drover-sidebar)
    (with-current-buffer drover-sidebar-buffer-name
      (text-scale-increase 1)
      (should drover-sidebar--stale))))

(ert-deftest drover-sidebar-test-revert-buffer ()
  (drover-sidebar-test-with (drover-sidebar-test--sample)
    (with-current-buffer (drover-sidebar-test--render)
      (push (drover-test-session "new" :status 'working) drover-test-sessions)
      (revert-buffer)
      (should (string-search "Session new" (buffer-string))))))

(provide 'drover-sidebar-test)
;;; drover-sidebar-test.el ends here
