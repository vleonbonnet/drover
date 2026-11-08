;;; drover-attention-test.el --- Tests of the attention commands and mode  -*- lexical-binding: t; -*-

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

;;; Code:

(require 'drover-test-helpers)
(require 'drover-attention)
(require 'drover-keys)
(require 'drover-prompt)

(defmacro drover-attention-test-with (sessions &rest body)
  "Run BODY with the fake backend serving SESSIONS and the mode's state isolated."
  (declare (indent 1) (debug t))
  `(drover-test-with-backend ,sessions
                             (let ((global-mode-string nil)
                                   ;; Readable glyphs, whatever the defaults.
                                   (opencode-session-status-icons '((waiting . "⚑") (failed . "✗") (done . "✓")
                                                           (retrying . "↻") (working . "◐") (idle . "·")))
                                   (drover-attention-glyph "☺")
                                   (drover-attention-mode nil)
                                   (drover-attention-mode-line nil)
                                   (drover-attention--mode-string-origin nil)
                                   (drover-attention--queue nil)
                                   (drover-attention--position 0)
                                   (drover-attention--depth 0)
                                   (post-command-hook nil)
                                   (drover-attention-statuses '(waiting failed done))
                                   (drover-attention-echo t)
                                   (drover-attention--pads (make-hash-table :test #'equal))
                                   (after-setting-font-hook nil)
                                   (enable-theme-functions nil)
                                   (last-command nil))
                               (unwind-protect
                                   (progn ,@body)
                                 (when drover-attention-mode
                                   (drover-attention-mode -1))))))

(defmacro drover-attention-test-messages (&rest body)
  "Run BODY and return the messages it showed, oldest first."
  (declare (indent 0) (debug t))
  `(let ((messages nil))
     (cl-letf (((symbol-function 'message)
                (lambda (format &rest arguments)
                  (when format
                    (push (apply #'format-message format arguments) messages)))))
       ,@body)
     (nreverse messages)))

(defun drover-attention-test-opened ()
  "Return the identifiers of the sessions opened, in order."
  (mapcar #'cadr (seq-filter (lambda (call) (eq (car call) :open))
                             (reverse drover-test-calls))))

(defun drover-attention-test-titled (id title &rest properties)
  "Return a fake session ID titled TITLE, with PROPERTIES."
  (apply #'drover-session-create :backend 'fake :id id :title title properties))

(defun drover-attention-test-press ()
  "Press `drover-next' again, where the previous press left the user."
  (drover-next))

(defun drover-attention-test-other-command ()
  "Run what the command loop runs after a command other than `drover-next'."
  (let ((this-command 'forward-char)
        (last-command-event ?f))
    (run-hooks 'post-command-hook)))

(defun drover-attention-test-help (string)
  "Return the tooltip of STRING, the mode line count, as hovering it shows it."
  (let ((help (get-text-property 1 'help-echo string)))
    (if (functionp help) (funcall help (selected-window) string 1) help)))

(defun drover-attention-test-face-at (string text)
  "Return the face in STRING at the first occurrence of TEXT."
  (get-text-property (string-search text string) 'face string))

;;;; Mode line

(ert-deftest drover-attention-test-format-counts ()
  "One count of every session needing the user, in the face of the most urgent."
  (drover-attention-test-with (list (drover-test-session "w1" :status 'waiting :since 1.0)
                                    (drover-test-session "d1" :status 'done :since 2.0)
                                    (drover-test-session "w2" :status 'waiting :since 3.0)
                                    (drover-test-session "r" :status 'working)
                                    (drover-test-session "i" :status 'idle))
    (drover-attention-mode 1)
    (let ((string drover-attention-mode-line))
      ;; As other indicators: the symbol, one plain space, the count,
      ;; nothing before.
      (should (equal (substring-no-properties string) "☺ 3"))
      (should-not (next-single-property-change 0 'display string))
      ;; One face over the whole string, the space included, and the
      ;; whole of it clickable.
      (dotimes (index (length string))
        (should (eq (get-text-property index 'face string) 'opencode-session-status-waiting))
        (should (eq (get-text-property index 'local-map string) drover-attention-mode-line-map))
        (should (eq (get-text-property index 'mouse-face string) 'mode-line-highlight))
        (should (get-text-property index 'help-echo string))))))

(ert-deftest drover-attention-test-format-most-urgent-face ()
  "The count takes the face of the most urgent status among those it counts."
  (drover-attention-test-with (list (drover-test-session "d" :status 'done)
                                    (drover-test-session "f" :status 'failed))
    (drover-attention-mode 1)
    (should (equal (substring-no-properties drover-attention-mode-line) "☺ 2"))
    (should (eq (drover-attention-test-face-at drover-attention-mode-line "☺") 'opencode-session-status-failed))
    (should (eq (drover-attention-test-face-at drover-attention-mode-line "2") 'opencode-session-status-failed))
    (setq drover-test-sessions (list (drover-test-session "d" :status 'done)))
    (drover-backend-changed 'fake)
    (drover-changed-now)
    (should (eq (drover-attention-test-face-at drover-attention-mode-line "1") 'opencode-session-status-done))))

(ert-deftest drover-attention-test-format-follows-status-order ()
  (drover-attention-test-with (list (drover-test-session "d" :status 'done)
                                    (drover-test-session "w" :status 'waiting))
    (let ((drover-attention-statuses '(done waiting))
          (drover-status-order '(done waiting failed retrying working idle)))
      (drover-attention-mode 1)
      (should (equal (substring-no-properties drover-attention-mode-line) "☺ 2"))
      (should (eq (drover-attention-test-face-at drover-attention-mode-line "2") 'opencode-session-status-done)))))

(ert-deftest drover-attention-test-format-honours-statuses ()
  (drover-attention-test-with (list (drover-test-session "d" :status 'done)
                                    (drover-test-session "w" :status 'waiting))
    (let ((drover-attention-statuses '(waiting)))
      (drover-attention-mode 1)
      (should (equal (substring-no-properties drover-attention-mode-line) "☺ 1")))))

(ert-deftest drover-attention-test-format-empty ()
  (drover-attention-test-with (list (drover-test-session "r" :status 'working)
                                    (drover-test-session "i" :status 'idle))
    (drover-attention-mode 1)
    (should-not drover-attention-mode-line)
    (should (memq 'drover-attention-mode-line global-mode-string))))

(ert-deftest drover-attention-test-redraws-mode-lines-on-change-only ()
  "Mode lines are drawn again when the count reads otherwise, and only then.
Sessions change many times a second while they work, mostly leaving the
count as it was; forcing every mode line costs tens of milliseconds."
  (drover-attention-test-with (list (drover-test-session "w" :status 'waiting :since 1.0)
                                    (drover-test-session "r" :status 'working :updated 1.0))
    (drover-attention-mode 1)
    (let ((forced nil))
      (cl-letf (((symbol-function 'force-mode-line-update)
                 (lambda (&optional all) (push all forced))))
        (cl-flet ((change (id function)
                    (setq forced nil)
                    (funcall function (seq-find (lambda (session) (equal (drover-session-id session) id))
                                                drover-test-sessions))
                    (drover-backend-changed 'fake id)
                    (drover-changed-now)))
          ;; A working session's step: the count stays, nothing is forced.
          (change "r" (lambda (session) (setf (drover-session-updated session) 2.0)))
          (should-not forced)
          ;; The session waiting longer: the same count, in the same face.
          (change "w" (lambda (session) (setf (drover-session-since session) 0.5)))
          (should-not forced)
          ;; Its turn ending unseen: one more, every mode line drawn again.
          (change "r" (lambda (session) (setf (drover-session-status session) 'done)))
          (should (equal forced '(t)))
          (should (equal (substring-no-properties drover-attention-mode-line) "☺ 2"))
          ;; Another status, the count the same, but in another face.
          (change "w" (lambda (session) (setf (drover-session-status session) 'failed)))
          (should (equal forced '(t)))
          (should (eq (drover-attention-test-face-at drover-attention-mode-line "2") 'opencode-session-status-failed))
          ;; Nothing needing the user any more: the count goes.
          (setq drover-test-sessions nil)
          (setq forced nil)
          (drover-backend-changed 'fake)
          (drover-changed-now)
          (should (equal forced '(t)))
          (should-not drover-attention-mode-line)
          ;; And nothing again: nothing to draw.
          (setq forced nil)
          (drover-backend-changed 'fake)
          (drover-changed-now)
          (should-not forced))))))

(ert-deftest drover-attention-test-format-follows-subagent-mode ()
  "The count is that of what lists show, and of every subagent waiting for the user."
  (drover-attention-test-with (list (drover-test-session "root" :status 'working)
                                    (drover-test-session "ended" :parent "root" :status 'done)
                                    (drover-test-session "broke" :parent "root" :status 'failed)
                                    (drover-test-session "asking" :parent "root" :status 'waiting)
                                    (drover-test-session "alone" :status 'done))
    (drover-attention-mode 1)
    ;; Active subagents: those whose turn ended hide, so do not count.
    (should (equal (substring-no-properties drover-attention-mode-line) "☺ 2"))
    (drover-set-show-subagents 'all)
    (should (equal (substring-no-properties drover-attention-mode-line) "☺ 4"))
    ;; None: the subagent waiting for the user still counts.
    (drover-set-show-subagents 'none)
    (should (equal (substring-no-properties drover-attention-mode-line) "☺ 2"))
    (should (eq (drover-attention-test-face-at drover-attention-mode-line "2") 'opencode-session-status-waiting))
    (let ((help (drover-attention-test-help drover-attention-mode-line)))
      (should (string-match-p "Session asking" help))
      (should-not (string-match-p "Session ended\\|Session broke" help)))
    ;; A filter narrows lists, not the count.
    (drover-set-filter :project "elsewhere")
    (should (equal (substring-no-properties drover-attention-mode-line) "☺ 2"))))

(ert-deftest drover-attention-test-orphans-follow-subagent-mode ()
  "A subagent whose parent is not listed counts as any subagent does.
So it does as the count is made anew, and as it follows a change."
  (drover-attention-test-with (list (drover-test-session "alone" :status 'done)
                                    (drover-test-session "ended" :parent "archived" :status 'done))
    (drover-attention-mode 1)
    (should (equal (substring-no-properties drover-attention-mode-line) "☺ 1"))
    (setq drover-test-sessions (list (drover-test-session "alone" :status 'done)
                                     (drover-test-session "ended" :parent "archived" :status 'failed)))
    (drover-backend-changed 'fake "ended")
    (drover-changed-now)
    (should (equal (substring-no-properties drover-attention-mode-line) "☺ 1"))
    (drover-set-show-subagents 'all)
    (should (equal (substring-no-properties drover-attention-mode-line) "☺ 2"))))

(ert-deftest drover-attention-test-help-echo-lists-sessions ()
  "The tooltip lists the sessions in the order `drover-next' visits them."
  (let ((now (float-time)))
    (drover-attention-test-with (list (drover-test-session "w" :status 'waiting :since 1.0
                                                           :project "drover" :branch "main"
                                                           :requests '((:kind permission :summary "bash: make")))
                                      (drover-test-session "d" :status 'done :since 2.0)
                                      (drover-test-session "e" :status 'done :since 3.0
                                                           :cache-expires (+ now 250) :cache-ttl 300)
                                      (drover-test-session "i" :status 'idle))
      (drover-attention-mode 1)
      (let ((help (drover-attention-test-help drover-attention-mode-line)))
        (should (equal (substring-no-properties help)
                       (concat "Sessions that need you, in the order of `drover-next':\n"
                               "  ✓ Session e  cache 4m\n"
                               "  ⚑ Session w  drover:main  permit: bash: make\n"
                               "  ✓ Session d\n"
                               "\n"
                               "mouse-1: visit live caches first, then these, then idle sessions\n"
                               "mouse-3: choose a session")))
        (should (eq (drover-attention-test-face-at help "cache 4m") 'drover-dim))))))

(ert-deftest drover-attention-test-help-echo-bounded ()
  "The tooltip names `drover-attention--help-sessions' sessions, and how many more."
  (drover-attention-test-with (mapcar (lambda (index)
                                        (drover-test-session (format "d%02d" index) :status 'done
                                                             :since (float index)))
                                      (number-sequence 1 25))
    (drover-attention-mode 1)
    (let ((help (drover-attention-test-help drover-attention-mode-line)))
      (should (string-match-p "Session d20\n  … and 5 more\n" help))
      (should-not (string-match-p "Session d21" help)))))

(ert-deftest drover-attention-test-mouse-bindings ()
  (should (eq (keymap-lookup drover-attention-mode-line-map "<mode-line> <mouse-1>") #'drover-next))
  (should (eq (keymap-lookup drover-attention-mode-line-map "<mode-line> <mouse-3>") #'drover-select)))

(ert-deftest drover-attention-test-default-glyph ()
  "The default glyph is the robot emoji, a plain Unicode symbol as other indicators use."
  (let ((glyph (eval (car (get 'drover-attention-glyph 'standard-value)) t)))
    (should (equal glyph "\U0001F916"))
    (should (eq (get-char-code-property (aref glyph 0) 'general-category) 'So))
    (should-not (rassoc glyph (eval (car (get 'opencode-subagent-icons 'standard-value)) t)))))

(ert-deftest drover-attention-test-mode-line-keeps-properties ()
  ;; The mode line drops the text properties of a variable not risky.
  ;; `format-mode-line' renders nothing in batch, hence no rendering here.
  (should (get 'drover-attention-mode-line 'risky-local-variable)))

(ert-deftest drover-attention-test-updates-on-change ()
  (drover-attention-test-with (list (drover-test-session "w" :status 'waiting))
    (drover-attention-mode 1)
    (should (equal (substring-no-properties drover-attention-mode-line) "☺ 1"))
    (setq drover-test-sessions (list (drover-test-session "w" :status 'waiting)
                                     (drover-test-session "f" :status 'failed)))
    ;; Nothing changes until backends report it.
    (should (equal (substring-no-properties drover-attention-mode-line) "☺ 1"))
    (drover-backend-changed 'fake "f")
    (drover-changed-now)
    (should (equal (substring-no-properties drover-attention-mode-line) "☺ 2"))
    (setq drover-test-sessions (list (drover-test-session "w" :status 'idle)))
    (drover-backend-changed 'fake "w")
    (drover-backend-changed 'fake "f")
    (drover-changed-now)
    (should-not drover-attention-mode-line)))

(ert-deftest drover-attention-test-mode-line-not-computed-when-drawn ()
  (drover-attention-test-with (list (drover-test-session "w" :status 'waiting))
    (drover-attention-mode 1)
    ;; The construct is a variable holding a string, nothing to evaluate.
    (should (equal global-mode-string '("" drover-attention-mode-line)))
    (should (stringp drover-attention-mode-line))))

;;;; Glyphs' ink

(defvar drover-attention-test-measured nil
  "Strings the fake font measured the ink of, newest first.")

(defmacro drover-attention-test-with-font (ink &rest body)
  "Run BODY with text measured in a fake font, every character 10 pixels wide.
INK is an alist (GLYPH LEFT . RIGHT): the ink of GLYPH reaches LEFT
pixels left of its start and RIGHT pixels right of it."
  (declare (indent 1) (debug t))
  `(let ((drover-attention-test-measured nil)
         (ink ,ink))
     (cl-letf (((symbol-function 'drover-text-pixel-width)
                (lambda (string &rest _) (* 10 (length string))))
               ((symbol-function 'drover-text-ink-extent)
                (lambda (string &rest _)
                  (push string drover-attention-test-measured)
                  (or (cdr (assoc (substring-no-properties string) ink))
                      (cons 0 (* 10 (length string)))))))
       ,@body)))

(defun drover-attention-test-spans (string ink)
  "Return what the characters of mode line STRING draw, as (START END TEXT).
Each character not displayed as a space draws its ink, in the fake font
of `drover-attention-test-with-font' with INK, from START to END in
pixels; spaces displayed as stretches advance by their width."
  (let ((x 0) (spans nil))
    (dotimes (index (length string))
      (let ((char (substring-no-properties string index (1+ index))))
        (pcase (get-text-property index 'display string)
          (`(space :width (,width)) (setq x (+ x width)))
          (_ (let ((reach (cdr (assoc char ink))))
               (unless (string-blank-p char)
                 (push (list (- x (or (car reach) 0)) (+ x (max 10 (or (cdr reach) 0))) char) spans))
               (setq x (+ x 10)))))))
    (nreverse spans)))

(ert-deftest drover-attention-test-glyphs-leave-room-for-ink ()
  "The count never goes under the ink of the glyph, a plain space covering what it can."
  (drover-attention-test-with (list (drover-test-session "w" :status 'waiting :since 1.0)
                                    (drover-test-session "d" :status 'done :since 2.0))
    ;; The robot draws 16 pixels right of its start, past its advance of
    ;; 10 by less than a space, as FiraCode Nerd Font draws its robots:
    ;; 1790 units of ink for an advance and a space of 1200.
    (drover-attention-test-with-font '(("☺" 0 . 16))
      (drover-attention-mode 1)
      (let ((string drover-attention-mode-line))
        (should (equal (substring-no-properties string) "☺ 2"))
        (should-not (next-single-property-change 0 'display string))
        (let ((spans (drover-attention-test-spans string ink)))
          (should (equal spans '((0 16 "☺") (20 30 "2"))))))
      ;; Measured as the mode line draws: over its face.
      (should (equal (get-text-property 0 'face (car drover-attention-test-measured))
                     '(opencode-session-status-waiting mode-line)))
      (should (= (length drover-attention-test-measured) 1))
      ;; Ink reaching 2 pixels left of its start and 25 past its advance,
      ;; more than a space: room before it, and a space as wide as needed.
      (setq ink '(("☺" 2 . 35)))
      (run-hooks 'after-setting-font-hook)
      (let ((string drover-attention-mode-line))
        (should (equal (substring-no-properties string) " ☺ 2"))
        (should (equal (get-text-property 0 'display string) '(space :width (2))))
        (should (equal (get-text-property 2 'display string) '(space :width (25))))
        (let ((spans (drover-attention-test-spans string ink)))
          (should (equal spans '((0 37 "☺") (37 47 "2")))))
        ;; The room belongs to the count: one face, clicks and tooltip.
        (dotimes (index (length string))
          (should (eq (get-text-property index 'face string) 'opencode-session-status-waiting))
          (should (eq (get-text-property index 'local-map string) drover-attention-mode-line-map))
          (should (get-text-property index 'help-echo string)))))))

(ert-deftest drover-attention-test-glyphs-measured-once-per-font ()
  "The glyph is measured once per font and face, though the count changes often."
  (drover-attention-test-with (list (drover-test-session "w" :status 'waiting :since 1.0))
    (drover-attention-test-with-font '(("☺" 0 . 16))
      (drover-attention-mode 1)
      (should (= (length drover-attention-test-measured) 1))
      (dotimes (_ 3)
        (setq drover-test-sessions (cons (drover-test-session (format "w%d" (length drover-test-sessions))
                                                              :status 'waiting :since 2.0)
                                         drover-test-sessions))
        (drover-backend-changed 'fake)
        (drover-changed-now))
      (should (equal (substring-no-properties drover-attention-mode-line) "☺ 4"))
      (should (= (length drover-attention-test-measured) 1))
      ;; In another face, as when the most urgent status changes: once.
      (dotimes (_ 2)
        (setq drover-test-sessions (list (drover-test-session "d" :status 'done)))
        (drover-backend-changed 'fake)
        (drover-changed-now))
      (should (= (length drover-attention-test-measured) 2))
      ;; Another font for the mode line: measured again.
      (cl-letf (((symbol-function 'face-font) (lambda (&rest _) "-other-font-")))
        (drover-attention--update))
      (should (= (length drover-attention-test-measured) 3))
      ;; A font set, or a theme enabled, measures again, and redraws.
      (should (memq #'drover-attention--fonts-changed after-setting-font-hook))
      (should (memq #'drover-attention--fonts-changed enable-theme-functions))
      (setq ink '(("☺" 0 . 30)))
      (run-hooks 'after-setting-font-hook)
      (should (= (length drover-attention-test-measured) 4))
      (should (equal (get-text-property 1 'display drover-attention-mode-line) '(space :width (20))))
      (run-hook-with-args 'enable-theme-functions 'some-theme)
      (should (= (length drover-attention-test-measured) 5))
      ;; Off, the mode follows fonts no more.
      (drover-attention-mode -1)
      (should-not (memq #'drover-attention--fonts-changed after-setting-font-hook))
      (should-not (memq #'drover-attention--fonts-changed enable-theme-functions)))))

;;;; Enabling and disabling

(ert-deftest drover-attention-test-mode-from-nil-mode-string ()
  (drover-attention-test-with nil
    (drover-attention-mode 1)
    (should (equal global-mode-string '("" drover-attention-mode-line)))
    (should (memq #'drover-attention--changed drover-changed-hook))
    (drover-attention-mode 1)
    (should (equal global-mode-string '("" drover-attention-mode-line)))
    (drover-attention-mode -1)
    (should (null global-mode-string))
    (should-not (memq #'drover-attention--changed drover-changed-hook))
    (should-not drover-attention--mode-string-origin)))

(ert-deftest drover-attention-test-mode-from-list-mode-string ()
  (drover-attention-test-with nil
    (setq global-mode-string (list "" 'display-time-string))
    (drover-attention-mode 1)
    (should (equal global-mode-string '("" display-time-string drover-attention-mode-line)))
    (drover-attention-mode -1)
    (should (equal global-mode-string '("" display-time-string)))))

(ert-deftest drover-attention-test-mode-from-string-mode-string ()
  (drover-attention-test-with nil
    (setq global-mode-string "[clock]")
    (drover-attention-mode 1)
    (should (equal global-mode-string '("[clock]" drover-attention-mode-line)))
    (drover-attention-mode -1)
    (should (equal global-mode-string "[clock]"))))

(ert-deftest drover-attention-test-mode-keeps-later-additions ()
  (drover-attention-test-with nil
    (drover-attention-mode 1)
    (setq global-mode-string (append global-mode-string '(other-string)))
    (drover-attention-mode -1)
    (should (equal global-mode-string '("" other-string)))))

(ert-deftest drover-attention-test-disable-clears-state ()
  (drover-attention-test-with (list (drover-test-session "w" :status 'waiting))
    (drover-attention-mode 1)
    (drover-next)
    (should drover-attention-mode-line)
    (should drover-attention--queue)
    (should (memq #'drover-attention--after-command post-command-hook))
    (drover-attention-mode -1)
    (should-not drover-attention-mode-line)
    (should-not drover-attention--queue)
    (should-not (memq #'drover-attention--after-command post-command-hook))
    ;; Changes no longer touch the mode line.
    (drover-backend-changed 'fake)
    (drover-changed-now)
    (should-not drover-attention-mode-line)))

(ert-deftest drover-attention-test-enable-starts-backends ()
  (drover-attention-test-with nil
    (drover-attention-mode 1)
    (should (equal drover-test-calls '((:start nil))))))

(ert-deftest drover-attention-test-starts-later-backends ()
  (drover-attention-test-with nil
    (drover-attention-mode 1)
    (let ((started nil))
      (drover-register-backend 'late :label "Late" :sessions #'ignore
                               :start (lambda () (setq started t)))
      (drover-changed-now)
      (should started))))

(ert-deftest drover-attention-test-unload-function-disables ()
  (drover-attention-test-with nil
    (drover-attention-mode 1)
    (should-not (drover-attention-unload-function))
    (should-not drover-attention-mode)
    (should (null global-mode-string))))

;;;; Visiting the next session

(defun drover-attention-test-sessions ()
  "Return sessions in a known round: w, f, d1, d2, which need the user, then i.
i is idle; r, working, is never visited."
  (list (drover-test-session "i" :status 'idle :updated 100.0)
        (drover-test-session "d2" :status 'done :since 40.0)
        (drover-test-session "w" :status 'waiting :since 50.0)
        (drover-test-session "r" :status 'working :since 10.0)
        (drover-test-session "d1" :status 'done :since 30.0)
        (drover-test-session "f" :status 'failed :since 60.0)))

(ert-deftest drover-attention-test-next-most-urgent ()
  (drover-attention-test-with (drover-attention-test-sessions)
    (drover-next)
    (should (equal drover-test-calls '((:open "w" nil))))))

(ert-deftest drover-attention-test-next-visits-what-it-counts ()
  "The sessions visited are those counted: hidden subagents are not, but asking ones."
  (drover-attention-test-with (list (drover-test-session "root" :status 'working)
                                    (drover-test-session "ended" :parent "root" :status 'done :since 1.0)
                                    (drover-test-session "asking" :parent "root" :status 'waiting :since 2.0)
                                    (drover-test-session "alone" :status 'done :since 3.0))
    (let ((drover-show-subagents 'none))
      (drover-next)
      (drover-attention-test-press)
      (drover-attention-test-press))
    ;; The subagent through its parent, which answers it.
    (should (equal (drover-attention-test-opened) '("root" "alone" "root")))))

(ert-deftest drover-attention-test-next-cycles-and-wraps ()
  "Each press visits the next of the queue, the first again after the last."
  (drover-attention-test-with (drover-attention-test-sessions)
    (should (equal (drover-attention-test-messages
                     (drover-next)
                     (dotimes (_ 6)
                       (drover-attention-test-press)))
                   '("1/5" "2/5" "3/5" "4/5" "5/5" "1/5" "2/5")))
    (should (equal (drover-attention-test-opened) '("w" "f" "d1" "d2" "i" "w" "f")))))

(ert-deftest drover-attention-test-next-keeps-first-order ()
  "Sessions changing during a round leave its queue as it was, after the last too."
  (drover-attention-test-with (drover-attention-test-sessions)
    (drover-next)
    ;; Visiting reads the session, and another one now waits longest.
    (setq drover-test-sessions
          (list (drover-test-session "w" :status 'idle)
                (drover-test-session "d2" :status 'waiting :since 1.0)
                (drover-test-session "d1" :status 'done :since 30.0)
                (drover-test-session "f" :status 'failed :since 60.0)))
    (drover-backend-changed 'fake)
    (drover-changed-now)
    (dotimes (_ 4)
      (drover-attention-test-press))
    ;; w no longer needs the user, but is still the round's first; i,
    ;; gone, is passed over.
    (should (equal (drover-attention-test-opened) '("w" "f" "d1" "d2" "w")))
    (should (equal drover-attention--queue
                   '((fake . "w") (fake . "f") (fake . "d1") (fake . "d2") (fake . "i"))))))

(ert-deftest drover-attention-test-next-newcomers-wait ()
  "Sessions coming to need the user during a round wait for the next one."
  (drover-attention-test-with (drover-attention-test-sessions)
    (should (equal (drover-attention-test-messages (drover-next)) '("1/5")))
    (setq drover-test-sessions
          (append drover-test-sessions
                  (list (drover-test-session "m" :status 'done :since 5.0)
                        (drover-test-session "n" :status 'waiting :since 70.0))))
    (drover-backend-changed 'fake)
    (drover-changed-now)
    (should (equal (drover-attention-test-messages
                     (dotimes (_ 5)
                       (drover-attention-test-press)))
                   '("2/5" "3/5" "4/5" "5/5" "1/5")))
    (should (equal (drover-attention-test-opened) '("w" "f" "d1" "d2" "i" "w")))
    ;; Another command ends the round: the next press lines them up anew.
    (drover-attention-test-other-command)
    (should (equal (drover-attention-test-messages (drover-attention-test-press)) '("1/7")))
    (should (equal (car (last (drover-attention-test-opened))) "w"))
    (drover-attention-test-press)
    (should (equal (car (last (drover-attention-test-opened))) "n"))))

(ert-deftest drover-attention-test-next-skips-gone-sessions ()
  "A session of the queue gone is passed over silently, the place told counting it."
  (drover-attention-test-with (drover-attention-test-sessions)
    (drover-next)
    (setq drover-test-sessions (seq-remove (lambda (session)
                                             (equal (drover-session-id session) "f"))
                                           drover-test-sessions))
    (should (equal (drover-attention-test-messages (drover-attention-test-press)) '("3/5")))
    (should (equal (drover-attention-test-opened) '("w" "d1")))
    ;; And as the round comes round again.
    (dotimes (_ 3)
      (drover-attention-test-press))
    (should (equal (drover-attention-test-opened) '("w" "d1" "d2" "i" "w")))))

(ert-deftest drover-attention-test-next-opens-fresh-records ()
  (drover-attention-test-with (drover-attention-test-sessions)
    (drover-next)
    (let ((opened nil))
      (setq drover-test-sessions (list (drover-test-session "w" :status 'waiting)
                                       (drover-attention-test-titled "f" "Renamed" :status 'idle)))
      (cl-letf (((symbol-function 'drover-open) (lambda (session &rest _) (setq opened session))))
        (drover-attention-test-press))
      (should (equal (drover-session-title opened) "Renamed")))))

(ert-deftest drover-attention-test-next-restarts-after-other-command ()
  "Another command between presses ends the round; the next press begins anew."
  (drover-attention-test-with (drover-attention-test-sessions)
    (drover-next)
    (drover-attention-test-press)
    (should (memq #'drover-attention--after-command post-command-hook))
    (setq drover-test-sessions
          (list (drover-test-session "d1" :status 'done :since 30.0)
                (drover-test-session "f" :status 'failed :since 60.0)))
    (drover-attention-test-other-command)
    (should-not drover-attention--queue)
    ;; No round, nothing runs after commands.
    (should-not (memq #'drover-attention--after-command post-command-hook))
    (drover-next)
    (should (equal (drover-attention-test-opened) '("w" "f" "f")))
    (should (equal drover-attention--queue '((fake . "f") (fake . "d1"))))))

(ert-deftest drover-attention-test-next-round-gone ()
  "Every session of the round gone, the next press lines sessions up anew."
  (drover-attention-test-with (list (drover-test-session "a" :status 'waiting))
    (drover-next)
    (setq drover-test-sessions (list (drover-test-session "b" :status 'done)))
    (should (equal (drover-attention-test-messages (drover-attention-test-press)) '("1/1")))
    (should (equal (drover-attention-test-opened) '("a" "b")))
    (should (equal drover-attention--queue '((fake . "b"))))))

(ert-deftest drover-attention-test-next-single-session-wraps ()
  (drover-attention-test-with (list (drover-test-session "a" :status 'waiting))
    (drover-next)
    (drover-attention-test-press)
    (should (equal (drover-attention-test-opened) '("a" "a")))))

(ert-deftest drover-attention-test-next-nothing-to-do ()
  "With every session in the middle of a turn, nothing is visited, and the echo says so.
Not even a session whose cache lives, nor an idle subagent."
  (drover-attention-test-with (list (drover-test-session "r" :status 'working
                                                         :cache-expires (+ (float-time) 60) :cache-ttl 300)
                                    (drover-test-session "t" :status 'retrying)
                                    (drover-test-session "s" :parent "r" :status 'idle))
    (let* ((drover-show-subagents 'all)
           (messages (drover-attention-test-messages (drover-next))))
      (should (equal messages '("No session to visit")))
      (should-not drover-test-calls)
      (should-not drover-attention--queue)
      (should-not (memq #'drover-attention--after-command post-command-hook))
      (should (equal (drover-attention-test-messages (drover-attention-test-press))
                     '("No session to visit")))
      (should-not drover-test-calls))))

(ert-deftest drover-attention-test-next-goes-on-to-idle ()
  "Once no session needs the user, a press visits the idle ones, as lists draw them."
  (drover-attention-test-with (list (drover-test-session "r" :status 'working)
                                    (drover-test-session "i1" :status 'idle :updated 10.0)
                                    (drover-test-session "i2" :status 'idle :updated 20.0))
    (let ((drover-group-by nil)
          (drover-sort-by 'updated)
          (drover-sort-descending t))
      (should (equal (drover-attention-test-messages
                       (drover-next)
                       (drover-attention-test-press)
                       (drover-attention-test-press))
                     '("1/2" "2/2" "1/2")))
      (should (equal (drover-attention-test-opened) '("i2" "i1" "i2"))))))

(ert-deftest drover-attention-test-next-echoes-position ()
  (drover-attention-test-with (list (drover-test-session "w" :status 'waiting :since 1.0
                                                         :requests '((:kind question :summary "Which?")))
                                    (drover-test-session "d" :status 'done :since 2.0))
    (should (equal (drover-attention-test-messages
                     (drover-next)
                     (drover-attention-test-press))
                   '("1/2" "2/2")))
    (let ((drover-attention-echo nil))
      (should-not (drover-attention-test-messages (drover-attention-test-press))))))

(ert-deftest drover-attention-test-next-uses-selected-window ()
  (drover-attention-test-with (list (drover-test-session "w" :status 'waiting))
    (drover-next)
    (should (equal drover-test-calls '((:open "w" nil))))))

(ert-deftest drover-attention-test-chains ()
  "What goes on with a round: the command, the menu, Transient, and no intent."
  (dolist (command '(drover-next drover-dispatch transient-quit-one transient-undefined transient-noop))
    (should (drover-attention--chains-p command ?j)))
  ;; A mouse button pressed, whose release runs the click's command.
  (should (drover-attention--chains-p 'mouse-drag-mode-line '(down-mouse-1 nil)))
  (should (drover-attention--chains-p 'mouse-drag-region '(down-mouse-1 nil)))
  (dolist (event '((mouse-movement nil) (switch-frame nil) (select-window nil) (focus-in nil)))
    (should (drover-attention--chains-p 'whatever event)))
  ;; Anything else, a click included.
  (dolist (command '(forward-char self-insert-command undo drover-list drover-select transient-mark-mode))
    (should-not (drover-attention--chains-p command ?x)))
  (should-not (drover-attention--chains-p 'mouse-set-point '(mouse-1 nil)))
  ;; No command, as the command loop runs the hook as it starts.
  (should (drover-attention--chains-p nil nil)))

;;;; Visiting through keys

(defvar drover-attention-test-shown nil
  "Sessions of the backend `shown', whose :open shows each in a buffer.")

(defun drover-attention-test-shown-buffer (id)
  "Return the buffer the backend `shown' shows session ID in."
  (get-buffer-create (format "*drover-attention-test %s*" id)))

(defun drover-attention-test-shown-session (id &rest properties)
  "Return a session ID of the backend `shown', ended unseen, with PROPERTIES."
  (apply #'drover-session-create :backend 'shown :id id :title (concat "Session " id)
         (append properties '(:status done))))

(defun drover-attention-test-show (session &rest _)
  "Show SESSION in its buffer in the selected window, as a backend's :open.
The buffer tells SESSION as its session at point, as a session's buffer
does; nothing marks it seen, as for a session of another host."
  (let ((id (drover-session-id session)))
    (push (list :open id nil) drover-test-calls)
    (with-current-buffer (drover-attention-test-shown-buffer id)
      (setq-local drover-session-at-point-function
                  (lambda () (seq-find (lambda (session) (equal (drover-session-id session) id))
                                       drover-attention-test-shown))))
    (set-window-buffer (selected-window) (drover-attention-test-shown-buffer id))))

(defun drover-attention-test-spec-id (spec)
  "Return the identifier of SPEC, an identifier or (ID . PROPERTIES)."
  (if (consp spec) (car spec) spec))

(defmacro drover-attention-test-with-keys (ids &rest body)
  "Run BODY with sessions IDS of the backend `shown', which needs you each.
Each of IDS is an identifier, or (ID . PROPERTIES) for a session of
other properties than having ended unseen.  <f19> opens
`drover-dispatch', where j runs `drover-next', as Caps Lock does; <f18>
runs `drover-next' itself, and <f17> another command."
  (declare (indent 1) (debug t))
  `(drover-attention-test-with nil
     (let ((drover-attention-test-shown
            (seq-map-indexed (lambda (spec index)
                               (apply #'drover-attention-test-shown-session
                                      (drover-attention-test-spec-id spec)
                                      (append (and (consp spec) (cdr spec))
                                              (list :since (float (1+ index))))))
                             ,ids))
           (map (make-sparse-keymap))
           (window-buffer (window-buffer (selected-window))))
       (drover-test-register-simple 'shown (lambda () drover-attention-test-shown)
                                    :open #'drover-attention-test-show
                                    :buffer (lambda (session)
                                              (get-buffer (format "*drover-attention-test %s*"
                                                                  (drover-session-id session)))))
       (drover-backend-changed 'shown)
       (drover-changed-now)
       (keymap-set map "<f19>" #'drover-dispatch)
       (keymap-set map "<f18>" #'drover-next)
       (keymap-set map "<f17>" #'end-of-buffer)
       (unwind-protect
           (let ((overriding-local-map map))
             ,@body)
         (set-window-buffer (selected-window) window-buffer)
         (dolist (spec ,ids)
           (kill-buffer (drover-attention-test-shown-buffer (drover-attention-test-spec-id spec))))))))

(defun drover-attention-test-keys (keys)
  "Type KEYS, as `kbd' reads them, through the command loop."
  (execute-kbd-macro (kbd keys)))

(defun drover-attention-test-reshow (sessions)
  "Have the backend `shown' serve SESSIONS from now on."
  (setq drover-attention-test-shown sessions)
  (drover-backend-changed 'shown)
  (drover-changed-now))

(ert-deftest drover-attention-test-keys-through-menu ()
  "Through the menu, each press visits the next session, though none is marked seen."
  (drover-attention-test-with-keys '("a" "b" "c")
    (should (equal (remove "" (drover-attention-test-messages
                                (dotimes (_ 3)
                                  (drover-attention-test-keys "<f19> j"))))
                   '("1/3" "2/3" "3/3")))
    (should (equal (drover-attention-test-opened) '("a" "b" "c")))
    (should (eq (window-buffer (selected-window)) (drover-attention-test-shown-buffer "c")))
    ;; After the last, the first again.
    (drover-attention-test-keys "<f19> j")
    (drover-attention-test-keys "<f19> j")
    (should (equal (drover-attention-test-opened) '("a" "b" "c" "a" "b")))))

(ert-deftest drover-attention-test-keys-direct-binding ()
  (drover-attention-test-with-keys '("a" "b" "c")
    (drover-attention-test-keys "<f18> <f18> <f18> <f18>")
    (should (equal (drover-attention-test-opened) '("a" "b" "c" "a")))))

(ert-deftest drover-attention-test-keys-menu-and-binding-mixed ()
  "The menu and a key of its own go on with the same round, in either order."
  (drover-attention-test-with-keys '("a" "b" "c")
    (drover-attention-test-keys "<f19> j <f18> <f19> j")
    (should (equal (drover-attention-test-opened) '("a" "b" "c"))))
  (drover-attention-test-with-keys '("a" "b" "c")
    (drover-attention-test-keys "<f18> <f19> j <f18>")
    (should (equal (drover-attention-test-opened) '("a" "b" "c")))))

(ert-deftest drover-attention-test-keys-other-command-ends-round ()
  "Any other command between presses ends the round, from the menu or a key."
  (dolist (keys '("<f19> j" "<f18>"))
    (drover-attention-test-with-keys '("a" "b" "c")
      (drover-attention-test-keys (concat keys " " keys))
      (should (equal (drover-attention-test-opened) '("a" "b")))
      (drover-attention-test-keys "<f17>")
      (should-not drover-attention--queue)
      (should-not (memq #'drover-attention--after-command post-command-hook))
      (should (equal (remove "" (drover-attention-test-messages (drover-attention-test-keys keys)))
                     '("1/3")))
      (should (equal (list keys (drover-attention-test-opened)) (list keys '("a" "b" "a")))))))

(ert-deftest drover-attention-test-keys-menu-machinery-chains ()
  "The menu opened and closed, or refusing a key, leaves the round going."
  (drover-attention-test-with-keys '("a" "b" "c")
    ;; Opened and closed without pressing j.
    (drover-attention-test-keys "<f19> j <f19> C-g <f19> j")
    (should (equal (drover-attention-test-opened) '("a" "b")))
    ;; A key the menu does not bind, which it refuses, staying open.
    (drover-attention-test-keys "<f19> <f16> j")
    (should (equal (drover-attention-test-opened) '("a" "b" "c")))))

(defun drover-attention-test-extended-command ()
  "Run a command as M-x does: read its name in a recursive edit, then run it.
In batch, a minibuffer reads standard input; a recursive edit runs the
commands typed in it as a minibuffer does, one level deeper."
  (interactive)
  (let ((buffer (get-buffer-create " *drover-attention-test command name*")))
    (with-current-buffer buffer
      (erase-buffer))
    (save-window-excursion
      (set-window-buffer (selected-window) buffer)
      (with-current-buffer buffer
        (recursive-edit)))
    (let ((suggest-key-bindings nil)
          (extended-command-suggest-shorter nil))
      (execute-extended-command nil (with-current-buffer buffer (string-trim (buffer-string)))))))

(ert-deftest drover-attention-test-keys-extended-command ()
  "M-x drover-next goes on with the round; M-x of another command ends it.
The commands typing the name are M-x's, judged as it ends."
  (drover-attention-test-with-keys '("a" "b" "c")
    (keymap-set map "<f15>" #'drover-attention-test-extended-command)
    (drover-attention-test-keys "<f18> <f15> d r o v e r - n e x t C-M-c")
    (should (equal (drover-attention-test-opened) '("a" "b")))
    (should drover-attention--queue)
    (drover-attention-test-keys "<f15> e n d - o f - b u f f e r C-M-c")
    (should-not drover-attention--queue)
    (drover-attention-test-keys "<f18>")
    (should (equal (drover-attention-test-opened) '("a" "b" "a")))))

(ert-deftest drover-attention-test-keys-mouse-click ()
  "A click runs a command on its release: the press does not end the round."
  (drover-attention-test-with-keys '("a" "b" "c")
    (keymap-set map "<down-mouse-1>" #'end-of-buffer)
    (keymap-set map "<mouse-1>" #'drover-next)
    (keymap-set map "<down-mouse-3>" #'end-of-buffer)
    (keymap-set map "<mouse-3>" #'end-of-buffer)
    (cl-flet ((click (button)
                (let ((position (posn-at-point (point-min))))
                  (execute-kbd-macro (vector (list (intern (format "down-mouse-%d" button)) position)
                                             (list (intern (format "mouse-%d" button)) position))))))
      (click 1)
      (click 1)
      (should (equal (drover-attention-test-opened) '("a" "b")))
      ;; Another click's release is another command.
      (click 3)
      (click 1)
      (should (equal (drover-attention-test-opened) '("a" "b" "a"))))))

(ert-deftest drover-attention-test-keys-first-press-on-shown ()
  "A round begins with its first session, though it shows already."
  (drover-attention-test-with-keys '("a" "b" "c")
    (set-window-buffer (selected-window) (drover-attention-test-shown-buffer "a"))
    ;; The menu clears the echo area as it opens and closes.
    (should (equal (remove "" (drover-attention-test-messages (drover-attention-test-keys "<f19> j")))
                   '("1/3")))
    (should (equal (drover-attention-test-opened) '("a")))
    (drover-attention-test-keys "<f19> j")
    (should (equal (drover-attention-test-opened) '("a" "b"))))
  ;; Alone, it is visited again.
  (drover-attention-test-with-keys '("a")
    (drover-attention-test-keys "<f19> j <f19> j")
    (should (equal (drover-attention-test-opened) '("a" "a")))))

(ert-deftest drover-attention-test-keys-skips-gone ()
  (drover-attention-test-with-keys '("a" "b" "c")
    (drover-attention-test-keys "<f19> j")
    (drover-attention-test-reshow (seq-remove (lambda (session) (equal (drover-session-id session) "b"))
                                              drover-attention-test-shown))
    (drover-attention-test-keys "<f19> j")
    (should (equal (drover-attention-test-opened) '("a" "c")))))

(ert-deftest drover-attention-test-keys-events-leave-queue ()
  "Sessions changing during a round, coming or ceasing to need you, leave its queue."
  (let ((now (float-time)))
    (dolist (keys '("<f19> j" "<f18>"))
      (drover-attention-test-with-keys '(("a" :status waiting) "b" "c")
        (drover-attention-test-keys keys)
        ;; d comes to need you with a cache about to expire, a stops,
        ;; c's cache is renewed: none of it reorders the round.
        (drover-attention-test-reshow
         (list (drover-attention-test-shown-session "a" :status 'idle :since 1.0)
               (drover-attention-test-shown-session "b" :since 2.0)
               (drover-attention-test-shown-session "c" :since 3.0 :cache-expires (+ now 600)
                                                    :cache-ttl 3600)
               (drover-attention-test-shown-session "d" :since 9.0 :cache-expires (+ now 60)
                                                    :cache-ttl 300)))
        (should (equal (remove "" (drover-attention-test-messages
                                    (drover-attention-test-keys (string-join (make-list 3 keys) " "))))
                       '("2/3" "3/3" "1/3")))
        (should (equal (list keys (drover-attention-test-opened)) (list keys '("a" "b" "c" "a"))))
        ;; The next round takes them as they stand.
        (drover-attention-test-keys (concat "<f17> " keys " " keys))
        (should (equal (list keys (drover-attention-test-opened))
                       (list keys '("a" "b" "c" "a" "d" "c"))))
        (kill-buffer (drover-attention-test-shown-buffer "d"))))))

(ert-deftest drover-attention-test-keys-session-shown-late ()
  "A session its backend shows later is passed by the next press; showing ends nothing."
  (drover-attention-test-with-keys '("a" "b" "c")
    (let ((home (window-buffer (selected-window))))
      (cl-letf (((symbol-function 'drover-attention-test-show)
                 (lambda (session &rest _)
                   (push (list :open (drover-session-id session) nil) drover-test-calls))))
        (drover-attention-test-keys "<f19> j <f19> j")
        (should (equal (drover-attention-test-opened) '("a" "b")))
        (should (eq (window-buffer (selected-window)) home))
        ;; Its buffer shows once fetched, no command running: the round goes on.
        (set-window-buffer (selected-window) (drover-attention-test-shown-buffer "b"))
        (drover-attention-test-keys "<f18>")
        (should (equal (drover-attention-test-opened) '("a" "b" "c")))))))

(ert-deftest drover-attention-test-keys-open-fails ()
  "A session its backend fails to open is told on one line; the next press goes on."
  (drover-attention-test-with-keys '("a" "b")
    (cl-letf (((symbol-function 'drover-attention-test-show)
               (lambda (session &rest _)
                 (push (list :open (drover-session-id session) nil) drover-test-calls)
                 (error "Cannot open\n%s" (make-string 500000 ?x)))))
      (let ((messages (remove "" (drover-attention-test-messages
                                   (drover-attention-test-keys "<f19> j")
                                   (drover-attention-test-keys "<f18>")))))
        (should (= (length messages) 2))
        (should (string-prefix-p "1/2 Session a did not open: Cannot open" (car messages)))
        (should (string-prefix-p "2/2 Session b did not open: Cannot open" (cadr messages)))
        (should (seq-every-p (lambda (message)
                               (and (not (string-search "\n" message)) (< (length message) 400)))
                             messages))))
    (should (equal (drover-attention-test-opened) '("a" "b")))))

(ert-deftest drover-attention-test-keys-one-press-one-visit ()
  "One press visits one session and tells one place, from the menu or a key."
  (dolist (keys '("<f19> j" "<f18>"))
    (drover-attention-test-with-keys '("a" "b" "c")
      (should (equal (list keys (remove "" (drover-attention-test-messages (drover-attention-test-keys keys))))
                     (list keys '("1/3"))))
      (should (equal (drover-attention-test-opened) '("a")))
      (should (equal (list keys (remove "" (drover-attention-test-messages (drover-attention-test-keys keys))))
                     (list keys '("2/3"))))
      (should (equal (drover-attention-test-opened) '("a" "b"))))))

(ert-deftest drover-attention-test-keys-main-window ()
  "From a list beside the window where sessions show, sessions show there."
  (drover-attention-test-with-keys '("a" "b" "c")
    (let* ((main (selected-window))
           (side (split-window main))
           (list (get-buffer-create " *drover-attention-test list*")))
      (unwind-protect
          (progn
            (with-current-buffer list
              (setq-local drover-view-p t)
              (setq-local drover-main-window-function (lambda () main)))
            (set-window-buffer side list)
            (drover-attention-test-keys "<f19> j")
            (select-window side)
            (drover-attention-test-keys "<f19> j")
            (should (equal (drover-attention-test-opened) '("a" "b")))
            (should (eq (window-buffer main) (drover-attention-test-shown-buffer "b"))))
        (delete-window side)
        (kill-buffer list)))))

(defmacro drover-attention-test-with-prompt (&rest body)
  "Run BODY with `prompt' bound to a buffer of drover's prompt."
  (declare (indent 0) (debug t))
  `(let ((prompt (get-buffer-create "*drover-attention-test prompt*")))
     (unwind-protect
         (progn
           (with-current-buffer prompt
             (drover-prompt-mode 1))
           ,@body)
       (kill-buffer prompt))))

(defmacro drover-attention-test-with-side (buffer &rest body)
  "Run BODY with `side' bound to a side window showing BUFFER, deleted after."
  (declare (indent 1) (debug t))
  `(let ((side (display-buffer-in-side-window ,buffer '((side . bottom)))))
     (unwind-protect
         (progn ,@body)
       (when (window-live-p side)
         (delete-window side)))))

(ert-deftest drover-attention-test-keys-prompt-side-window ()
  "From the prompt's side window, sessions show in the window the user came from."
  (drover-attention-test-with-keys '("a" "b" "c")
    (drover-attention-test-with-prompt
      (let ((main (selected-window)))
        (drover-attention-test-keys "<f19> j")
        (drover-attention-test-with-side prompt
          (select-window side)
          (drover-attention-test-keys "<f19> j")
          (should (equal (drover-attention-test-opened) '("a" "b")))
          (should (eq (window-buffer main) (drover-attention-test-shown-buffer "b")))
          (should (eq (window-buffer side) prompt))
          ;; Through a key of its own alike.
          (select-window side)
          (drover-attention-test-keys "<f18>")
          (should (equal (drover-attention-test-opened) '("a" "b" "c")))
          (should (eq (window-buffer main) (drover-attention-test-shown-buffer "c"))))))))

(ert-deftest drover-attention-test-keys-subagent-through-root ()
  "A subagent waiting is visited through the top of its tree, by its cache, once."
  (let* ((now (float-time))
         (sessions `(("s" :cache-expires ,(+ now 1200) :cache-ttl 3600)
                     ("r" :status working :cache-expires ,(+ now 600) :cache-ttl 3600)
                     ("k" :parent "r" :status waiting
                      :requests ((:kind permission :summary "Edit emacs.org")))
                     ("j" :parent "r" :status waiting)
                     "u")))
    (drover-attention-test-with-keys sessions
      (should (equal (drover-attention-test-ids (drover-attention-order (drover-attention-sessions) now))
                     '("k" "j" "s" "u")))
      (should (equal (remove "" (drover-attention-test-messages (drover-attention-test-keys "<f19> j")))
                     '("1/3")))
      (should (eq (window-buffer (selected-window)) (drover-attention-test-shown-buffer "r")))
      (drover-attention-test-keys "<f19> j <f18>")
      (should (equal (drover-attention-test-opened) '("r" "s" "u")))
      (should (equal drover-attention--queue '((shown . "r") (shown . "s") (shown . "u"))))
      ;; A new round begins on it again, shown.
      (drover-attention-test-keys "<f17> <f18>")
      (should (equal (drover-attention-test-opened) '("r" "s" "u" "r"))))))

;;;; The order of visits

(defun drover-attention-test-ids (sessions)
  "Return the identifiers of SESSIONS."
  (mapcar #'drover-session-id sessions))

(defun drover-attention-test-cached (now)
  "Return sessions, some with a prompt cache live at NOW.
Of those needing the user, c2 and c1 come first, then the others as
lists draw them; a round begins with i, idle, whose cache expires
first."
  (list (drover-test-session "d1" :status 'done :since 30.0)
        (drover-test-session "c1" :status 'done :since 40.0
                             :cache-expires (+ now 600) :cache-ttl 3600)
        (drover-test-session "w" :status 'waiting :since 50.0)
        (drover-test-session "x" :status 'done :since 20.0
                             :cache-expires (- now 10) :cache-ttl 300)
        (drover-test-session "c2" :status 'failed :since 60.0
                             :cache-expires (+ now 120) :cache-ttl 300)
        (drover-test-session "root" :status 'working :since 5.0)
        ;; A subagent's cache is its parent's to reuse, never its own.
        (drover-test-session "k" :parent "root" :status 'waiting :since 55.0
                             :cache-expires (+ now 60) :cache-ttl 300)
        (drover-test-session "i" :status 'idle :cache-expires (+ now 30) :cache-ttl 300)))

(ert-deftest drover-attention-test-next-caches-first ()
  "Live caches first, the soonest to expire first, then the others as lists draw them.
An expired cache counts as none, and so does a subagent's.  A cache
comes first whether its session needs the user or is idle; one of a
session working does not count, the session being left out."
  (let ((now (float-time)))
    (drover-attention-test-with (drover-attention-test-cached now)
      (should (equal (drover-attention-test-ids (drover-attention-order (drover-attention-sessions) now))
                     '("c2" "c1" "w" "k" "x" "d1")))
      (drover-next)
      (dotimes (_ 7)
        (drover-attention-test-press))
      (should (equal (drover-attention-test-opened) '("i" "c2" "c1" "w" "root" "x" "d1" "i"))))))

(ert-deftest drover-attention-test-next-tiers ()
  "A round visits live caches, then sessions needing the user, then open idle ones, then the others.
Each in the order lists draw them; a session whose cache lives comes by
its cache, whatever else it is; a session working never comes, its
cache live or not."
  (let ((now (float-time))
        (buffers (list (generate-new-buffer " *open1*") (generate-new-buffer " *open2*"))))
    (unwind-protect
        (drover-attention-test-with
            (list (drover-test-session "closed1" :status 'idle :updated 90.0)
                  (drover-test-session "open1" :status 'idle :updated 80.0)
                  (drover-test-session "done1" :status 'done :since 70.0 :updated 70.0)
                  (drover-test-session "closed2" :status 'idle :updated 60.0)
                  (drover-test-session "open2" :status 'idle :updated 50.0)
                  (drover-test-session "done2" :status 'done :since 40.0 :updated 40.0)
                  (drover-test-session "cached-idle" :status 'idle :updated 30.0
                                       :cache-expires (+ now 100) :cache-ttl 300)
                  (drover-test-session "cached-done" :status 'done :since 20.0 :updated 20.0
                                       :cache-expires (+ now 200) :cache-ttl 300)
                  (drover-test-session "busy" :status 'working :updated 10.0
                                       :cache-expires (+ now 50) :cache-ttl 300))
          (let ((drover-group-by nil)
                (drover-sort-by 'updated)
                (drover-sort-descending t)
                (open `(("open1" . ,(car buffers)) ("open2" . ,(cadr buffers)))))
            (apply #'drover-register-backend
                   'fake
                   :buffer (lambda (session) (cdr (assoc (drover-session-id session) open)))
                   (alist-get 'fake drover--backends))
            (should (equal (drover-attention-test-ids
                            (drover-visit-order (drover-attention--round-sessions) now))
                           '("cached-idle" "cached-done" "done1" "done2" "open1" "open2"
                             "closed1" "closed2")))
            (drover-next)
            (dotimes (_ 7)
              (drover-attention-test-press))
            (should (equal (drover-attention-test-opened)
                           '("cached-idle" "cached-done" "done1" "done2" "open1" "open2"
                             "closed1" "closed2")))))
      (mapc #'kill-buffer buffers))))

(ert-deftest drover-attention-test-next-tree-once ()
  "An idle session whose subagent waits for the user comes once, as that subagent does."
  (drover-attention-test-with (list (drover-test-session "other" :status 'idle :updated 90.0)
                                    (drover-test-session "root" :status 'idle :updated 10.0)
                                    (drover-test-session "asking" :parent "root" :status 'waiting
                                                         :since 5.0))
    (let ((drover-group-by nil)
          (drover-sort-by 'updated)
          (drover-sort-descending t))
      (should (equal (drover-attention-test-messages
                       (drover-next)
                       (drover-attention-test-press))
                     '("1/2" "2/2")))
      (should (equal (drover-attention-test-opened) '("root" "other"))))))

(ert-deftest drover-attention-test-order-subagent-by-root-cache ()
  "A subagent waiting ranks by the cache of the top of its tree, which answering it resumes.
The tree is visited once, through its top, where the subagent is answered."
  (let ((now (float-time)))
    (pcase-dolist (`(,root-expires ,expected)
                   `((,(+ now 2000) ("agent" "spp" "u")) (,(+ now 3500) ("spp" "agent" "u"))
                     (nil ("spp" "agent" "u"))))
      (drover-attention-test-with
          (list (drover-test-session "spp" :status 'done :since 10.0
                                     :cache-expires (+ now 3000) :cache-ttl 3600)
                (drover-test-session "u" :status 'done :since 5.0)
                (drover-test-session "drover" :status 'working :since 1.0
                                     :cache-expires root-expires :cache-ttl 3600)
                (drover-test-session "agent" :parent "drover" :status 'waiting :since 2.0
                                     :requests '((:kind permission :summary "Edit emacs.org"))))
        (should (equal (drover-attention-test-ids (drover-attention-order (drover-attention-sessions) now))
                       expected))
        (should (equal (drover-attention-cache (drover-test-session "agent" :parent "drover"
                                                                    :status 'waiting)
                                               now)
                       root-expires))
        (should (equal (drover-attention-test-messages
                         (dotimes (_ 3)
                           (drover-attention-test-press)))
                       '("1/3" "2/3" "3/3")))
        (should (equal (drover-attention-test-opened)
                       (mapcar (lambda (id) (if (equal id "agent") "drover" id)) expected)))))))

(ert-deftest drover-attention-test-order-caches-alike ()
  "Caches expiring at the same time come in the order lists draw them."
  (let ((now (float-time)))
    (drover-attention-test-with (list (drover-test-session "a" :status 'done :since 30.0
                                                           :cache-expires (+ now 60) :cache-ttl 300)
                                      (drover-test-session "b" :status 'waiting :since 40.0
                                                           :cache-expires (+ now 60) :cache-ttl 300))
      (should (equal (drover-attention-test-ids (drover-attention-order (drover-attention-sessions) now))
                     '("b" "a"))))))

(ert-deftest drover-attention-test-order-follows-view-settings ()
  "The sessions without a live cache come as `drover-view-groups' draws them."
  (let ((now (float-time)))
    (drover-attention-test-with
        (list (drover-attention-test-titled "w" "Bravo" :status 'waiting :since 50.0 :updated 50.0
                                            :project "beta" :created 3.0)
              (drover-attention-test-titled "f" "Alpha" :status 'failed :since 60.0 :updated 60.0
                                            :project "alpha" :created 1.0)
              (drover-attention-test-titled "d" "Charlie" :status 'done :since 30.0 :updated 30.0
                                            :project "beta" :created 2.0)
              (drover-attention-test-titled "e" "Delta" :status 'done :since 70.0 :updated 70.0
                                            :project "alpha" :created 4.0
                                            :cache-expires (+ now 600) :cache-ttl 3600)
              (drover-attention-test-titled "i" "Echo" :status 'idle :updated 80.0))
      (pcase-dolist (`(,grouping ,sort ,descending ,expected)
                     '((status urgency nil ("e" "w" "f" "d"))
                       (nil updated t ("e" "f" "w" "d"))
                       (nil updated nil ("e" "d" "w" "f"))
                       (nil title nil ("e" "f" "w" "d"))
                       (project created nil ("e" "d" "w" "f"))
                       (project title t ("e" "d" "w" "f"))))
        (let ((drover-group-by grouping)
              (drover-sort-by sort)
              (drover-sort-descending descending))
          (let ((order (drover-attention-test-ids (drover-attention-order (drover-attention-sessions) now)))
                (drawn (drover-attention-test-ids
                        (drover-tree-sessions (apply #'append (mapcar #'cdr (drover-view-groups nil now)))))))
            (should (equal (list grouping sort descending order)
                           (list grouping sort descending expected)))
            (should (equal (cdr order) (seq-filter (lambda (id) (member id (cdr order))) drawn)))))))))

(ert-deftest drover-attention-test-order-hidden-subagent ()
  "A subagent waiting that lists hide comes where they would draw it."
  (drover-attention-test-with (list (drover-test-session "root" :status 'working :updated 90.0)
                                    (drover-test-session "asking" :parent "root" :status 'waiting
                                                         :since 2.0 :updated 2.0)
                                    (drover-test-session "alone" :status 'done :since 3.0 :updated 50.0)
                                    (drover-test-session "older" :status 'done :since 1.0 :updated 10.0))
    (let ((drover-show-subagents 'none)
          (drover-group-by nil)
          (drover-sort-by 'updated)
          (drover-sort-descending t))
      ;; Under its parent, the latest active.
      (should (equal (drover-attention-test-ids (drover-attention-order (drover-attention-sessions)))
                     '("asking" "alone" "older")))
      (let ((drover-sort-descending nil))
        (should (equal (drover-attention-test-ids (drover-attention-order (drover-attention-sessions)))
                       '("older" "alone" "asking")))))))

(ert-deftest drover-attention-test-order-filtered-last ()
  "Sessions the filter leaves out come after those lists draw."
  (drover-attention-test-with (list (drover-test-session "a" :status 'done :since 1.0 :project "p")
                                    (drover-test-session "b" :status 'waiting :since 2.0 :project "q")
                                    (drover-test-session "c" :status 'failed :since 3.0 :project "p"))
    (should (equal (drover-attention-test-ids (drover-attention-order (drover-attention-sessions)))
                   '("b" "c" "a")))
    (let ((drover-filter '(:project "p")))
      (should (equal (drover-attention-test-ids (drover-attention-order (drover-attention-sessions)))
                     '("c" "a" "b"))))))

(ert-deftest drover-attention-test-next-keeps-order-as-caches-change ()
  "Caches expiring or renewed during a round leave its order as it was."
  (let ((now (float-time)))
    (drover-attention-test-with (list (drover-test-session "u" :status 'done :since 1.0)
                                      (drover-test-session "c" :status 'done :since 2.0
                                                           :cache-expires (+ now 300) :cache-ttl 300))
      (drover-next)
      (setq drover-test-sessions
            (list (drover-test-session "u" :status 'done :since 1.0
                                       :cache-expires (+ now 100) :cache-ttl 300)
                  (drover-test-session "c" :status 'idle :since 2.0
                                       :cache-expires (- now 1) :cache-ttl 300)))
      (drover-backend-changed 'fake)
      (drover-changed-now)
      (drover-attention-test-press)
      (should (equal (drover-attention-test-opened) '("c" "u")))
      ;; A new round takes the order as it stands.
      (drover-attention-test-other-command)
      (drover-next)
      (should (equal (car (last (drover-attention-test-opened))) "u")))))

(ert-deftest drover-attention-test-order-remote-and-stale ()
  "Sessions of a host not connected are not visited, whatever their cache."
  (let ((now (float-time)))
    (drover-attention-test-with (list (drover-test-session "s" :status 'done :since 1.0 :host "core"
                                                           :stale 10.0
                                                           :cache-expires (+ now 60) :cache-ttl 300)
                                      (drover-test-session "l" :status 'done :since 2.0))
      (should (equal (drover-attention-test-ids (drover-attention-order (drover-attention-sessions) now))
                     '("l"))))))

;;;; Choosing a session

(defvar drover-attention-test-prompt nil
  "The prompt `completing-read' last received.")

(defvar drover-attention-test-table nil
  "The completion table `completing-read' last received.")

(defmacro drover-attention-test-select (answer &rest body)
  "Run BODY answering ANSWER to `completing-read'.
`drover-attention-test-prompt' and `drover-attention-test-table' hold
what it received."
  (declare (indent 1) (debug t))
  `(let ((drover-attention-test-prompt nil)
         (drover-attention-test-table nil))
     (cl-letf (((symbol-function 'completing-read)
                (lambda (prompt collection &optional _predicate require-match &rest _)
                  (should (eq require-match t))
                  (setq drover-attention-test-prompt prompt
                        drover-attention-test-table collection)
                  ,answer)))
       ,@body)))

(defun drover-attention-test-metadata (property)
  "Return PROPERTY of the metadata of `drover-attention-test-table'."
  (completion-metadata-get (completion-metadata "" drover-attention-test-table nil) property))

(ert-deftest drover-attention-test-select-order-and-metadata ()
  (drover-attention-test-with (drover-attention-test-sessions)
    (drover-attention-test-select "Session d1"
      (drover-select)
      (should (equal drover-attention-test-prompt "Session: "))
      (should (equal (all-completions "" drover-attention-test-table)
                     '("Session w" "Session f" "Session d1" "Session d2" "Session r" "Session i")))
      (should (eq (drover-attention-test-metadata 'category) 'drover-session))
      (should (eq (drover-attention-test-metadata 'display-sort-function) #'identity))
      (should (eq (drover-attention-test-metadata 'cycle-sort-function) #'identity))
      (should (equal (all-completions "Session d" drover-attention-test-table)
                     '("Session d1" "Session d2"))))
    (should (equal drover-test-calls '((:open "d1" nil))))))

(ert-deftest drover-attention-test-select-follows-next ()
  "Sessions needing the user come in the order `drover-next' visits them, then the others."
  (let ((now (float-time)))
    (drover-attention-test-with (drover-attention-test-cached now)
      (drover-attention-test-select "Session w"
        (drover-select)
        (should (equal (all-completions "" drover-attention-test-table)
                       '("Session c2" "Session c1" "Session w" "Session k" "Session x" "Session d1"
                         "Session root" "Session i")))))))

(ert-deftest drover-attention-test-select-groups ()
  (drover-attention-test-with (drover-attention-test-sessions)
    (drover-attention-test-select "Session w"
      (drover-select)
      (let ((group (drover-attention-test-metadata 'group-function)))
        (should (equal (mapcar (lambda (name) (funcall group name nil))
                               (all-completions "" drover-attention-test-table))
                       '("Needs you" "Failed" "Done" "Done" "Working" "Idle")))
        (should (equal (funcall group "Session d1" t) "Session d1"))))))

(ert-deftest drover-attention-test-select-annotations ()
  (let ((now (float-time)))
    (drover-attention-test-with
        (list (drover-test-session "w" :status 'waiting :since (- now 300)
                                   :project "drover" :branch "main"
                                   :requests '((:kind permission :summary "bash: make test")))
              (drover-attention-test-titled "long-title" "A longer title" :status 'idle :updated (- now 7200))
              (drover-test-session "f" :status 'failed :since (- now 30) :branch "dev" :detail "Boom"))
      (drover-attention-test-select "Session w"
        (drover-select)
        (let* ((annotate (drover-attention-test-metadata 'annotation-function))
               (waiting (funcall annotate "Session w"))
               (idle (funcall annotate "A longer title"))
               (failed (funcall annotate "Session f")))
          (should (string-match-p "⚑ drover:main  *5m  permit: bash: make test\\'"
                                  (substring-no-properties waiting)))
          (should (string-match-p "· +2h\\'" (substring-no-properties idle)))
          (should (string-match-p "✗ dev +3[0-9]s  Boom\\'" (substring-no-properties failed)))
          (should (eq (drover-attention-test-face-at waiting "⚑") 'opencode-session-status-waiting))
          (should (eq (drover-attention-test-face-at waiting "drover:main") 'drover-dim))
          (should (eq (drover-attention-test-face-at waiting "5m") 'drover-dim))
          (should (eq (drover-attention-test-face-at waiting "permit") 'completions-annotations))
          ;; Columns line up whatever the candidate's width.
          (should (= (+ (string-width "Session w") (string-search "⚑" waiting))
                     (+ (string-width "A longer title") (string-search "·" idle))))
          (should (= (progn (string-match "5m" waiting) (match-end 0))
                     (progn (string-match "3[0-9]s" failed) (match-end 0))))
          (should-not (funcall annotate "Unknown")))))))

(ert-deftest drover-attention-test-select-unique-names ()
  (drover-attention-test-with (list (drover-attention-test-titled "a" "Same" :status 'waiting :since 1.0)
                                    (drover-attention-test-titled "b" "Same" :status 'waiting :since 2.0)
                                    (drover-attention-test-titled "c" "Same (a)" :status 'waiting :since 3.0)
                                    (drover-attention-test-titled "d" "Other" :status 'waiting :since 4.0))
    (drover-attention-test-select "Same (b)"
      (drover-select)
      (should (equal (all-completions "" drover-attention-test-table)
                     '("Same (a)" "Same (b)" "Same (a) <2>" "Other"))))
    (should (equal drover-test-calls '((:open "b" nil))))))

(ert-deftest drover-attention-test-select-untitled ()
  (drover-attention-test-with (list (drover-attention-test-titled "ses_1" nil))
    (drover-attention-test-select "ses_1"
      (drover-select)
      (should (equal (all-completions "" drover-attention-test-table) '("ses_1"))))
    (should (equal drover-test-calls '((:open "ses_1" nil))))))

(ert-deftest drover-attention-test-select-other-window ()
  "With a prefix argument, the session opens in another window than the selected one."
  (drover-attention-test-with (list (drover-test-session "a"))
    (save-window-excursion
      (delete-other-windows)
      (let ((origin (selected-window)))
        (drover-attention-test-select "Session a"
          (let ((current-prefix-arg '(4)))
            (call-interactively #'drover-select)))
        (should (equal drover-test-calls '((:open "a" nil))))
        (should-not (eq (selected-window) origin))
        (should (window-live-p origin))))))

(ert-deftest drover-attention-test-select-visible-sessions ()
  (drover-attention-test-with (list (drover-test-session "root")
                                    (drover-test-session "child" :parent "root")
                                    (drover-test-session "asking" :parent "root" :status 'waiting))
    (drover-attention-test-select "Session root"
      (drover-select)
      (should (equal (all-completions "" drover-attention-test-table)
                     '("Session asking" "Session root"))))))

(ert-deftest drover-attention-test-select-follows-subagent-mode ()
  "Subagents are offered as lists show them, but those waiting for you always."
  (drover-attention-test-with (list (drover-test-session "root")
                                    (drover-test-session "child" :parent "root")
                                    (drover-test-session "working" :parent "root" :status 'working)
                                    (drover-test-session "asking" :parent "root" :status 'waiting))
    (let ((drover-show-subagents 'none))
      (drover-attention-test-select "Session asking"
        (drover-select)
        (should (equal (all-completions "" drover-attention-test-table)
                       '("Session asking" "Session root")))))
    (should (equal drover-test-calls '((:open "asking" nil))))
    (let ((drover-show-subagents 'all))
      (drover-attention-test-select "Session root"
        (drover-select)
        (should (equal (all-completions "" drover-attention-test-table)
                       '("Session asking" "Session working" "Session root" "Session child")))))))

(ert-deftest drover-attention-test-select-nothing ()
  (drover-attention-test-with nil
    (drover-attention-test-select (error "Should not prompt")
      (should-error (drover-select) :type 'user-error))))

(ert-deftest drover-attention-test-select-unknown-answer ()
  (drover-attention-test-with (list (drover-test-session "a"))
    (drover-attention-test-select ""
      (should-error (drover-select) :type 'user-error))
    (should-not drover-test-calls)))

;;;; Catching up

(defmacro drover-attention-test-answering (answer &rest body)
  "Run BODY answering ANSWER to every `y-or-n-p', whose prompts it returns."
  (declare (indent 1) (debug t))
  `(let ((prompts nil))
     (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) (push prompt prompts) ,answer)))
       ,@body)
     (nreverse prompts)))

(defun drover-attention-test-marked ()
  "Return the identifiers of the sessions marked seen, in order."
  (mapcar #'cadr (seq-filter (lambda (call) (eq (car call) :mark-seen)) (reverse drover-test-calls))))

(ert-deftest drover-attention-test-mark-all-seen ()
  (drover-attention-test-with (list (drover-test-session "done" :status 'done :since 10.0)
                                    (drover-test-session "failed" :status 'failed :since 20.0)
                                    (drover-test-session "waiting" :status 'waiting :since 5.0)
                                    (drover-test-session "working" :status 'working)
                                    (drover-test-session "idle"))
    (let (messages)
      (should (equal (drover-attention-test-answering t
                       (setq messages (drover-attention-test-messages (drover-mark-all-seen))))
                     '("Mark 2 sessions seen? ")))
      (should (equal (sort (drover-attention-test-marked) #'string<) '("done" "failed")))
      (should (equal messages '("Marked 2 sessions seen"))))))

(ert-deftest drover-attention-test-mark-all-seen-declined ()
  (drover-attention-test-with (list (drover-test-session "done" :status 'done))
    (drover-attention-test-answering nil (drover-mark-all-seen))
    (should-not (drover-attention-test-marked))))

(ert-deftest drover-attention-test-mark-all-seen-older-than ()
  (drover-attention-test-with (list (drover-test-session "old" :status 'done :since (- (float-time) (* 5 3600)))
                                    (drover-test-session "new" :status 'done :since (- (float-time) 60))
                                    (drover-test-session "unknown" :status 'done))
    (should (equal (drover-attention-test-answering t (drover-mark-all-seen 2))
                   '("Mark 1 session seen (ended 2 hours ago or more)? ")))
    (should (equal (drover-attention-test-marked) '("old")))))

(ert-deftest drover-attention-test-mark-all-seen-nothing ()
  (drover-attention-test-with (list (drover-test-session "working" :status 'working))
    (should (equal (drover-attention-test-messages
                     (should-not (drover-attention-test-answering t (drover-mark-all-seen))))
                   '("No session to mark seen")))))

(ert-deftest drover-attention-test-mark-all-seen-needs-support ()
  (drover-attention-test-with (list (drover-test-session "done" :status 'done))
    (drover-register-backend 'bare :label "Bare"
                             :sessions (lambda () (list (drover-session-create :backend 'bare :id "b"
                                                                               :status 'done))))
    (should (equal (drover-attention-test-answering t (drover-mark-all-seen))
                   '("Mark 1 session seen? ")))
    (should (equal (drover-attention-test-marked) '("done")))))

(provide 'drover-attention-test)
;;; drover-attention-test.el ends here
