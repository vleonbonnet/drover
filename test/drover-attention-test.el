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
                                   (drover-attention--cycle nil)
                                   (drover-attention--position 0)
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
  "Press `drover-next' again right after the previous press."
  (let ((last-command 'drover-next))
    (drover-next)))

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
  (drover-attention-test-with (list (drover-test-session "w" :status 'waiting :since 1.0
                                                         :project "drover" :branch "main"
                                                         :requests '((:kind permission :summary "bash: make")))
                                    (drover-test-session "d" :status 'done :since 2.0)
                                    (drover-test-session "e" :status 'done :since 3.0)
                                    (drover-test-session "i" :status 'idle))
    (drover-attention-mode 1)
    (let ((help (drover-attention-test-help drover-attention-mode-line)))
      (should (equal help (concat "Sessions that need you:\n"
                                  "Needs you (1):\n"
                                  "  ⚑ Session w  drover:main  permit: bash: make\n"
                                  "Done (2):\n"
                                  "  ✓ Session d\n"
                                  "  ✓ Session e\n"
                                  "\n"
                                  "mouse-1: visit the most urgent\n"
                                  "mouse-3: choose a session"))))))

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
    (should drover-attention--cycle)
    (drover-attention-mode -1)
    (should-not drover-attention-mode-line)
    (should-not drover-attention--cycle)
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
  "Return sessions needing the user in a known order: w, f, d1, d2."
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
    (should (equal (drover-attention-test-opened) '("asking" "alone" "asking")))))

(ert-deftest drover-attention-test-next-cycles-and-wraps ()
  (drover-attention-test-with (drover-attention-test-sessions)
    (drover-next)
    (dotimes (_ 4)
      (drover-attention-test-press))
    (should (equal (drover-attention-test-opened) '("w" "f" "d1" "d2" "w")))))

(ert-deftest drover-attention-test-next-keeps-first-order ()
  (drover-attention-test-with (drover-attention-test-sessions)
    (drover-next)
    ;; Visiting reads the session, and another one now waits longest.
    (setq drover-test-sessions
          (list (drover-test-session "w" :status 'idle)
                (drover-test-session "d2" :status 'waiting :since 1.0)
                (drover-test-session "d1" :status 'done :since 30.0)
                (drover-test-session "f" :status 'failed :since 60.0)))
    (drover-attention-test-press)
    (drover-attention-test-press)
    (drover-attention-test-press)
    (drover-attention-test-press)
    (should (equal (drover-attention-test-opened) '("w" "f" "d1" "d2" "w")))))

(ert-deftest drover-attention-test-next-skips-gone-sessions ()
  (drover-attention-test-with (drover-attention-test-sessions)
    (drover-next)
    (setq drover-test-sessions (seq-remove (lambda (session)
                                             (equal (drover-session-id session) "f"))
                                           drover-test-sessions))
    (drover-attention-test-press)
    (should (equal (drover-attention-test-opened) '("w" "d1")))))

(ert-deftest drover-attention-test-next-opens-fresh-records ()
  (drover-attention-test-with (drover-attention-test-sessions)
    (drover-next)
    (let ((opened nil))
      (setq drover-test-sessions (list (drover-test-session "w" :status 'waiting)
                                       (drover-attention-test-titled "f" "Renamed" :status 'idle)))
      (cl-letf (((symbol-function 'drover-open) (lambda (session &rest _) (setq opened session))))
        (drover-attention-test-press))
      (should (equal (drover-session-title opened) "Renamed")))))

(ert-deftest drover-attention-test-next-restarts-when-not-repeated ()
  (drover-attention-test-with (drover-attention-test-sessions)
    (drover-next)
    (drover-attention-test-press)
    (setq drover-test-sessions
          (list (drover-test-session "d1" :status 'done :since 30.0)
                (drover-test-session "f" :status 'failed :since 60.0)))
    (let ((last-command 'forward-char))
      (drover-next))
    (should (equal (drover-attention-test-opened) '("w" "f" "f")))))

(ert-deftest drover-attention-test-next-restarts-when-cycle-gone ()
  (drover-attention-test-with (list (drover-test-session "a" :status 'waiting))
    (drover-next)
    (setq drover-test-sessions (list (drover-test-session "b" :status 'done)))
    (drover-attention-test-press)
    (should (equal (drover-attention-test-opened) '("a" "b")))
    (should (equal drover-attention--cycle '((fake . "b"))))))

(ert-deftest drover-attention-test-next-single-session-wraps ()
  (drover-attention-test-with (list (drover-test-session "a" :status 'waiting))
    (drover-next)
    (drover-attention-test-press)
    (should (equal (drover-attention-test-opened) '("a" "a")))))

(ert-deftest drover-attention-test-next-nothing-to-do ()
  (drover-attention-test-with (list (drover-test-session "r" :status 'working)
                                    (drover-test-session "i" :status 'idle))
    (let ((messages (drover-attention-test-messages (drover-next))))
      (should (equal messages '("No session needs you")))
      (should-not drover-test-calls)
      (should-not drover-attention--cycle))
    (should (equal (drover-attention-test-messages (drover-attention-test-press))
                   '("No session needs you")))
    (should-not drover-test-calls)))

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
