;;; drover-look-test.el --- Tests of the look drover takes from opencode.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; Drover keeps no look of its own: its glyphs, faces, animations and
;; formats are opencode.el's, and its former names aliases of those.

;;; Code:

(require 'drover-test-helpers)
(require 'drover-list)

(defconst drover-look-test-options
  '((drover-status-glyphs . opencode-session-status-icons)
    (drover-animations . opencode-session-status-animations)
    (drover-list-animation-interval . opencode-session-status-animation-interval)
    (drover-subagent-glyphs . opencode-subagent-icons)
    (drover-archived-glyph . opencode-archived-icon)
    (drover-agent-faces . opencode-agent-faces)
    (drover-status-labels . opencode-session-status-labels))
  "Drover's former variables and the opencode.el ones they name now.")

(defconst drover-look-test-faces
  '((drover-waiting . opencode-session-status-waiting)
    (drover-failed . opencode-session-status-failed)
    (drover-done . opencode-session-status-done)
    (drover-retrying . opencode-session-status-retrying)
    (drover-working . opencode-session-status-working)
    (drover-idle . opencode-session-status-idle)
    (drover-open-session . opencode-session-open)
    (drover-shown-session . opencode-session-shown)
    (drover-active-session . opencode-session-active)
    (drover-cache-bar . opencode-cache-bar)
    (drover-cache-trough . opencode-cache-trough)
    (drover-context-bar . opencode-context-bar)
    (drover-context-near . opencode-context-near)
    (drover-context-over . opencode-context-over)
    (drover-prompt-picked . opencode-setting-chosen)
    (drover-prompt-inherited . opencode-setting-inherited))
  "Drover's former faces and the opencode.el ones they name now.")

(ert-deftest drover-look-test-former-names ()
  "Drover's former options, faces and functions are obsolete names of opencode.el's."
  (pcase-dolist (`(,old . ,new) drover-look-test-options)
    (should (eq (indirect-variable old) new))
    (should (get old 'byte-obsolete-variable)))
  (pcase-dolist (`(,old . ,new) drover-look-test-faces)
    (should (facep new))
    (should (eq (get old 'face-alias) new))
    (should (get old 'obsolete-face)))
  (pcase-dolist (`(,old . ,new) '((drover-status-face . opencode-session-status-face)
                                  (drover-status-label . opencode-session-status-label)
                                  (drover-status-glyph . opencode-session-status-glyph)
                                  (drover-format-duration . opencode-format-duration)
                                  (drover-format-age . opencode-format-age)
                                  (drover-format-tokens . opencode-format-tokens)))
    (should (eq (indirect-function old) (indirect-function new)))
    (should (get old 'byte-obsolete-info)))
  ;; A customization under a former name reaches what every display draws.
  (let ((drover-status-glyphs '((waiting . "W"))))
    (should (equal (substring-no-properties (opencode-session-status-glyph 'waiting)) "W")))
  (should (equal drover-status-order opencode-session-statuses)))

(defvar drover-look-test-old nil
  "A former option, of `drover-look-test-adopt'.")

(defvar drover-look-test-new nil
  "What it names now, of `drover-look-test-adopt'.")

(ert-deftest drover-look-test-adopt ()
  "A value the user gave a former option before it became an alias goes to the new one."
  (cl-flet ((adopt (old-value standard new-value)
              (let ((drover-look-test-new new-value))
                (unwind-protect
                    (progn
                      (makunbound 'drover-look-test-old)
                      (put 'drover-look-test-old 'standard-value (and standard (list (list 'quote standard))))
                      (set-default 'drover-look-test-old old-value)
                      (drover--adopt-option 'drover-look-test-old 'drover-look-test-new)
                      (should-not (boundp 'drover-look-test-old))
                      drover-look-test-new)
                  (put 'drover-look-test-old 'standard-value nil)
                  (set-default 'drover-look-test-old nil)))))
    ;; Changed by the user: adopted.
    (should (equal (adopt '(mine) '(standard) '(theirs)) '(mine)))
    ;; Its standard value, as an older drover left it: not.
    (should (equal (adopt '(standard) '(standard) '(theirs)) '(theirs)))
    ;; Set before any definition, as a configuration may: adopted.
    (should (equal (adopt '(mine) nil '(theirs)) '(mine))))
  ;; An alias already is left alone.
  (let ((opencode-archived-icon "A"))
    (drover--adopt-option 'drover-archived-glyph 'opencode-archived-icon)
    (should (eq (indirect-variable 'drover-archived-glyph) 'opencode-archived-icon))
    (should (equal opencode-archived-icon "A"))))

(ert-deftest drover-look-test-agent-faces ()
  "Agents of every backend draw from opencode.el's faces, by their name."
  (let ((opencode-agent-faces '(bold italic)))
    (should (memq (drover-agent-face "build") '(bold italic)))
    (should (eq (drover-agent-face "build") (drover-agent-face "build"))))
  (let ((opencode-agent-faces nil))
    (should-not (drover-agent-face "build"))))

(provide 'drover-look-test)
;;; drover-look-test.el ends here
