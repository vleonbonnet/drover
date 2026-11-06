;;; drover-filter.el --- Searchable filter checklists  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Valentin Leon

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

;; A filter is edited in a temporary checklist.  All starts an exclusion
;; selection, None an inclusion selection: new values follow that choice.
;; Searching changes only which choices show, never which are selected.

;;; Code:

(require 'drover-core)
(require 'drover-ui)

(unless (and (boundp 'drover-core-generation) (>= drover-core-generation 21))
  (load "drover-core" nil t))

(defvar-local drover-filter--choices nil
  "Alist of display names and values offered by this checklist.")
(defvar-local drover-filter--selection nil
  "Draft selection of this checklist, independent of the active filter.")
(defvar-local drover-filter--query ""
  "Text narrowing the choices shown in this checklist.")
(defvar-local drover-filter--title ""
  "Name of the field this checklist filters.")
(defvar-local drover-filter--accepted nil
  "Whether the user applied this checklist's draft.")

(defvar drover-filter-mode-map
  (let ((map (make-sparse-keymap)))
    (dotimes (offset 94)
      (define-key map (string (+ 33 offset)) #'drover-filter--search))
    (define-key map [remap self-insert-command] #'drover-filter--search)
    (define-key map (kbd "SPC") #'drover-filter--toggle)
    (define-key map (kbd "C-n") #'next-line)
    (define-key map (kbd "C-p") #'previous-line)
    (define-key map (kbd "DEL") #'drover-filter--backspace)
    (define-key map (kbd "<backspace>") #'drover-filter--backspace)
    (define-key map (kbd "C-u") #'drover-filter--clear-search)
    (define-key map (kbd "M-SPC") #'drover-filter--search-space)
    (define-key map (kbd "M-a") #'drover-filter--all)
    (define-key map (kbd "M-n") #'drover-filter--none)
    (define-key map (kbd "M-i") #'drover-filter--invert)
    (define-key map (kbd "RET") #'drover-filter--apply)
    (define-key map (kbd "C-g") #'drover-filter--cancel)
    (define-key map [remap quit-window] #'drover-filter--cancel)
    map)
  "Keys for a searchable filter checklist.")

(define-derived-mode drover-filter-mode special-mode "Drover filter"
  "Edit a filter with a searchable checklist.
Type to search, then toggle choices and apply or cancel the draft.
\\{drover-filter-mode-map}"
  (setq-local truncate-lines t))

(defun drover-filter--draw ()
  "Draw this checklist, preserving the choice at point when visible."
  (let ((choice (get-text-property (line-beginning-position) 'drover-filter-choice))
        (line (line-number-at-pos))
        (inhibit-read-only t)
        (case-fold-search t)
        (selected 0)
        target)
    (erase-buffer)
    (dolist (entry drover-filter--choices)
      (let ((checked (drover-filter-selection-match-p (cdr entry) drover-filter--selection)))
        (when checked (cl-incf selected))
        (when (string-match-p (regexp-quote drover-filter--query) (car entry))
          (when (equal choice entry) (setq target (point)))
          (insert (propertize (concat (if checked "[x] " "[ ] ") (car entry) "\n")
                              'drover-filter-choice entry)))))
    (when (= (point-min) (point-max))
      (insert (drover-face "No matching choices\n" 'drover-dim)))
    (setq header-line-format
          (list (format "%s — %d/%d selected — Search: %s"
                        drover-filter--title selected (length drover-filter--choices)
                        drover-filter--query)))
    (setq mode-line-format
          '(" SPC toggle  M-a All  M-n None  M-i Invert  RET Apply  C-g Cancel  |  Type to search, C-u clear"))
    (goto-char (point-min))
    (if target (goto-char target) (forward-line (1- line)))
    (when (and (eobp) (> (point) (point-min))) (forward-line -1))
    (set-buffer-modified-p nil)))

(defun drover-filter--toggle ()
  "Toggle the choice at point, keeping the checklist open."
  (interactive)
  (when-let* ((entry (get-text-property (line-beginning-position) 'drover-filter-choice)))
    (let* ((value (cdr entry))
           (exclude (or (null drover-filter--selection)
                        (eq (car-safe drover-filter--selection) :exclude)))
           (values (drover-filter-selection-values drover-filter--selection)))
      (setq values (if (member value values) (remove value values) (append values (list value))))
      (setq drover-filter--selection
            (if exclude (and values (cons :exclude values)) (cons :include values))))
    (drover-filter--draw)))

(defun drover-filter--all ()
  "Select every value, including those encountered later."
  (interactive)
  (setq drover-filter--selection nil)
  (drover-filter--draw))

(defun drover-filter--none ()
  "Deselect every value, including those encountered later."
  (interactive)
  (setq drover-filter--selection '(:include))
  (drover-filter--draw))

(defun drover-filter--invert ()
  "Invert the whole selection, including choices hidden by the search."
  (interactive)
  (let ((values (drover-filter-selection-values drover-filter--selection)))
    (setq drover-filter--selection
          (if (or (null drover-filter--selection)
                  (eq (car-safe drover-filter--selection) :exclude))
              (cons :include values)
            (and values (cons :exclude values)))))
  (drover-filter--draw))

(defun drover-filter--search ()
  "Append the typed character to the checklist's search."
  (interactive)
  (when (characterp last-command-event)
    (setq drover-filter--query (drover-bound-line
                                (concat drover-filter--query (string last-command-event))))
    (goto-char (point-min))
    (drover-filter--draw)))

(defun drover-filter--search-space ()
  "Append a space to the checklist's search."
  (interactive)
  (let ((last-command-event ?\s)) (drover-filter--search)))

(defun drover-filter--backspace ()
  "Remove the last character of the checklist's search."
  (interactive)
  (unless (string-empty-p drover-filter--query)
    (setq drover-filter--query (substring drover-filter--query 0 -1))
    (drover-filter--draw)))

(defun drover-filter--clear-search ()
  "Show every choice without changing the selection."
  (interactive)
  (setq drover-filter--query "")
  (drover-filter--draw))

(defun drover-filter--apply ()
  "Apply the draft and return to the filter's menu."
  (interactive)
  (setq drover-filter--accepted t)
  (exit-recursive-edit))

(defun drover-filter--cancel ()
  "Discard the draft and return to the filter's menu."
  (interactive)
  (exit-recursive-edit))

;;;###autoload
(defun drover-filter-read (prompt names current)
  "Read a categorical filter with PROMPT from NAMES, starting at CURRENT.
NAMES is an alist (NAME . VALUE).  Return the draft when applied, or
CURRENT when cancelled.  The caller applies the returned value."
  (let ((buffer (generate-new-buffer " *drover filter*")))
    (unwind-protect
        (save-window-excursion
          (with-current-buffer buffer
            (drover-filter-mode)
            (setq drover-filter--title (drover-bound-line prompt)
                  drover-filter--choices
                  (mapcar (lambda (entry) (cons (drover-bound-line (car entry)) (cdr entry))) names)
                  drover-filter--selection current)
            (drover-filter--draw))
          (pop-to-buffer buffer '(display-buffer-same-window))
          (recursive-edit)
          (with-current-buffer buffer
            (if drover-filter--accepted drover-filter--selection current)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(provide 'drover-filter)
;;; drover-filter.el ends here
