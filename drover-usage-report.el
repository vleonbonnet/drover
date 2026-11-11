;;; drover-usage-report.el --- The report of what agents cost  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; Author: Valentin Leon <valentin@leon.click>
;; Keywords: tools

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

;; `drover-usage', $ in `drover-dispatch', shows in `*drover usage*'
;; the totals of `drover-usage-report': tokens and cost, of today, this
;; week, this month or ever, grouped by session, project, harness,
;; provider, model, host or day, each group broken down, in Magit's
;; sections; then where each cost comes from.  Its menu,
;; `drover-usage-menu', V there, sets what it totals on the families of
;; keys of the list's menu: = for options.  `drover-usage-mode-line-mode'
;; shows today's cost in the mode line.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'magit-section)
(require 'transient)
(require 'drover-core)
(require 'drover-ui)
(require 'drover-keys)
(require 'drover-usage)

;;;; The report

(defcustom drover-usage-buffer-name "*drover usage*"
  "The name of the buffer of `drover-usage'."
  :type 'string
  :group 'drover-usage)

(defvar-keymap drover-usage-mode-map
  :doc "Keymap of `drover-usage-mode'."
  :parent magit-section-mode-map
  "g" #'drover-usage-refresh
  "V" #'drover-usage-menu
  "RET" #'drover-usage-visit
  "q" #'quit-window)

(define-derived-mode drover-usage-mode magit-section-mode "Drover usage"
  "Mode of the report of what agents cost, `drover-usage'.
\\<drover-usage-mode-map>\\[drover-usage-menu] chooses the period, the grouping and what
is totalled; \\[drover-usage-refresh] reads anew what changed.

\\{drover-usage-mode-map}"
  :interactive nil
  (setq-local revert-buffer-function (lambda (&rest _) (drover-usage-refresh)))
  (setq-local truncate-lines t)
  (add-hook 'drover-usage-changed-hook #'drover-usage--redraw-soon))

(defclass drover-usage-section (magit-section) ()
  "A section of the report of usage.")

(defvar-local drover-usage--redraw-timer nil
  "Timer drawing the report again.")

(defconst drover-usage--columns '(("Cost" . 14) ("Input" . 8) ("Output" . 8) ("Cache read" . 11)
                                  ("Cache write" . 12))
  "The columns of the report's numbers, as (TITLE . WIDTH).")

(defun drover-usage--label-width ()
  "Return the characters a label of the report takes, from its window's width."
  (let ((window (get-buffer-window (current-buffer) t)))
    (max 20 (- (if window (window-max-chars-per-line window) 100)
               (apply #'+ (mapcar #'cdr drover-usage--columns)) 6))))

(defun drover-usage--line (indent label sum &optional face)
  "Return the line of LABEL, INDENT characters in, and the numbers of SUM.
LABEL is cut to its room; FACE, if any, is its face."
  (let* ((width (- (drover-usage--label-width) indent))
         (label (truncate-string-to-width (drover-bound-line (or label "") width) width nil ?\s "…")))
    (concat (make-string indent ?\s)
            (if face (drover-face label face) label)
            (format "%14s%8s%8s%11s%12s"
                    (drover-usage-format-short sum)
                    (drover-usage-format-tokens (drover-usage-sum-input sum))
                    (drover-usage-format-tokens (drover-usage-sum-output sum))
                    (drover-usage-format-tokens (drover-usage-sum-read sum))
                    (drover-usage-format-tokens (drover-usage-sum-write sum)))
            "\n")))

(defun drover-usage--title (period grouping)
  "Return the title of the report of PERIOD by GROUPING."
  (let ((start (drover-usage-period-start period)))
    (concat "Usage " (alist-get period drover-usage-period-labels)
            (and start (not (eq period 'today))
                 (format-time-string " (from %a %e %b)" (drover-usage--day-time start)))
            ", by " (alist-get grouping drover-usage-grouping-labels)
            (and drover-usage-filter
                 (concat ", of "
                         (mapconcat (lambda (key) (format "%s" (plist-get drover-usage-filter key)))
                                    (seq-filter (lambda (key) (plist-get drover-usage-filter key))
                                                '(:project :harness :host :model :provider))
                                    ", "))))))

(defun drover-usage--insert ()
  "Insert the report of usage, as the settings tell, in the current buffer."
  (let* ((report (drover-usage-report drover-usage-period drover-usage-group-by drover-usage-breakdown))
         (total (plist-get report :total))
         (hidden (make-hash-table :test #'equal)))
    (when magit-root-section
      (dolist (section (oref magit-root-section children))
        (when (eq (oref section type) 'drover-usage-group)
          (puthash (oref section value) (oref section hidden) hidden))))
    (magit-insert-section (drover-usage-root)
      (magit-insert-heading (drover-face (drover-usage--title drover-usage-period drover-usage-group-by)
                                         'drover-heading))
      (insert (drover-face (concat (make-string (drover-usage--label-width) ?\s)
                                   (format "%14s%8s%8s%11s%12s" "Cost" "Input" "Output" "Cache read" "Cache write")
                                   "\n")
                           'drover-dim))
      (insert (drover-usage--line 0 "Total" total 'bold))
      (when (and (drover-usage-reading-p) drover-usage--started)
        (insert (drover-face "Reading what the engines recorded…\n" 'drover-dim)))
      (insert "\n")
      (if (null (plist-get report :groups))
          (insert (drover-face "No usage.\n" 'drover-dim))
        (let ((many (> (length (plist-get report :groups)) 8)))
          (pcase-dolist (`(,value ,label ,sum ,items) (plist-get report :groups))
            (magit-insert-section (drover-usage-group value (gethash value hidden many))
              (magit-insert-heading (drover-usage--line 0 label sum))
              (pcase-dolist (`(,item ,item-label ,item-sum) items)
                (magit-insert-section (drover-usage-item (cons value item))
                  (insert (drover-usage--line 2 item-label item-sum 'drover-dim))))))))
      (insert "\n")
      (drover-usage--insert-sources report))))

(defun drover-usage--insert-sources (report)
  "Insert where the numbers of REPORT come from: sources, prices, hosts."
  (magit-insert-section (drover-usage-sources nil t)
    (magit-insert-heading (drover-face "Sources" 'drover-heading))
    (let ((oc (drover-usage--oc-current)))
      (insert (format "  OpenCode: %s\n"
                      (cond ((null oc) "not read yet")
                            ((drover-usage--oc-failure oc) (drover-usage--oc-failure oc))
                            ((consp (drover-usage--oc-back oc)) "its service's database, history still read")
                            (t "costs its service recorded, from its database")))))
    (insert (format "  Claude Code: %d transcripts, its subscription %s\n"
                    (if-let* ((index (drover-usage--claude-current)))
                        (hash-table-count (drover-transcript-index-entries index))
                      0)
                    (if (eq (drover-usage-billing 'claude-code nil) 'included) "including usage" "not used")))
    (insert (format "  Codex: %d rollouts, %s\n"
                    (if-let* ((index (drover-usage--codex-current)))
                        (hash-table-count (drover-transcript-index-entries index))
                      0)
                    (if (eq (drover-usage-billing 'codex nil) 'included)
                        "logged in through ChatGPT, included" "priced per token")))
    (pcase-dolist (`(,host . ,entry) (plist-get report :hosts))
      (insert (format "  %s: %s\n" (drover-host-label host)
                      (cond ((plist-get entry :error) (concat "failed: " (plist-get entry :error)))
                            ((not (plist-get entry :time)) "asked…")
                            (t (concat (if (plist-get entry :away) "not connected, as of " "as of ")
                                       (format-time-string "%H:%M" (plist-get entry :time))
                                       (and (plist-get entry :reading) ", still reading")
                                       (and (plist-get entry :more)
                                            (> (plist-get entry :more) 0)
                                            (format ", %d rows left out" (plist-get entry :more)))))))))
    (insert (drover-face "  Costs, by where they come from\n" 'drover-dim))
    (pcase-dolist (`(,host ,harness ,provider ,model ,source ,sum) (plist-get report :sources))
      ;; The cost first: the line is long, and cut on the right.
      (insert (format "    %14s  " (drover-usage-format-short sum))
              (drover-face (drover-bound-line
                            (format "%s%s · %s · %s: %s"
                                    (if host (concat (drover-host-label host) ": ") "")
                                    (drover-usage--engine-label harness)
                                    (or provider "unknown") (or model "unknown") source))
                           'drover-dim)
              "\n"))))

(defun drover-usage--draw ()
  "Draw the report of usage in the current buffer anew, keeping point."
  (let ((inhibit-read-only t)
        (line (line-number-at-pos))
        (column (current-column)))
    (erase-buffer)
    (drover-usage--insert)
    (goto-char (point-min))
    (forward-line (1- line))
    (move-to-column column)))

(defun drover-usage--redraw-soon ()
  "Draw the reports of usage shown again soon.
On `drover-usage-changed-hook'."
  (dolist (buffer (buffer-list))
    (when (eq (buffer-local-value 'major-mode buffer) 'drover-usage-mode)
      (with-current-buffer buffer
        (unless (timerp drover-usage--redraw-timer)
          (setq drover-usage--redraw-timer
                (run-at-time 0.5 nil #'drover-usage--redraw buffer)))))))

(defun drover-usage--redraw (buffer)
  "Draw the report of usage in BUFFER again, if shown; from a timer."
  (drover-with-reporting "usage report"
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq drover-usage--redraw-timer nil)
        (when (get-buffer-window buffer t)
          (drover-usage--draw))))))

;;;###autoload
(defun drover-usage ()
  "Show what agents cost, in tokens and money, today, this week, or ever.
Grouped by session, project, harness, provider, model, host or day, as
the menu of the report chooses, \\<drover-usage-mode-map>\\[drover-usage-menu] there."
  (interactive)
  (drover-usage-start)
  (let ((buffer (get-buffer-create drover-usage-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'drover-usage-mode)
        (drover-usage-mode))
      (drover-usage--draw))
    (pop-to-buffer buffer)))

(defun drover-usage-refresh ()
  "Read anew what changed of the usage, and draw the report again.
The files are looked at at once and OpenCode's database read; peers
are asked again."
  (interactive)
  (drover-usage-start)
  (dolist (index (list (drover-usage--claude-current) (drover-usage--codex-current)))
    (when index (drover-transcript-look-soon index)))
  (drover-usage--oc-look)
  (maphash (lambda (key entry)
             (unless (plist-get entry :pending)
               (puthash key (plist-put (copy-sequence entry) :time 0) drover-usage--remote)))
           drover-usage--remote)
  (when (derived-mode-p 'drover-usage-mode)
    (drover-usage--draw)))

(defun drover-usage-visit ()
  "Open the session at point, in a report grouped by session."
  (interactive)
  (let* ((section (magit-current-section))
         (value (and section (oref section value)))
         (value (if (eq (oref section type) 'drover-usage-item) (car value) value)))
    (pcase value
      (`(nil ,engine ,id)
       (if-let* ((session (drover-find-engine-session engine id)))
           (drover-open session)
         (user-error "No such session listed")))
      (_ (magit-section-toggle section)))))

(defun drover-usage--settings-changed ()
  "Draw the reports again after a setting changed."
  (dolist (buffer (buffer-list))
    (when (eq (buffer-local-value 'major-mode buffer) 'drover-usage-mode)
      (with-current-buffer buffer
        (drover-usage--draw)))))

;;;; The menu

(defun drover-usage--choose (prompt labels current)
  "Read one of LABELS, an alist (VALUE . LABEL), with PROMPT.
CURRENT is the default."
  (let ((choice (completing-read (format "%s (%s): " prompt (alist-get current labels))
                                 (mapcar #'cdr labels) nil t nil nil (alist-get current labels))))
    (car (rassoc choice labels))))

(defun drover-usage--set (variable)
  "Return a function setting VARIABLE and drawing the reports again."
  (lambda (value)
    (set variable value)
    (drover-usage--settings-changed)))

(transient-define-infix drover-usage--period ()
  "Total the usage of today, this week, this month or ever."
  :class 'drover--setting
  :description "period"
  :getter (lambda () drover-usage-period)
  :setter (drover-usage--set 'drover-usage-period)
  :chooser (lambda (current)
             (let ((periods (mapcar #'car drover-usage-period-labels)))
               (or (cadr (memq current periods)) (car periods))))
  :options drover-usage-period-labels)

(transient-define-infix drover-usage--group-by ()
  "Group the usage."
  :class 'drover--setting
  :description "group"
  :getter (lambda () drover-usage-group-by)
  :setter (drover-usage--set 'drover-usage-group-by)
  :chooser (lambda (current) (drover-usage--choose "Group by" drover-usage-grouping-labels current))
  :labeler (lambda (grouping) (alist-get grouping drover-usage-grouping-labels)))

(transient-define-infix drover-usage--breakdown ()
  "Break each group of the usage down."
  :class 'drover--setting
  :description "within"
  :getter (lambda () drover-usage-breakdown)
  :setter (drover-usage--set 'drover-usage-breakdown)
  :chooser (lambda (current)
             (drover-usage--choose "Break down by" (cons '(nil . "nothing") drover-usage-grouping-labels)
                                   current))
  :labeler (lambda (grouping) (alist-get grouping drover-usage-grouping-labels)))

(transient-define-infix drover-usage--sort-by ()
  "Order the groups of the usage."
  :class 'drover--setting
  :description "sort"
  :getter (lambda () drover-usage-sort-by)
  :setter (drover-usage--set 'drover-usage-sort-by)
  :chooser (lambda (current)
             (let ((orders '(cost tokens name)))
               (or (cadr (memq current orders)) (car orders))))
  :options '((cost . "cost") (tokens . "tokens") (name . "name")))

(defun drover-usage--filter-values (grouping)
  "Return the values of GROUPING the usage of this period shows.
As (VALUE . LABEL)."
  (let ((drover-usage-filter nil))
    (mapcar (lambda (group) (cons (car group) (cadr group)))
            (plist-get (drover-usage-report drover-usage-period grouping nil) :groups))))

(defmacro drover-usage--define-filter (name key grouping)
  "Define the infix NAME filtering the usage by GROUPING, as KEY of the filter."
  `(transient-define-infix ,name ()
     ,(format "Total the usage of one %s." grouping)
     :class 'drover--setting
     :description ,(symbol-name grouping)
     :getter (lambda () (plist-get drover-usage-filter ,key))
     :setter (lambda (value)
               (setq drover-usage-filter (plist-put (copy-sequence drover-usage-filter) ,key value))
               (drover-usage--settings-changed))
     :chooser (lambda (current)
                (if current
                    nil
                  (let ((values (drover-usage--filter-values ',grouping)))
                    (car (rassoc (completing-read ,(format "Usage of %s: " grouping) (mapcar #'cdr values) nil t)
                                 values)))))
     :labeler (lambda (value) (and value (format "%s" value)))))

(drover-usage--define-filter drover-usage--filter-project :project project)
(drover-usage--define-filter drover-usage--filter-harness :harness harness)
(drover-usage--define-filter drover-usage--filter-host :host host)
(drover-usage--define-filter drover-usage--filter-model :model model)
(drover-usage--define-filter drover-usage--filter-provider :provider provider)

(defun drover-usage-filter-clear ()
  "Total the usage of every session."
  (interactive)
  (setq drover-usage-filter nil)
  (drover-usage--settings-changed))

;;;###autoload (autoload 'drover-usage-menu "drover-usage" nil t)
(transient-define-prefix drover-usage-menu ()
  "Choose what the report of usage totals, and how."
  [["Usage"
    ("=d" drover-usage--period)
    ("=g" drover-usage--group-by)
    ("=b" drover-usage--breakdown)
    ("=o" drover-usage--sort-by)]
   ["Filter"
    ("=p" drover-usage--filter-project)
    ("=H" drover-usage--filter-harness)
    ("=h" drover-usage--filter-host)
    ("=m" drover-usage--filter-model)
    ("=v" drover-usage--filter-provider)
    ("=c" "clear" drover-usage-filter-clear :transient t)]
   ["Report"
    ("g" "Refresh" drover-usage-refresh :transient t)
    ("V" "Close" transient-quit-one)]]
  drover--menu-close)

;;;; The mode line

(defvar drover-usage--mode-line ""
  "What the mode line shows of today's usage.")

(defvar drover-usage--mode-line-help nil
  "The tooltip of `drover-usage--mode-line', made as shown.")

(defun drover-usage--mode-line-update ()
  "Compute what the mode line shows of today's usage again."
  (when (bound-and-true-p drover-usage-mode-line-mode)
    (let* ((report (drover-usage-report 'today 'harness nil))
           (total (plist-get report :total))
           (text (if (zerop (drover-usage-sum-tokens total)) "" (drover-usage-format-short total))))
      (setq drover-usage--mode-line-help
            (drover-bound-lines
             (string-join (cons (concat "Today: " (drover-usage-format-sum total))
                                (mapcar (pcase-lambda (`(,_value ,label ,sum ,_items))
                                          (concat label ": " (drover-usage-format-sum sum)))
                                        (plist-get report :groups)))
                          "\n")))
      (unless (equal text (substring-no-properties drover-usage--mode-line))
        (setq drover-usage--mode-line
              (if (string-empty-p text)
                  ""
                (propertize text 'help-echo (lambda (&rest _) drover-usage--mode-line-help)
                            'mouse-face 'mode-line-highlight
                            'local-map (make-mode-line-mouse-map 'mouse-1 #'drover-usage))))
        (force-mode-line-update t)))))

(defun drover-usage--mode-line-follow ()
  "Follow the usage changed, for the mode line; on `drover-usage-changed-hook'."
  (drover-usage--mode-line-update))

;;;###autoload
(define-minor-mode drover-usage-mode-line-mode
  "Show in the mode line what agents cost today; a click shows the report."
  :global t
  :group 'drover-usage
  (if drover-usage-mode-line-mode
      (progn
        (drover-usage-start)
        (add-to-list 'global-mode-string '(:eval drover-usage--mode-line) t)
        (add-hook 'drover-usage-changed-hook #'drover-usage--mode-line-follow)
        (drover-usage--mode-line-update))
    (setq global-mode-string (delete '(:eval drover-usage--mode-line) global-mode-string))
    (remove-hook 'drover-usage-changed-hook #'drover-usage--mode-line-follow)))

(provide 'drover-usage-report)
;;; drover-usage-report.el ends here
