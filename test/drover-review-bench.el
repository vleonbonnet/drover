;;; drover-review-bench.el --- Timing the review of large diffs  -*- lexical-binding: t; -*-

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

;; `make bench-review' opens the reviews of a fake session's changes,
;; 5000 lines changed in one file, then across 100 files, then 50,000
;; across 200, each with comments to place, and reports what the command
;; costs, the longest slice of drawing, the longest Emacs went without
;; running timers, and how long the whole drawing took.  Not part of
;; `make test'.

;;; Code:

(require 'drover-test-helpers)
(require 'drover-review)

(defun drover-review-bench--files (files lines)
  "Return FILES files of LINES lines changed each, as `drover-review-fetch' tells them."
  (cl-loop for index from 1 to files
           collect (list :file (format "src/file-%03d.el" index) :status 'modified
                         :additions lines :deletions (/ lines 4)
                         :patch (with-output-to-string
                                  (cl-loop for hunk from 0 below (max 1 (/ lines 40))
                                           for start = (1+ (* hunk 60))
                                           do (princ (format "@@ -%d,13 +%d,43 @@ (defun hunk-%d ()\n"
                                                             start start hunk))
                                           (dotimes (_ 3) (princ " (let ((context t))\n"))
                                           (dotimes (i 10) (princ (format "-  (old-call %d)\n" i)))
                                           (dotimes (i 40) (princ (format "+  (new-call %d \"%s\")\n"
                                                                          i (make-string 30 ?x)))))))))

(defun drover-review-bench--run (label files)
  "Open the review of FILES, report it under LABEL, and close it."
  (drover-test-with-state
    (drover-test-with-engines
      (let* ((drover-review--store nil)
             (drover-review--store-file nil)
             (session (drover-test-session "big" :engine 'fake :engine-id "big"
                                           :directory temporary-file-directory))
             (key (drover-review-key session))
             (slices nil)
             (draw (symbol-function 'drover-review--draw-some))
             (changed (apply #'+ (mapcar (lambda (file) (+ (plist-get file :additions)
                                                            (plist-get file :deletions)))
                                         files))))
        (drover-test-register-engine 'fake :changes (lambda (_id _scope _directory callback)
                                                      (funcall callback (list :files files))))
        ;; Comments on lines of every tenth file, to place as it draws.
        (cl-loop for file in files by (lambda (list) (nthcdr 10 list))
                 do (drover-review-add-comment key (list :file (plist-get file :file) :side 'new
                                                         :start 20 :end 20 :text "Why?"
                                                         :lines (list (format "+  (new-call 16 \"%s\")"
                                                                              (make-string 30 ?x)))
                                                         :hunk "@@")))
        (drover-test-with-backend (list session)
          (cl-letf (((symbol-function 'drover-review--draw-some)
                     (lambda ()
                       (let ((start (float-time)))
                         (funcall draw)
                         (push (- (float-time) start) slices)))))
            (save-window-excursion
              (garbage-collect)
              (let* ((start (float-time))
                     (command nil)
                     (buffer nil)
                     (lag (drover-test-lag
                           (lambda ()
                             (setq buffer (drover-review session)
                                   command (- (float-time) start))
                             (while (buffer-local-value 'drover-review--drawing buffer)
                               (accept-process-output nil 0.001)))))
                     (total (- (float-time) start)))
                (message "%-32s %6d lines changed, %3d files: command %5.1f ms, longest slice %5.1f ms, %3d slices, longest without timers %5.1f ms, drawn in %6.1f ms, %d comments shown"
                         label changed (length files) (* 1000 command) (* 1000 (apply #'max slices))
                         (length slices) (* 1000 lag) (* 1000 total)
                         (hash-table-count (buffer-local-value 'drover-review--placed buffer)))
                (kill-buffer buffer)))))))))

(defun drover-review-bench-main ()
  "Time the reviews of large diffs, and report."
  (drover-review-bench--run "5000 lines in one file" (drover-review-bench--files 1 4000))
  (drover-review-bench--run "5000 lines in 100 files" (drover-review-bench--files 100 40))
  (drover-review-bench--run "50,000 lines in 200 files" (drover-review-bench--files 200 200)))

(provide 'drover-review-bench)
;;; drover-review-bench.el ends here
