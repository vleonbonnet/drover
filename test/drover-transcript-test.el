;;; drover-transcript-test.el --- Tests of the index of transcripts  -*- lexical-binding: t; -*-

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

;; The index of `drover-transcript' over files of a temporary directory,
;; read by a reader of the test's, which records what it was given.

;;; Code:

(require 'drover-test-helpers)
(require 'drover-transcript)

(defmacro drover-transcript-test-with (&rest body)
  "Run BODY with `index', an index of the .jsonl files under `root'.
The files are those of `root''s subdirectories; the reader returns the
file's size as :size and the size it read from before as :from, and
`changes' collects what the index reported, newest first."
  (declare (indent 0) (debug t))
  `(drover-test-with-state
     (let* ((root (file-name-as-directory (make-temp-file "drover-transcripts" t)))
            (changes nil)
            (index (drover-transcript-index-create
                    'test-transcripts
                    (lambda () (list (list root 1 "\\.jsonl\\'")))
                    (lambda (_file attributes old)
                      (list :size (file-attribute-size attributes)
                            :from (and old (drover-transcript-size old))))
                    (lambda (file old new) (push (list (file-name-nondirectory file) old new) changes)))))
       (unwind-protect
           (progn ,@body)
         (drover-transcript-stop index)
         (delete-directory root t)))))

(ert-deftest drover-transcript-test-changes ()
  "Files coming, changing and going are read and reported; others are not."
  (drover-transcript-test-with
    (let ((a (expand-file-name "p/a.jsonl" root))
          (b (expand-file-name "p/b.jsonl" root)))
      (drover-test-write a "one\n")
      (drover-test-write b "two\n")
      (drover-test-write (expand-file-name "p/notes.txt" root) "skipped\n")
      (drover-transcript-scan index)
      (should (equal (sort (mapcar #'car changes) #'string<) '("a.jsonl" "b.jsonl")))
      (setq changes nil)
      (drover-transcript-scan index)
      (should-not changes)
      (drover-test-write a "one\nmore\n")
      (delete-file b)
      (drover-transcript-scan index)
      (should (equal (sort changes (lambda (x y) (string< (car x) (car y))))
                     '(("a.jsonl" (:size 4 :from nil) (:size 9 :from 4))
                       ("b.jsonl" (:size 4 :from nil) nil))))
      (should (equal (drover-transcript-lookup index a) '(:size 9 :from 4))))))

(ert-deftest drover-transcript-test-unlisted-directory-kept ()
  "Entries of a directory that could not be listed stay, its files maybe there."
  (drover-transcript-test-with
    (let ((a (expand-file-name "p/a.jsonl" root)))
      (drover-test-write a "one\n")
      (drover-transcript-scan index)
      (setq changes nil)
      (cl-letf* ((list (symbol-function 'directory-files-and-attributes))
                 ((symbol-function 'directory-files-and-attributes)
                  (lambda (directory &rest arguments)
                    (if (string-suffix-p "/p" directory)
                        (signal 'file-error (list "Opening directory" "Permission denied" directory))
                      (apply list directory arguments)))))
        (drover-transcript-scan index))
      (should-not changes)
      (should (drover-transcript-lookup index a)))))

(ert-deftest drover-transcript-test-files-by-key ()
  "Files are found by what their data names them, those kept between Emacs sessions too.
A file kept is found by its key though its backend is not told of it,
nothing having changed since it was read."
  (drover-test-with-state
    (let* ((root (file-name-as-directory (make-temp-file "drover-transcripts" t)))
           (make (lambda ()
                   (drover-transcript-index-create
                    'test-transcripts
                    (lambda () (list (list root 1 "\\.jsonl\\'")))
                    (lambda (file _attributes _old)
                      (with-temp-buffer
                        (insert-file-contents file)
                        (list :id (buffer-substring (point-min) (line-end-position 1)))))
                    #'ignore nil #'drover-transcript-id)))
           (index (funcall make))
           (a (expand-file-name "p/a.jsonl" root))
           (b (expand-file-name "p/b.jsonl" root))
           (moved (expand-file-name "q/b.jsonl" root)))
      (unwind-protect
          (progn
            (drover-test-write a "one\n")
            (drover-test-write b "two\n")
            (drover-transcript-scan index)
            (should (equal (drover-transcript-keyed-file index "one") a))
            (should (equal (drover-transcript-keyed-file index "two") b))
            ;; A file read as another conversation is no longer the first's.
            (drover-test-write a "three\nmore\n")
            (drover-transcript-scan index)
            (should-not (drover-transcript-keyed-file index "one"))
            (should (equal (drover-transcript-keyed-file index "three") a))
            ;; A conversation moving to another file is that file's.
            (rename-file b (progn (make-directory (file-name-directory moved) t) moved))
            (drover-transcript-scan index)
            (should (equal (drover-transcript-keyed-file index "two") moved))
            (drover-transcript-stop index)
            ;; A new Emacs finds them by what it kept, before any look.
            (let ((kept (funcall make)))
              (drover-transcript-load kept)
              (should (equal (drover-transcript-keyed-file kept "three") a))
              (should (equal (drover-transcript-keyed-file kept "two") moved))
              (should-not (drover-transcript-keyed-file kept "one"))
              (drover-transcript-scan kept)
              (should (equal (drover-transcript-keyed-file kept "two") moved))
              (delete-file a)
              (drover-transcript-scan kept)
              (should-not (drover-transcript-keyed-file kept "three"))))
        (drover-transcript-stop index)
        (delete-directory root t)))))

(ert-deftest drover-transcript-test-slices ()
  "The index works in slices, from timers, each giving way after its time."
  (drover-transcript-test-with
    (dotimes (index 30)
      (drover-test-write (expand-file-name (format "p%d/f.jsonl" index) root) "x\n"))
    (let ((timers nil)
          (drover-transcript-slice 0)
          (steps 0))
      (cl-letf* ((step (symbol-function 'drover-transcript--step))
                 ((symbol-function 'drover-transcript--step)
                  (lambda (index) (cl-incf steps) (funcall step index)))
                 ((symbol-function 'run-at-time)
                  (lambda (delay _repeat function &rest arguments)
                    (let ((timer (timer-create)))
                      (timer-set-function timer function arguments)
                      (push (cons delay timer) timers)
                      timer))))
        (drover-transcript-start index)
        (should (equal (mapcar #'car timers) '(0)))
        ;; Each slice takes one step when its time is up at once.
        (let ((slices 0))
          (while (and timers (< slices 200))
            (let ((timer (cdr (pop timers))))
              (setq steps 0)
              (apply (timer--function timer) (timer--args timer))
              (cl-incf slices)
              (should (<= steps 1))
              ;; The look over, the next one waits its interval.
              (when (and timers (= (caar timers) drover-transcript-scan-interval))
                (setq timers nil))))
          (should (> slices 30)))
        (should (= (length changes) 30))))))

(ert-deftest drover-transcript-test-quick-looks ()
  "Most looks check only the files changed lately, and the directories' names."
  (drover-transcript-test-with
    (let ((old (expand-file-name "p/old.jsonl" root))
          (recent (expand-file-name "p/recent.jsonl" root))
          (now (float-time)))
      (drover-test-write old "old\n")
      (drover-test-write recent "recent\n")
      (set-file-times old (seconds-to-time (- now (* 3 86400))))
      ;; The directory's own time, as files come and go.
      (set-file-times (expand-file-name "p" root) (seconds-to-time (- now 60)))
      (drover-transcript-scan index)
      (setq changes nil)
      (drover-test-write old "old\nmore\n")
      (set-file-times old (seconds-to-time (- now (* 3 86400))))
      (drover-test-write recent "recent\nmore\n")
      (set-file-times (expand-file-name "p" root) (seconds-to-time (- now 60)))
      (drover-transcript-scan index t)
      ;; The recent one is read again, the old one not yet.
      (should (equal (mapcar #'car changes) '("recent.jsonl")))
      ;; A file coming changes its directory's time: it is found.
      (drover-test-write (expand-file-name "p/new.jsonl" root) "new\n")
      (drover-transcript-scan index t)
      (should (equal (mapcar #'car changes) '("new.jsonl" "recent.jsonl")))
      ;; One going too, old or not.
      (delete-file old)
      (drover-transcript-scan index t)
      (should (equal (car changes) '("old.jsonl" (:size 4 :from nil) nil)))
      ;; A full look checks every file, old ones too.
      (let ((aged (expand-file-name "p/aged.jsonl" root)))
        (drover-test-write aged "aged\n")
        (set-file-times aged (seconds-to-time (- now (* 3 86400))))
        (drover-transcript-scan index)
        (setq changes nil)
        (drover-test-write aged "aged\nmore\n")
        (set-file-times aged (seconds-to-time (- now (* 3 86400))))
        (set-file-times (expand-file-name "p" root) (seconds-to-time (- now 30)))
        (drover-transcript-scan index)
        (setq changes nil)
        (drover-test-write aged "aged\nmore\nagain\n")
        (set-file-times aged (seconds-to-time (- now (* 3 86400))))
        (set-file-times (expand-file-name "p" root) (seconds-to-time (- now 30)))
        (drover-transcript-scan index t)
        (should-not changes)
        (drover-transcript-scan index)
        (should (equal (mapcar #'car changes) '("aged.jsonl")))))))

(ert-deftest drover-transcript-test-reading ()
  "Readers read the ends of a file, values found without parsing their lines."
  (let ((file (make-temp-file "drover-transcript" nil ".jsonl"))
        (drover-transcript-window 64))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "{\"type\":\"meta\",\"cwd\":\"/w\\\"ork/\",\"blob\":\"" (make-string 200 ?x) "\"}\n"
                    "{\"type\":\"title\",\"title\":\"Last é\"}\n"))
          (let ((size (file-attribute-size (file-attributes file))))
            (with-temp-buffer
              (let ((tail (drover-transcript-insert-ends file size)))
                (should (< (- (point-max) (point-min)) 140))
                ;; The first line, cut, still tells its directory.
                (goto-char (point-min))
                (should (equal (drover-transcript-sniff "cwd" nil tail) "/w\"ork/"))
                (should-not (drover-transcript-lines "\"type\":\"meta\"" nil (point-min) tail))
                (should (equal (alist-get 'title (drover-transcript-last "\"type\":\"title\"" nil tail))
                               "Last é"))))
            ;; What was appended since a size, at most a window of it.
            (with-temp-buffer
              (should (= (drover-transcript-insert-ends file size (- size 10)) (point-min)))
              (should (= (string-bytes (buffer-string)) 10)))))
      (delete-file file))))

(ert-deftest drover-transcript-test-text-and-time ()
  "Titles are their first line, cut; times are the CLIs' ISO 8601 ones."
  (should (equal (drover-transcript-text "\n  Fix the  build \nand test") "Fix the  build"))
  (should-not (drover-transcript-text " \n "))
  (let ((drover-transcript-text-limit 5))
    (should (equal (drover-transcript-text "abcdefgh") "abcde…")))
  (should (eql (drover-transcript-time "2026-11-01T10:00:00.500Z") 1793527200.5))
  (should-not (drover-transcript-time "yesterday")))

(provide 'drover-transcript-test)
;;; drover-transcript-test.el ends here
