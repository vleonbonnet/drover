;;; bytecode-compare.el --- Compare standalone byte code  -*- lexical-binding: t; -*-

;;; Commentary:

;; Read, never load, the compiled definitions.  Printing them again gives
;; shared objects canonical reader labels: the compiler can write #2=
;; where another compilation writes #1= for exactly the same object.

;;; Code:

(defun bytecode-compare-docstrings (form seen)
  "Remove docstring positions in FORM, visiting shared objects once via SEEN."
  (when (and (or (consp form) (vectorp form) (byte-code-function-p form))
             (not (gethash form seen)))
    (puthash form t seen)
    (cond
     ((consp form)
      (if (and (equal (car form) "BYTECODE") (integerp (cdr form)))
          (setcdr form 'position)
        (bytecode-compare-docstrings (car form) seen)
        (bytecode-compare-docstrings (cdr form) seen)))
     (t (dotimes (i (length form))
          (bytecode-compare-docstrings (aref form i) seen)))))
  form)

(defun bytecode-compare-read (file)
  "Return FILE's definitions with compiler-specific numbering removed."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((load-file-name "BYTECODE")
          (print-circle t)
          (print-gensym t)
          (print-length nil)
          (print-level nil)
          forms)
      (condition-case nil
          (while t
            (let ((text (prin1-to-string
                         (bytecode-compare-docstrings
                          (read (current-buffer)) (make-hash-table :test #'eq)))))
              (push (replace-regexp-in-string
                     "\\(#:[A-Za-z_-]*[A-Za-z_-]\\)[0-9]+" "\\1" text)
                    forms)))
        (end-of-file nil))
      (nreverse forms))))

(let ((standalone (pop command-line-args-left))
      (status 0))
  (dolist (file command-line-args-left)
    (unless (equal (bytecode-compare-read file)
                   (bytecode-compare-read (expand-file-name file standalone)))
      (message "%s differs when compiled without the optional front ends" file)
      (setq status 1)))
  (kill-emacs status))

;;; bytecode-compare.el ends here
