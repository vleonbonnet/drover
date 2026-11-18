;;; drover-query.el --- The queries of the search of sessions  -*- lexical-binding: t; -*-

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

;; What `drover-search' reads: words and "quoted phrases", all of which
;; a session must hold, OR between them for either, a leading - to
;; exclude a term or group, parentheses to group, and fields of the
;; session's record such as project:drover, which apply to the whole
;; query.  A term holds when anything of the session holds it, the
;; words of a session in any of its messages: the backends searching
;; what was said evaluate the query over each conversation, the OpenCode
;; service of my fork in its own index (its route
;; /api/experimental/session/search/sessions reads the same syntax).
;; This file only reads and evaluates queries; `drover-search' applies
;; them to sessions.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defconst drover-query-fields '("harness" "project" "branch" "host" "status" "archived")
  "The fields of a session's record a query may name, as FIELD:VALUE.")

(defconst drover-query-limit 32
  "The most terms one query may hold, as the OpenCode service takes.")

;;;; Terms

;; A term is a list (term TEXT PREFIX PHRASE NEEDLE REGEXP): plain data,
;; as a search in progress keeps them across a reload of drover.

(defun drover-query-term-text (term)
  "Return the text of TERM, its words separated by single spaces."
  (nth 1 term))

(defun drover-query-term-prefix (term)
  "Return non-nil when TERM matches as a prefix, typed ending in *."
  (nth 2 term))

(defun drover-query-term-phrase (term)
  "Return non-nil when TERM was typed in double quotes."
  (nth 3 term))

(defun drover-query--term (text prefix phrase)
  "Return the term of TEXT, PREFIX and PHRASE.
Nil when TEXT holds no letter nor digit, which no search can match."
  (let ((words (split-string text "[[:space:]]+" t)))
    (when (and words (string-match-p "[[:alnum:]]" text))
      (let ((text (string-join words " "))
            (downcased (mapcar #'downcase words)))
        (list 'term text prefix phrase (downcase text)
              (and (cdr words)
                   (mapconcat #'regexp-quote downcased "[[:space:]]+")))))))

(defun drover-query-term-in (term haystack)
  "Return non-nil when TERM is in HAYSTACK, a downcased string.
A word anywhere, a prefix as a word, the words of a phrase in order
across any blanks: no string is made."
  (if-let* ((regexp (nth 5 term)))
      (string-match-p regexp haystack)
    (string-search (nth 4 term) haystack)))

;;;; Reading

(defun drover-query--tokens (text typing)
  "Return the tokens of TEXT, a list.
Each is `open', `close', `or', `not', (phrase TEXT), (word TEXT PREFIX)
or (field NAME VALUE).  With TYPING, a last word TEXT ends in, without
a blank after it, is a prefix: it is being typed."
  (let ((position 0)
        (tokens nil)
        (case-fold-search nil))
    (while (length> text position)
      (cond
       ((eq (string-match "[[:space:]]+" text position) position)
        (setq position (match-end 0)))
       ((eq (aref text position) ?\()
        (push 'open tokens)
        (setq position (1+ position)))
       ((eq (aref text position) ?\))
        (push 'close tokens)
        (setq position (1+ position)))
       ((and (eq (string-match "\\([a-z]+\\):\"\\([^\"]*\\)\"?" text position) position)
             (member (match-string 1 text) drover-query-fields))
        (push (list 'field (match-string 1 text) (match-string 2 text)) tokens)
        (setq position (match-end 0)))
       ((eq (string-match "\"\\([^\"]*\\)\"?" text position) position)
        (push (list 'phrase (match-string 1 text)) tokens)
        (setq position (match-end 0)))
       ((eq (string-match "-[^[:space:])]" text position) position)
        (push 'not tokens)
        (setq position (1+ position)))
       (t
        (string-match "[^[:space:]\"()]+" text position)
        (let ((word (match-string 0 text)))
          (setq position (match-end 0))
          (cond
           ((equal word "OR") (push 'or tokens))
           ((and (string-match "\\`\\([a-z]+\\):\\(.*\\)\\'" word)
                 (member (match-string 1 word) drover-query-fields))
            (unless (string-empty-p (match-string 2 word))
              (push (list 'field (match-string 1 word) (match-string 2 word)) tokens)))
           (t
            (let* ((base (replace-regexp-in-string "\\*+\\'" "" word))
                   (typed (and typing (length= text position)
                               (string-match-p "[[:alnum:]]\\'" word))))
              (push (list 'word base (and (or typed (not (equal base word)))
                                          (length> base 1)))
                    tokens))))))))
    (nreverse tokens)))

(defun drover-query--join (type items)
  "Return ITEMS joined by TYPE, `and' or `or', or the one item, or nil."
  (if (cdr items) (cons type items) (car items)))

(defun drover-query--read (tokens)
  "Return the expression TOKENS tell, or nil when they tell no term.
`or' binds looser than terms side by side, `not' tighter; an unclosed
parenthesis runs to the end, and a stray one, an operator without an
operand and a term without a letter or digit are dropped."
  (let ((depth 0))
    (cl-labels
        ((disjunction ()
           (let ((items nil) (more t))
             (while (and more tokens)
               (when-let* ((item (conjunction)))
                 (push item items))
               (if (eq (car tokens) 'or) (pop tokens) (setq more nil)))
             (drover-query--join 'or (nreverse items))))
         (conjunction ()
           (let ((items nil) (more t))
             (while (and more tokens)
               (pcase (car tokens)
                 ('or (setq more nil))
                 ('close (if (> depth 0) (setq more nil) (pop tokens)))
                 (_ (when-let* ((item (unary)))
                      (push item items)))))
             (drover-query--join 'and (nreverse items))))
         (unary ()
           (if (eq (car tokens) 'not)
               (progn
                 (pop tokens)
                 (when-let* ((item (unary)))
                   (if (eq (car-safe item) 'not) (cadr item) (list 'not item))))
             (primary)))
         (primary ()
           (let ((token (car tokens)))
             (unless (memq token '(nil or close))
               (pop tokens)
               (pcase token
                 ('open
                  (cl-incf depth)
                  (prog1 (disjunction)
                    (cl-decf depth)
                    (when (eq (car tokens) 'close) (pop tokens))))
                 (`(phrase ,text) (drover-query--term text nil t))
                 (`(word ,text ,prefix) (drover-query--term text prefix nil))
                 (`(field ,name ,value) (list 'field name value)))))))
      (disjunction))))

(defun drover-query--fields-p (expression)
  "Return non-nil when EXPRESSION names a field of the session's record."
  (pcase expression
    (`(field . ,_) t)
    (`(not ,item) (drover-query--fields-p item))
    (`(,(or 'and 'or) . ,items) (seq-some #'drover-query--fields-p items))))

(defun drover-query-parse (text &optional typing)
  "Return the query TEXT tells, a plist.
:expression, what sessions must hold, a tree of terms (see
`drover-query-term-text'), (and . ITEMS), (or . ITEMS) and (not ITEM),
or nil for none; :filters, the fields of the session's record the
whole query names, a list of (NAME VALUE NEGATED); :error, a line,
when it names fields elsewhere than at its top, where they cannot
apply, or holds more than `drover-query-limit' terms.  With TYPING, a
last word TEXT ends in is a prefix, as it is being typed."
  (let* ((expression (drover-query--read (drover-query--tokens (or text "") typing)))
         (items (if (eq (car-safe expression) 'and) (cdr expression) (and expression (list expression))))
         (filters nil)
         (kept nil))
    (dolist (item items)
      (pcase item
        (`(field ,name ,value) (push (list name value nil) filters))
        (`(not (field ,name ,value)) (push (list name value t) filters))
        (_ (push item kept))))
    (let ((expression (drover-query--join 'and (nreverse kept))))
      (list :expression expression
            :filters (nreverse filters)
            :error (cond ((drover-query--fields-p expression)
                          "fields such as project:drover apply to the whole query, not inside OR or parentheses")
                         ((length> (drover-query-terms expression) drover-query-limit)
                          (format "a query holds %d terms at most" drover-query-limit)))))))

;;;; Writing and walking

(defun drover-query--term-string (term)
  "Return TERM as a query writes it."
  (let ((text (drover-query-term-text term)))
    (concat (if (or (drover-query-term-phrase term) (string-search " " text) (equal text "OR"))
                (concat "\"" text "\"")
              text)
            (if (drover-query-term-prefix term) "*" ""))))

(defun drover-query-string (expression)
  "Return EXPRESSION as a query writes it, which reading gives back.
As sent to the backends and to other hosts, and as the OpenCode service
reads it."
  (pcase expression
    ('nil "")
    (`(term . ,_) (drover-query--term-string expression))
    (`(not ,item) (concat "-" (if (eq (car item) 'term)
                                  (drover-query-string item)
                                (concat "(" (drover-query-string item) ")"))))
    (`(and . ,items) (mapconcat (lambda (item)
                                  (if (eq (car item) 'or)
                                      (concat "(" (drover-query-string item) ")")
                                    (drover-query-string item)))
                                items " "))
    (`(or . ,items) (mapconcat #'drover-query-string items " OR "))
    (`(field ,name ,value) (concat name ":" (if (string-match-p "[[:space:]\"()]" value)
                                                (concat "\"" value "\"")
                                              value)))))

(defun drover-query-term-key (term)
  "Return what tells TERM from others: its text downcased, and whether a prefix.
Terms of equal keys are one."
  (cons (nth 4 term) (and (drover-query-term-prefix term) t)))

(defun drover-query--collect (expression positive-only)
  "Return the terms of EXPRESSION, once each, in order.
With POSITIVE-ONLY, those under no `not', or under two."
  (let ((seen nil) (terms nil))
    (cl-labels ((visit (node negated)
                  (pcase node
                    (`(term . ,_)
                     (unless (or (and positive-only negated) (member (drover-query-term-key node) seen))
                       (push (drover-query-term-key node) seen)
                       (push node terms)))
                    (`(not ,item) (visit item (not negated)))
                    (`(,(or 'and 'or) . ,items) (dolist (item items) (visit item negated))))))
      (visit expression nil))
    (nreverse terms)))

(defun drover-query-terms (expression)
  "Return the terms of EXPRESSION, once each, in the order they come."
  (drover-query--collect expression nil))

(defun drover-query-positives (expression)
  "Return the terms a session matching EXPRESSION holds rather than lacks.
Those under no `not', or under two, once each, in order."
  (drover-query--collect expression t))

(defun drover-query-words (expression)
  "Return the words of the terms of EXPRESSION a matching session holds, downcased.
For highlighting what matched."
  (delete-dups (mapcan (lambda (term) (split-string (nth 4 term) " " t))
                       (drover-query-positives expression))))

(defun drover-query-single-term (expression)
  "Return the term EXPRESSION is when it is one term held, else nil."
  (and (eq (car-safe expression) 'term) expression))

;;;; Evaluating

(defun drover-query-evaluate (expression holds)
  "Return non-nil when EXPRESSION holds, HOLDS a function of a term.
HOLDS returns non-nil when the term is held."
  (pcase expression
    (`(term . ,_) (and (funcall holds expression) t))
    (`(not ,item) (not (drover-query-evaluate item holds)))
    (`(and . ,items) (seq-every-p (lambda (item) (drover-query-evaluate item holds)) items))
    (`(or . ,items) (and (seq-some (lambda (item) (drover-query-evaluate item holds)) items) t))))

(defun drover-query-in (expression haystack)
  "Return whether EXPRESSION holds of HAYSTACK, a part of what a session holds.
HAYSTACK is a downcased string: a term in it holds, one not in it may
still be elsewhere, in what was said.  So the value is t when
EXPRESSION holds whatever the rest holds, nil when it cannot, and
`unknown' otherwise, as three-valued logic has it.  It makes no
string nor closure: it runs for every session as each key is typed."
  (pcase (car expression)
    ('term (if (drover-query-term-in expression haystack) t 'unknown))
    ('not (let ((value (drover-query-in (cadr expression) haystack)))
            (cond ((eq value t) nil)
                  ((null value) t)
                  (t 'unknown))))
    ('and (let ((items (cdr expression)) (value t))
            (while (and items value)
              (let ((item (drover-query-in (car items) haystack)))
                (setq value (cond ((null item) nil)
                                  ((eq item 'unknown) 'unknown)
                                  (t value))
                      items (cdr items))))
            value))
    ('or (let ((items (cdr expression)) (value nil))
           (while (and items (not (eq value t)))
             (let ((item (drover-query-in (car items) haystack)))
               (setq value (cond ((eq item t) t)
                                 ((eq item 'unknown) 'unknown)
                                 (t value))
                     items (cdr items))))
           value))
    (_ t)))

(provide 'drover-query)
;;; drover-query.el ends here
