;;; drover-query-test.el --- Tests of the queries of the search of sessions  -*- lexical-binding: t; -*-

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

;; Reading, writing and evaluating queries, as the OpenCode service of
;; the fork reads them too (its packages/core/test/fork/session-search.test.ts).

;;; Code:

(require 'ert)
(require 'drover-query)

(defun drover-query-test-string (text &optional typing)
  "Return TEXT read then written again, with TYPING."
  (drover-query-string (plist-get (drover-query-parse text typing) :expression)))

(ert-deftest drover-query-test-reads-as-the-service ()
  "Terms side by side, OR, exclusion and groups read as the service reads them."
  (should (equal (plist-get (drover-query-parse "aiur \"home  lab\" -nomad (consul OR vault*)") :expression)
                 '(and (term "aiur" nil nil "aiur" nil)
                       (term "home lab" nil t "home lab" "home[[:space:]]+lab")
                       (not (term "nomad" nil nil "nomad" nil))
                       (or (term "consul" nil nil "consul" nil) (term "vault" t nil "vault" nil)))))
  (dolist (case '(;; OR binds looser than terms side by side; "or" is a word.
                  ("a b OR c or" . "a b OR c or")
                  ("-(a OR \"b c\") --d e-f" . "-(a OR \"b c\") d e-f")
                  ;; As typed: unclosed groups and quotes run to the end,
                  ;; dangling operators and stray parentheses are dropped.
                  ("OR (a \"b c" . "a \"b c\"")
                  ("a ) OR - ( )" . "a")
                  (" - ?! () " . "")
                  ;; Prefixes of two characters or more.
                  ("x* ab* \"OR\"" . "x ab* \"OR\"")))
    (should (equal (drover-query-test-string (car case)) (cdr case))))
  ;; Written, then read, it stays.
  (dolist (text '("a (b OR -\"c d\") -(e f) g*" "\"OR\" -x" "(a OR b) (c OR d)"))
    (should (equal (drover-query-test-string (drover-query-test-string text)) (drover-query-test-string text)))))

(ert-deftest drover-query-test-typing ()
  "As typed, the word the text ends in is a prefix, not after a blank, a quote or a parenthesis."
  (should (equal (drover-query-test-string "aiur homel" t) "aiur homel*"))
  (should (equal (drover-query-test-string "aiur homel " t) "aiur homel"))
  (should (equal (drover-query-test-string "aiur \"homel" t) "aiur \"homel\""))
  (should (equal (drover-query-test-string "(a OR b)" t) "a OR b"))
  (should (equal (drover-query-test-string "x" t) "x")))

(ert-deftest drover-query-test-fields ()
  "Fields apply to the whole query, negated or not, and nowhere else."
  (let ((parsed (drover-query-parse "project:drover aiur -status:idle harness:\"claude code\" host: OR")))
    (should (equal (plist-get parsed :filters)
                   '(("project" "drover" nil) ("status" "idle" t) ("harness" "claude code" nil))))
    (should (equal (drover-query-string (plist-get parsed :expression)) "aiur"))
    (should-not (plist-get parsed :error)))
  ;; A word naming no field is a word.
  (should (equal (drover-query-test-string "foo:bar") "foo:bar"))
  (dolist (text '("(project:drover OR aiur)" "aiur -(status:idle b)"))
    (should (string-match-p "apply to the whole query" (plist-get (drover-query-parse text) :error))))
  (should (string-match-p "32 terms"
                          (plist-get (drover-query-parse (mapconcat (lambda (n) (format "w%d" n))
                                                                    (number-sequence 1 33) " "))
                                     :error))))

(ert-deftest drover-query-test-terms ()
  "Terms come once each; those held rather than lacked, and their words, for snippets."
  (let ((expression (plist-get (drover-query-parse "a -b (A OR -(c -d)) \"e f\"") :expression)))
    (should (equal (mapcar #'drover-query-term-text (drover-query-terms expression)) '("a" "b" "c" "d" "e f")))
    (should (equal (mapcar #'drover-query-term-text (drover-query-positives expression)) '("a" "d" "e f")))
    (should (equal (drover-query-words expression) '("a" "d" "e" "f")))
    (should (drover-query-single-term (plist-get (drover-query-parse "\"x y\"") :expression)))
    (should-not (drover-query-single-term (plist-get (drover-query-parse "-x") :expression)))
    (should (drover-query-evaluate expression (lambda (term) (not (equal (drover-query-term-text term) "b")))))
    (should-not (drover-query-evaluate expression #'always))))

(ert-deftest drover-query-test-in ()
  "What a session tells holds, cannot hold, or leaves what was said to decide."
  (let ((expression (plist-get (drover-query-parse "aiur -nomad (consul OR \"vault  server\")") :expression)))
    (should (eq (drover-query-in expression "aiur\nnomad") nil))
    (should (eq (drover-query-in expression "aiur consul") 'unknown))
    (should (eq (drover-query-in expression "x") 'unknown)))
  (let ((expression (plist-get (drover-query-parse "aiur (consul OR \"vault server\")") :expression)))
    (should (eq (drover-query-in expression "aiur the vault\n server") t))
    (should (eq (drover-query-in expression "aiur vaultserver") 'unknown)))
  (should (eq (drover-query-in (plist-get (drover-query-parse "-x") :expression) "y") 'unknown))
  (should (eq (drover-query-in (plist-get (drover-query-parse "a OR -x") :expression) "x") 'unknown))
  (should (eq (drover-query-in (plist-get (drover-query-parse "a OR b") :expression) "zb") t)))

(provide 'drover-query-test)
;;; drover-query-test.el ends here
