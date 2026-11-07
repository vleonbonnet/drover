;;; drover-usage-test.el --- Tests of what agents cost  -*- lexical-binding: t; -*-

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

;; The usage engines record, totalled: Claude Code's transcripts,
;; Codex's rollouts and OpenCode's database, all in the throwaway homes
;; of `drover-test-with-homes', OpenCode's database made by the tests,
;; with the tables and members its service writes.  Sources are read
;; synchronously (`drover-usage-scan'); sums are checked exactly.

;;; Code:

(require 'drover-test-helpers)
(require 'drover-transcript-fixtures)
(require 'drover-list)
(require 'drover-usage-report)

(defconst drover-usage-test-prices
  '(("opus" :input 5 :output 25 :cache-read 0.5 :cache-write 6.25 :cache-write-1h 10)
    ("gpt" :input 1 :output 8 :cache-read 0.1 :cache-write 0))
  "Prices of the tests, dollars per million tokens.")

(defmacro drover-usage-test-with (&rest body)
  "Run BODY with the usage's sources fresh, over the fixtures' homes.
`home' is the homes' directory.  Every harness is priced, at
`drover-usage-test-prices', but OpenCode's GitHub Copilot, included;
OpenCode's catalog prices nothing."
  (declare (indent 0) (debug t))
  `(drover-test-with-homes
     (drover-test-with-backend nil
       (let ((drover-usage--started nil)
             (drover-usage--starting nil)
             (drover-usage--claude-index nil)
             (drover-usage--codex-index nil)
             (drover-usage--oc nil)
             (drover-usage--facts nil)
             (drover-usage--generation 0)
             (drover-usage--changed nil)
             (drover-usage--notify-timer nil)
             (drover-usage--remote (make-hash-table :test #'equal))
             (drover-usage--logins (make-hash-table :test #'equal))
             (drover-usage--catalog nil)
             (drover-usage-changed-hook nil)
             (drover-usage-filter nil)
             (drover-usage-period 'all)
             (drover-usage-group-by 'project)
             (drover-usage-breakdown 'model)
             (drover-usage-sort-by 'cost)
             (drover-usage-prices drover-usage-test-prices)
             (drover-usage-billing '((opencode "\\`github-copilot" included) (nil nil priced)))
             (drover-list-usage-function #'drover-usage-session-text)
             (kill-emacs-hook nil))
         (cl-letf (((symbol-function 'drover-usage--opencode-catalog) #'ignore)
                   ((symbol-function 'drover-usage--remote-hosts) #'ignore))
           (unwind-protect
               (progn ,@body)
             (drover-usage-stop)
             (when (timerp drover-usage--notify-timer)
               (cancel-timer drover-usage--notify-timer))))))))

(defun drover-usage-test-home ()
  "Return the directory of the homes of `drover-test-with-homes'."
  (file-name-directory (directory-file-name drover-state-directory)))

(defun drover-usage-test-sum (sum)
  "Return SUM as (INPUT OUTPUT READ WRITE COST PRICED INCLUDED)."
  (and sum
       (list (drover-usage-sum-input sum) (drover-usage-sum-output sum)
             (drover-usage-sum-read sum) (drover-usage-sum-write sum)
             (drover-usage-sum-cost sum)
             (drover-usage-sum-priced sum) (drover-usage-sum-included sum))))

(defun drover-usage-test-cost= (a b)
  "Return non-nil when the costs A and B are the same, to a billionth of a dollar."
  (< (abs (- a b)) 1e-9))

(defun drover-usage-test-totals (engine id)
  "Return the totals of the session ID of ENGINE, as `drover-usage-test-sum' does."
  (drover-usage-test-sum
   (drover-usage-session-totals (drover-session-create :backend engine :id id :engine engine :engine-id id))))

(defun drover-usage-test-check (totals tokens cost &optional priced included)
  "Check TOTALS against TOKENS, (INPUT OUTPUT READ WRITE), COST, PRICED and INCLUDED.
PRICED defaults to t."
  (should totals)
  (should (equal (seq-take totals 4) tokens))
  (should (drover-usage-test-cost= (nth 4 totals) cost))
  (should (eq (nth 5 totals) (if (eq priced 'none) nil t)))
  (should (eq (nth 6 totals) included)))

;;;; Claude Code

(defconst drover-usage-test-a "aaaaaaaa-0000-4000-8000-00000000000a"
  "A conversation of Claude Code.")

(defconst drover-usage-test-b "bbbbbbbb-0000-4000-8000-00000000000b"
  "A conversation forked from `drover-usage-test-a', its history copied.")

(defun drover-usage-test-reply (id directory time message usage &rest keys)
  "Return the record of a reply of conversation ID at TIME, MESSAGE its message.
USAGE is a plist of `drover-fixture-claude-usage'; KEYS more arguments
of `drover-fixture-claude-assistant'."
  (apply #'drover-fixture-claude-assistant id directory time "Done." :message message
         :usage (apply #'drover-fixture-claude-usage usage) keys))

(defun drover-usage-test-claude (now)
  "Write the transcripts of the tests' Claude Code conversations, around NOW.
A: a reply recorded twice, its usage told again with more output, a
reply writing into an hour's cache, a subagent; B, forked from A, its
first reply copied, then one of its own.  Return A's transcript."
  (let* ((directory (expand-file-name "work/alpha/" (drover-usage-test-home)))
         (a (drover-fixture-claude-file drover-usage-test-a directory))
         (b (drover-fixture-claude-file drover-usage-test-b directory))
         (first (list (drover-usage-test-reply drover-usage-test-a directory (- now 50) "msg_1"
                                               '(:input 10 :minutes 100 :read 1000 :output 50))
                      (drover-usage-test-reply drover-usage-test-a directory (- now 49) "msg_1"
                                               '(:input 10 :minutes 100 :read 1000 :output 70)))))
    (make-directory directory t)
    (drover-fixture-write
     a (append (list (drover-fixture-claude-user drover-usage-test-a directory (- now 51) "Fix it"))
               first
               (list (drover-usage-test-reply drover-usage-test-a directory (- now 40) "msg_2"
                                              '(:input 5 :hour 200 :read 2000 :output 30))
                     ;; Made up by the CLI: no usage of the API.
                     (drover-fixture-claude-api-error drover-usage-test-a directory (- now 39) "Overloaded"))))
    (drover-fixture-write
     (expand-file-name (concat drover-usage-test-a "/subagents/agent-x.jsonl") (file-name-directory a))
     (list (drover-usage-test-reply drover-usage-test-a directory (- now 45) "msg_3"
                                    '(:input 1 :minutes 10 :read 100 :output 9) :sidechain t)))
    (drover-fixture-write
     b (append (mapcar (lambda (line) (string-replace drover-usage-test-a drover-usage-test-b line)) first)
               (list (drover-usage-test-reply drover-usage-test-b directory (- now 30) "msg_4"
                                              '(:input 2 :read 0 :output 4)))))
    a))

(ert-deftest drover-usage-test-claude-code ()
  "Claude Code's replies are counted once each, subagents with their conversation, priced."
  (drover-usage-test-with
    (let ((now (float-time)))
      (drover-usage-test-claude now)
      (drover-usage-scan)
      ;; A: msg_1 as told last, msg_2, and its subagent's msg_3.
      (drover-usage-test-check (drover-usage-test-totals 'claude-code drover-usage-test-a)
                               '(16 109 3100 310) (/ (+ 2925 3775 342.5) 1e6))
      ;; Its subagent alone, as the claude-code backend names it.
      (drover-usage-test-check (drover-usage-test-sum
                                (drover-usage-session-totals
                                 (drover-session-create :backend 'claude-code
                                                        :id (concat drover-usage-test-a "/agent-x")
                                                        :engine 'claude-code)))
                               '(1 9 100 10) 342.5e-6)
      ;; B: its copy of msg_1 counts for A, which recorded it first.
      (drover-usage-test-check (drover-usage-test-totals 'claude-code drover-usage-test-b)
                               '(2 4 0 0) 110e-6)
      (let ((total (drover-usage-totals 'today)))
        (drover-usage-test-check (drover-usage-test-sum total) '(18 113 3100 310)
                                 (/ (+ 2925 3775 342.5 110) 1e6))))))

(ert-deftest drover-usage-test-claude-code-appended ()
  "Replies appended are read as they come, the file read on from where it was, in pieces."
  (drover-usage-test-with
    (let* ((now (float-time))
           (file (drover-usage-test-claude now))
           (directory (expand-file-name "work/alpha/" home))
           (drover-usage-read-chunk 300))
      (drover-usage-scan)
      (should (= (plist-get (drover-transcript-lookup drover-usage--claude-index file) :offset)
                 (file-attribute-size (file-attributes file))))
      ;; The same message told again, then a new one.
      (drover-fixture-claude-append
       file
       (drover-usage-test-reply drover-usage-test-a directory (- now 20) "msg_2"
                                '(:input 5 :hour 200 :read 2000 :output 31))
       (drover-usage-test-reply drover-usage-test-a directory (- now 10) "msg_5"
                                '(:input 100 :read 0 :output 1000)))
      (let ((reads 0))
        (cl-letf* ((read (symbol-function 'drover-usage--claude-read))
                   ((symbol-function 'drover-usage--claude-read)
                    (lambda (&rest arguments) (cl-incf reads) (apply read arguments))))
          (drover-usage-scan))
        ;; Two records of about 900 bytes, 300 at a time, from where it ended.
        (should (<= 2 reads 4)))
      ;; msg_2 as told last, over the end of the file; msg_5 new.
      (drover-usage-test-check (drover-usage-test-totals 'claude-code drover-usage-test-a)
                               '(116 1110 3100 310)
                               (/ (+ 2925 3800 25500 342.5) 1e6)))))

(ert-deftest drover-usage-test-claude-code-kept ()
  "What was read is kept between Emacs sessions, and only what changed is read again."
  (drover-usage-test-with
    (let ((now (float-time)))
      (drover-usage-test-claude now)
      (drover-usage-scan)
      (drover-usage-stop)
      (setq drover-usage--facts nil)
      (let ((reads 0))
        (cl-letf* ((read (symbol-function 'drover-usage--claude-read))
                   ((symbol-function 'drover-usage--claude-read)
                    (lambda (&rest arguments) (cl-incf reads) (apply read arguments))))
          (drover-usage-scan))
        (should (= reads 0)))
      (drover-usage-test-check (drover-usage-test-totals 'claude-code drover-usage-test-a)
                               '(16 109 3100 310) (/ (+ 2925 3775 342.5) 1e6)))))

(ert-deftest drover-usage-test-long-lines ()
  "A reply of half a megabyte counts; names from outside are bounded."
  (drover-usage-test-with
    (let* ((now (float-time))
           (directory (expand-file-name "work/alpha/" home))
           (file (drover-fixture-claude-file drover-usage-test-a directory))
           (huge (make-string 500000 ?y)))
      (make-directory directory t)
      (drover-fixture-write
       file (list (drover-usage-test-reply drover-usage-test-a directory (- now 5) "msg_9"
                                           '(:input 1 :output 2) :model (concat "opus-" huge))
                  (drover-fixture-claude-user drover-usage-test-a directory (- now 4) huge)
                  (drover-usage-test-reply drover-usage-test-a directory (- now 3) "msg_10"
                                           '(:input 3 :output 4))))
      (drover-usage-scan)
      (drover-usage-test-check (drover-usage-test-totals 'claude-code drover-usage-test-a)
                               '(4 6 0 0) (/ (+ 5 50 15 100) 1e6))
      (dolist (fact (drover-usage--facts-facts (drover-usage--current-facts)))
        (should (<= (length (drover-usage--fact-model fact)) 201))))))

;;;; Codex

(defconst drover-usage-test-codex-id "019f0000-0000-7000-8000-0000000000c1"
  "A conversation of Codex.")

(defun drover-usage-test-codex-tokens (time total last)
  "Return a count of tokens Codex records at TIME, its TOTAL and LAST counts.
Each a list (INPUT CACHED OUTPUT)."
  (cl-flet ((usage (counts)
              (list :input_tokens (nth 0 counts) :cached_input_tokens (nth 1 counts)
                    :output_tokens (nth 2 counts) :reasoning_output_tokens 0
                    :total_tokens (+ (nth 0 counts) (nth 2 counts)))))
    (drover-fixture-codex-event time "token_count"
                                :info (list :total_token_usage (usage total)
                                            :last_token_usage (usage last)
                                            :model_context_window 400000)
                                :rate_limits :null)))

(ert-deftest drover-usage-test-codex ()
  "Codex's requests count once each, their cached input apart, a count repeated not again."
  (drover-usage-test-with
    (let* ((now (float-time))
           (directory (expand-file-name "work/beta/" home))
           (file (drover-fixture-codex drover-usage-test-codex-id directory :start (- now 100))))
      (drover-fixture-write
       file (list (drover-usage-test-codex-tokens (- now 90) '(1000 600 50) '(1000 600 50))
                  ;; The same totals again, as rate limits change.
                  (drover-usage-test-codex-tokens (- now 89) '(1000 600 50) '(1000 600 50))
                  (drover-usage-test-codex-tokens (- now 80) '(3000 2400 80) '(2000 1800 30)))
       t)
      (drover-usage-scan)
      ;; Input 1000 - 600 + 2000 - 1800, output 80, read 2400, at gpt prices.
      (drover-usage-test-check (drover-usage-test-totals 'codex drover-usage-test-codex-id)
                               '(600 80 2400 0) (/ (+ 600 (* 80 8) (* 2400 0.1)) 1e6))
      (let ((fact (car (drover-usage--facts-facts (drover-usage--current-facts)))))
        (should (equal (drover-usage--fact-model fact) "gpt-5.5"))
        (should (equal (drover-usage--fact-provider fact) "openai"))
        (should (equal (drover-usage--fact-directory fact) directory))))))

;;;; OpenCode

(defun drover-usage-test-oc-db ()
  "Make OpenCode's database, with the tables and columns its service reads; return it."
  (make-directory (file-name-directory drover-opencode-database) t)
  (let ((db (sqlite-open drover-opencode-database)))
    (sqlite-execute db "CREATE TABLE session_v2 (id text PRIMARY KEY, project_id text, parent_id text, directory text, title text, cost real, time_created integer, time_updated integer)")
    (sqlite-execute db "CREATE TABLE session_message (id text PRIMARY KEY, session_id text NOT NULL, type text NOT NULL, seq integer, time_created integer NOT NULL, time_updated integer, data text NOT NULL)")
    (sqlite-execute db "CREATE INDEX session_message_time_created_idx ON session_message (time_created)")
    db))

(defun drover-usage-test-oc-session (db id parent directory title time)
  "Add the OpenCode session ID to DB, of PARENT, in DIRECTORY, titled TITLE, at TIME."
  (sqlite-execute db "INSERT INTO session_v2 VALUES (?, 'prj', ?, ?, ?, 0, ?, ?)"
                  (list id parent directory title (round (* 1000 time)) (round (* 1000 time)))))

(cl-defun drover-usage-test-oc-step (db id session time &key (provider "prodsec-gateway")
                                        (model "claude-opus-5-5") tokens cost (done t))
  "Add the step ID of SESSION to DB at TIME, through PROVIDER and MODEL.
TOKENS is (INPUT OUTPUT REASONING READ WRITE), COST its cost; DONE nil
for a step running.  A step already there is replaced."
  (sqlite-execute
   db "INSERT OR REPLACE INTO session_message VALUES (?, ?, 'assistant', 0, ?, ?, ?)"
   (list id session (round (* 1000 time)) (round (* 1000 time))
         (json-serialize
          (append `((id . ,id) (type . "assistant") (agent . "build")
                    (model . ((id . ,model) (providerID . ,provider)))
                    (time . ,(append `((created . ,(round (* 1000 time))))
                                     (and done `((completed . ,(round (* 1000 (+ time 1)))))))))
                  (and tokens
                       `((tokens . ((input . ,(nth 0 tokens)) (output . ,(nth 1 tokens))
                                    (reasoning . ,(nth 2 tokens))
                                    (cache . ((read . ,(nth 3 tokens)) (write . ,(nth 4 tokens))))))
                         (cost . ,cost))))))))

(defun drover-usage-test-oc (now)
  "Fill OpenCode's database of the tests around NOW; return it, open.
ses_a, a subagent ses_b of it, steps through GitHub Copilot, included,
and a gateway, priced; one of three days ago; one running."
  (let ((db (drover-usage-test-oc-db))
        (directory (expand-file-name "work/oc/" (drover-usage-test-home))))
    (drover-usage-test-oc-session db "ses_a" nil directory "Agent work" now)
    (drover-usage-test-oc-session db "ses_b" "ses_a" directory "Subtask" now)
    (drover-usage-test-oc-step db "msg_0" "ses_a" (- now (* 3 86400)) :provider "github-copilot"
                               :model "claude-opus-5.5" :tokens '(7 0 0 0 0) :cost 0.1)
    (drover-usage-test-oc-step db "msg_1" "ses_a" (- now 120) :provider "github-copilot"
                               :model "claude-opus-5.5" :tokens '(3 40 10 500 20) :cost 0.05)
    (drover-usage-test-oc-step db "msg_2" "ses_a" (- now 100) :tokens '(1 5 0 100 0) :cost 0.01)
    (drover-usage-test-oc-step db "msg_3" "ses_b" (- now 90) :tokens '(2 3 0 50 5) :cost 0.002)
    (drover-usage-test-oc-step db "msg_4" "ses_a" (- now 10) :done nil)
    db))

(ert-deftest drover-usage-test-opencode ()
  "OpenCode's steps count as its service recorded them, a subscription's included, a step once."
  (skip-unless (sqlite-available-p))
  (drover-usage-test-with
    (let* ((now (float-time))
           (db (drover-usage-test-oc now)))
      (unwind-protect
          (progn
            (drover-usage-scan)
            ;; ses_a: Copilot's included, the gateway's at its recorded cost.
            (drover-usage-test-check (drover-usage-test-totals 'opencode "ses_b") '(2 3 50 5) 0.002)
            (drover-usage-test-check (drover-usage-test-totals 'opencode "ses_a")
                                     '(13 58 650 25) 0.012 t t)
            ;; Today, without the step of three days ago.
            (drover-usage-test-check (drover-usage-test-sum (drover-usage-totals 'today))
                                     '(6 58 650 25) 0.012 t t)
            ;; The running step ends: counted once.
            (drover-usage-test-oc-step db "msg_4" "ses_a" (- now 10) :tokens '(4 6 0 0 0) :cost 0.004)
            (drover-usage-scan)
            (drover-usage-test-check (drover-usage-test-totals 'opencode "ses_a")
                                     '(17 64 650 25) 0.016 t t)
            (drover-usage-scan)
            (drover-usage-test-check (drover-usage-test-totals 'opencode "ses_a")
                                     '(17 64 650 25) 0.016 t t)
            ;; Kept: an Emacs starting reads only what came since.
            (drover-usage-stop)
            (sqlite-execute db "DELETE FROM session_message WHERE id = 'msg_0'")
            (drover-usage-test-oc-step db "msg_5" "ses_b" (- now 5) :tokens '(1 1 0 0 0) :cost 0.001)
            (drover-usage-scan)
            (drover-usage-test-check (drover-usage-test-totals 'opencode "ses_a")
                                     '(18 65 650 25) 0.017 t t))
        (sqlite-close db)))))

(ert-deftest drover-usage-test-opencode-unreadable ()
  "Without OpenCode's database, the report tells why, and totals the others."
  (drover-usage-test-with
    (drover-usage-test-claude (float-time))
    (drover-usage-scan)
    (should (equal (drover-usage--oc-failure (drover-usage--oc-current)) "no database to read"))
    (drover-usage-test-check (drover-usage-test-sum (drover-usage-totals 'all)) '(18 113 3100 310)
                             (/ (+ 2925 3775 342.5 110) 1e6))))

;;;; Billing and prices

(ert-deftest drover-usage-test-included-value ()
  "What a subscription included shows what it would have cost at list prices, in green."
  (let ((drover-usage-billing '((claude-code nil included)))
        (drover-usage-prices '(("opus" :input 5 :output 25 :cache-read 0.5 :cache-write 6.25))))
    (let* ((pricer (drover-usage--pricer))
           (local (drover-usage--fact-make :harness 'claude-code :model "claude-opus-5" :id "c"
                                           :input 1000000 :output 100000))
           (remote (drover-usage--fact-make :host "core" :harness 'codex :model "gpt-6" :id "x"
                                            :input 10 :billing 'included :cost 1.25 :source "included"))
           (priced (drover-usage--fact-make :host "core" :harness 'opencode :id "o"
                                            :input 10 :billing 'priced :cost 12.4 :source "recorded"))
           (local-sum (drover-usage--fact-sum local pricer))
           (both (drover-usage--add (drover-usage--fact-sum priced pricer)
                                    (drover-usage--fact-sum remote pricer))))
      ;; $5 a million in, $25 out: none of it paid.
      (should (equal (funcall pricer local) '(included nil "included" 7.5)))
      (should (= (drover-usage-sum-cost local-sum) 0))
      (should (= (drover-usage-sum-value local-sum) 7.5))
      (should (equal (drover-usage-format-short local-sum) "$7.50"))
      (should (eq (get-text-property 0 'font-lock-face (drover-usage-format-short local-sum))
                  'drover-usage-included))
      (should (equal (drover-usage-format-cost local-sum) "$7.50 included"))
      ;; Another host's, as it priced it; beside a cost paid.
      (should (equal (funcall pricer remote) '(included nil "included" 1.25)))
      (should (= (drover-usage-sum-cost both) 12.4))
      (let ((short (drover-usage-format-short both)))
        (should (equal short "$12.40+$1.25"))
        (should-not (get-text-property 0 'font-lock-face short))
        (should (eq (get-text-property 6 'font-lock-face short) 'drover-usage-included)))
      (should (equal (drover-usage-format-cost both) "$12.40 + $1.25 included"))
      ;; Without a price, it is included all the same.
      (let ((unknown (drover-usage--fact-sum (drover-usage--fact-make :harness 'claude-code :model "mystery"
                                                                      :id "m" :input 5)
                                             pricer)))
        (should (equal (drover-usage-format-short unknown) "incl."))
        (should (equal (drover-usage-format-cost unknown) "included"))))))

(ert-deftest drover-usage-test-included-value-travels ()
  "A host sends what its included usage would have cost; its peer shows it."
  (let* ((row (vector "claude-code" "c" "" 20261106 "anthropic" "claude-opus-5" "" 10 20 0 0 7.5
                      "included" "included" "Title"))
         (fact (drover-usage-decode-row row "core")))
    (should (eq (drover-usage--fact-billing fact) 'included))
    (should (= (drover-usage--fact-cost fact) 7.5))
    (should (equal (nth 3 (funcall (drover-usage--pricer) fact)) 7.5))
    ;; An older host sends none.
    (aset row 11 :false)
    (should-not (nth 3 (funcall (drover-usage--pricer) (drover-usage-decode-row row "core"))))))

(ert-deftest drover-usage-test-billing ()
  "Claude Code logged in to an account, and Codex through ChatGPT, are included; a key or gateway is not."
  (drover-usage-test-with
    (let ((drover-usage-billing (eval (car (get 'drover-usage-billing 'standard-value)) t)))
      (should (eq (drover-usage-billing 'claude-code "anthropic") 'priced))
      (drover-test-write (drover-usage--claude-config-file)
                         "{\"oauthAccount\": {\"billingType\": \"stripe_subscription\"}, \"projects\": {}}")
      (should (eq (drover-usage-billing 'claude-code "anthropic") 'included))
      (drover-test-write (expand-file-name "settings.json" drover-claude-code-home)
                         "{\"env\": {\"ANTHROPIC_BASE_URL\": \"https://gateway.example\"}}")
      (should (eq (drover-usage-billing 'claude-code "gateway") 'priced))
      (should (eq (drover-usage-billing 'codex "openai") 'priced))
      (drover-test-write (expand-file-name "auth.json" drover-codex-home)
                         "{\"auth_mode\": \"chatgpt\", \"tokens\": {}}")
      (should (eq (drover-usage-billing 'codex "openai") 'included))
      ;; Copilot bills its tokens, as OpenCode records them.
      (should (eq (drover-usage-billing 'opencode "github-copilot") 'priced))
      (should (eq (drover-usage-billing 'opencode "prodsec-gateway") 'priced)))))

(ert-deftest drover-usage-test-prices ()
  "Prices come from the user's table, then OpenCode's catalog, then drover's own."
  (drover-usage-test-with
    (let ((drover-usage-prices nil))
      (should (equal (cdr (drover-usage-price 'claude-code "claude-opus-5-5")) "drover's estimate"))
      (cl-letf (((symbol-function 'drover-usage--opencode-catalog)
                 (let ((catalog `(((id . "claude-opus-5.5") (providerID . "github-copilot")
                                   (cost . (((input . 9) (output . 9) (cache . ((read . 0) (write . 0)))))))
                                  ((id . "claude-opus-5-5") (providerID . "anthropic")
                                   (cost . (((input . 4) (output . 20) (cache . ((read . 0.4) (write . 5))))
                                            ((tier . ((type . "context") (size . 200000)))
                                             (input . 8) (output . 40) (cache . ((read . 0.8) (write . 10))))))))))
                   (lambda () catalog))))
        (should (equal (drover-usage-price 'claude-code "claude-opus-5-5")
                       '((:input 4 :output 20 :cache-read 0.4 :cache-write 5) . "OpenCode's catalog, anthropic"))))
      (let ((drover-usage-prices '(("opus" :input 1 :output 2))))
        (should (equal (cdr (drover-usage-price 'claude-code "claude-opus-5-5")) "your prices"))))))

(ert-deftest drover-usage-test-periods ()
  "Today, this week and this month begin where the calendar says."
  (let ((drover-usage-week-start 1)
        ;; Wednesday 4 November 2026, noon.
        (now (float-time (encode-time (list 0 0 12 4 11 2026)))))
    (should (= (drover-usage-period-start 'today now) 20261104))
    (should (= (drover-usage-period-start 'week now) 20261102))
    (should (= (drover-usage-period-start 'month now) 20261101))
    (should-not (drover-usage-period-start 'all now))
    (let ((drover-usage-week-start 0))
      (should (= (drover-usage-period-start 'week now) 20261101)))))

;;;; The report

(ert-deftest drover-usage-test-report ()
  "The report groups and breaks down every harness's usage, and tells where costs come from."
  (skip-unless (sqlite-available-p))
  (drover-usage-test-with
    (let* ((now (float-time))
           (db (drover-usage-test-oc now)))
      (unwind-protect
          (progn
            (drover-usage-test-claude now)
            (drover-usage-scan)
            (let ((report (drover-usage-report 'all 'harness 'model)))
              (should (equal (mapcar #'cadr (plist-get report :groups)) '("OpenCode" "Claude Code")))
              (should (equal (mapcar #'cadr (nth 3 (assoc 'opencode (plist-get report :groups))))
                             '("claude-opus-5-5" "claude-opus-5.5"))))
            (let ((report (drover-usage-report 'all 'session nil)))
              (should (member "Agent work" (mapcar #'cadr (plist-get report :groups))))
              ;; A subagent's usage is its parent's.
              (should-not (member "Subtask" (mapcar #'cadr (plist-get report :groups)))))
            (let ((drover-usage-filter '(:harness opencode)))
              (drover-usage-test-check (drover-usage-test-sum (plist-get (drover-usage-report 'all 'model nil) :total))
                                       '(13 58 650 25) 0.012 t t))
            (save-window-excursion
              (drover-usage)
              (with-current-buffer drover-usage-buffer-name
                (should (derived-mode-p 'drover-usage-mode))
                (let ((text (buffer-string)))
                  (should (string-match-p "Usage ever, by project" text))
                  (should (string-match-p "\\$0\\.02\\+" text))
                  (should (string-match-p "OpenCode · github-copilot · claude-opus-5.5: included" text))
                  (should (string-match-p "recorded by OpenCode" text))
                  (should (string-match-p "Claude Code · anthropic · claude-opus-5-5: your prices" text)))
                (funcall (drover-usage--set 'drover-usage-group-by) 'day)
                (should (string-match-p "Usage ever, by day" (buffer-string))))
              (kill-buffer drover-usage-buffer-name)))
        (sqlite-close db)))))

(ert-deftest drover-usage-test-menu ()
  "The report's menu shows each setting's value, on the list's families of keys."
  (drover-usage-test-with
    (dolist (key '("=d" "=g" "=b" "=o" "=p" "=H" "=h" "=m" "=v" "=c" "g"))
      (should (transient-get-suffix 'drover-usage-menu key)))))

;;;; The list

(ert-deftest drover-usage-test-list ()
  "The list shows each session's totals in its details and its cost, drawn again as they change."
  (drover-usage-test-with
    (let* ((now (float-time))
           (file (drover-usage-test-claude now))
           (directory (expand-file-name "work/alpha/" home))
           (session (drover-session-create :backend 'fake :id "a" :title "Alpha" :status 'idle
                                           :engine 'claude-code :engine-id drover-usage-test-a
                                           :updated now)))
      (setq drover-test-sessions (list session))
      (drover-usage-scan)
      (should (equal (drover-usage-session-text session 'column) "$0.01"))
      (should (string-prefix-p "$0.01 · 16 in, 109 out" (drover-usage-session-text session 'details)))
      (let ((drover-group-by nil)
            (drover-list-hidden-fields '(host harness location branch agent model provider context
                                                   created age cache summary)))
        (save-window-excursion
          (drover-list)
          (with-current-buffer (drover-list--buffer)
            (should (string-match-p "\\$0\\.01" (buffer-string)))
            (let ((drawn (drover-list--model-rows drover-list--model)))
              (should (gethash '(fake . "a") drawn)))
            ;; A dollar more, read from the transcript: the row is drawn again.
            (drover-fixture-claude-append
             file (drover-usage-test-reply drover-usage-test-a directory (- now 5) "msg_8"
                                           '(:input 0 :output 40000)))
            (drover-usage-scan)
            (drover-usage--notify)
            (drover-changed-now)
            (should (string-match-p "\\$1\\.01" (buffer-string))))
          (kill-buffer (drover-list--buffer)))))))

(ert-deftest drover-usage-test-prints-in-proportion ()
  "What the usage keeps prints in proportion to what it read."
  (skip-unless (sqlite-available-p))
  (drover-usage-test-with
    (let* ((now (float-time))
           (db (drover-usage-test-oc now)))
      (unwind-protect
          (progn
            (drover-usage-test-claude now)
            (drover-usage-scan)
            (drover-usage-totals 'all)
            (drover-usage-session-totals (drover-session-create :backend 'opencode :id "ses_a"
                                                                :engine 'opencode :engine-id "ses_a"))
            (drover-test-prints-in-proportion
             10 (list (cons "a fact" (car (drover-usage--facts-facts (drover-usage--current-facts)))))))
        (sqlite-close db)))))

;;;; Other hosts

(ert-deftest drover-usage-test-wire ()
  "Totals travel as rows of the dimensions asked; what comes is checked and bounded."
  (drover-usage-test-with
    (drover-usage-test-claude (float-time))
    (drover-usage-scan)
    (let* ((reply (drover-usage-remote-answer '(:period "all" :by ["session"])))
           (rows (append (plist-get reply :rows) nil))
           (facts (mapcar (lambda (row) (drover-usage-decode-row row "core")) rows)))
      (should (eq (plist-get reply :reading) :false))
      (should (= (length rows) 3))
      (should (equal (sort (mapcar #'drover-usage--fact-id facts) #'string<)
                     (sort (list drover-usage-test-a (concat drover-usage-test-a "/agent-x") drover-usage-test-b)
                           #'string<)))
      (should (equal (apply #'+ (mapcar #'drover-usage--fact-output facts)) 113)))
    (should-not (drover-usage-decode-row [1 2 3] "core"))
    (should-not (drover-usage-decode-row ["claude-code" "x" "" 0 "" "" "" -1 0 0 0 :false "priced" "" ""] "core"))
    (should-not (drover-usage-decode-row ["claude-code" "x" "" 0 "" "" "" 1 0 0 0 :false "free" "" ""] "core"))
    (let ((fact (drover-usage-decode-row
                 (vector "Not An Engine" (make-string 5000 ?i) "" 20261104 "p" (make-string 500000 ?m) ""
                         1 2 3 4 1.5 "priced" "its prices" "")
                 "core")))
      (should fact)
      (should-not (drover-usage--fact-harness fact))
      (should (<= (length (drover-usage--fact-id fact)) 201))
      (should (<= (length (drover-usage--fact-model fact)) 201))
      (should (= (drover-usage--fact-cost fact) 1.5)))))

(provide 'drover-usage-test)
;;; drover-usage-test.el ends here
