;;; drover-search-test.el --- Tests for drover-search  -*- lexical-binding: t; -*-

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

;; The search of sessions: what it matches as keys are typed, scoped by
;; the view settings, in the list's order; the backends searching what
;; was said, debounced, cancelled, bounded and merged; Claude Code's
;; transcripts and Codex's rollouts searched with ripgrep, the real one
;; when installed, and scripts standing for it.  The minibuffer is not
;; read: the completion table and style are called as a completion UI
;; calls them, and `completing-read' is stubbed for the command.

;;; Code:

(require 'drover-test-helpers)
(require 'drover-transcript-fixtures)
(require 'drover-list)
(require 'drover-search)

(defmacro drover-search-test-with (sessions &rest body)
  "Run BODY with the fake backend serving SESSIONS, no search in progress.
The list's defaults: one group, last activity first."
  (declare (indent 1) (debug t))
  `(drover-test-with-backend ,sessions
     (let ((drover-group-by nil)
           (drover-sort-by 'updated)
           (drover-sort-descending t)
           (drover-search--current nil)
           (drover-search--programmatic nil)
           (drover-search-delay 0.05)
           (drover-search-text-limit 200))
       (unwind-protect
           (progn ,@body)
         (dolist (state (list drover-search--current drover-search--programmatic))
           (when state
             (drover-search--finish state)))))))

(defun drover-search-test-ids (query &optional state)
  "Return the identifiers of the sessions QUERY matches, in order.
In STATE, by default a search made now."
  (mapcar (lambda (entry) (drover-session-id (drover-search--entry-session entry)))
          (drover-search-entries (or state (drover-search--build)) query)))

(defun drover-search-test-candidates (query)
  "Return the candidates a completion UI gets for QUERY, as `drover-search' sets it up."
  (let ((completion-styles '(drover-search))
        (completion-category-overrides (cons '(drover-session (styles drover-search))
                                             completion-category-overrides)))
    (let ((all (completion-all-completions query #'drover-search--table nil (length query))))
      (when (consp all)
        (setcdr (last all) nil))
      (mapcar #'substring-no-properties all))))

(defun drover-search-test-wait (predicate &optional seconds)
  "Wait for PREDICATE to return non-nil, SECONDS at most, 5 by default."
  (let ((deadline (+ (float-time) (or seconds 5))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (funcall predicate)))

(defun drover-search-test-sessions ()
  "Return sessions of every kind of field, a subagent among them."
  (list (drover-test-session "a" :status 'idle :updated 100.0 :project "drover" :branch "main"
                             :agent "build" :model "Opus 5.5" :provider-name "Anthropic")
        (drover-test-session "b" :status 'waiting :updated 300.0 :project "spyglass" :branch "search"
                             :agent "plan" :model "GPT-5.5" :provider-name "OpenAI"
                             :requests '((:kind permission :summary "run the zebra tests")))
        (drover-test-session "c" :status 'done :updated 200.0 :project "drover" :branch "sidebar"
                             :agent "build" :model "Opus 5.5" :provider-name "Anthropic"
                             :detail "Finished the Zebra crossing")
        (drover-test-session "c1" :parent "c" :status 'idle :updated 150.0 :project "drover"
                             :agent "explore")))

;;;; What keys typed match

(ert-deftest drover-search-test-matches-what-sessions-tell ()
  "Every word typed, any case, is in the title, summary, project or branch."
  (drover-search-test-with (drover-search-test-sessions)
    (should (equal (drover-search-test-ids "") '("b" "c" "a")))
    (should (equal (drover-search-test-ids "session") '("b" "c" "a")))
    (should (equal (drover-search-test-ids "ZEBRA") '("b" "c")))
    (should (equal (drover-search-test-ids "zebra tests") '("b")))
    (should (equal (drover-search-test-ids "tests zebra") '("b")))
    (should (equal (drover-search-test-ids "spyglass") '("b")))
    (should (equal (drover-search-test-ids "sidebar") '("c")))
    (should (equal (drover-search-test-ids "drover main") '("a")))
    (should-not (drover-search-test-ids "nowhere"))
    ;; A hidden subagent matching shows, its parent not matching.
    (should (equal (drover-search-test-ids "session c1") '("c1")))
    ;; Its parent matching, it stays hidden, as in the list.
    (should (equal (drover-search-test-ids "drover session") '("c" "a")))
    (should (equal (drover-search-test-ids "c1") '("c1")))))

(ert-deftest drover-search-test-hidden-subagents ()
  "A subagent the list hides shows when it matches and no session above it does."
  (drover-search-test-with
      (list (drover-test-session "p" :title "Parent" :updated 10.0)
            (drover-test-session "s" :title "Child task" :parent "p" :updated 9.0)
            (drover-test-session "q" :title "Other task" :updated 8.0)
            (drover-test-session "t" :title "Other child task" :parent "q" :updated 7.0))
    (setq drover-test-sessions
          (mapcar (lambda (session)
                    (setf (drover-session-title session)
                          (pcase (drover-session-id session)
                            ("p" "Parent") ("s" "Child task") ("q" "Other task") (_ "Other child task")))
                    session)
                  drover-test-sessions))
    (should (equal (drover-search-test-ids "") '("p" "q")))
    (should (equal (drover-search-test-ids "child") '("s" "t")))
    ;; "Other" matches q: its subagent stays hidden, as the list hides it.
    (should (equal (drover-search-test-ids "other") '("q")))
    (let ((drover-show-subagents 'all))
      (should (equal (drover-search-test-ids "") '("p" "s" "q" "t")))
      (should (equal (drover-search-test-ids "other") '("q" "t"))))))

(ert-deftest drover-search-test-hidden-orphan ()
  "A subagent atop a tree of its own, hidden, is found by its words, filter or not.
Its parent not listed, the list hides it under a filter as without one,
and the search offers it once it matches."
  (drover-search-test-with
      (list (drover-test-session "a" :title "Alpha" :updated 10.0)
            (drover-test-session "o" :title "Lonely helper" :parent "gone" :status 'idle :updated 9.0))
    (dolist (filter '(nil (:host (:exclude "elsewhere"))))
      (let ((drover-filter filter))
        (should (equal (drover-search-test-ids "") '("a")))
        (should (equal (drover-search-test-ids "lonely") '("o")))))))

(ert-deftest drover-search-test-search-texts ()
  "The texts backends tell for searching match, those of recording backends too."
  (drover-search-test-with (list (drover-test-session "a" :engine 'claude-code :engine-id "conv-a")
                                 (drover-test-session "b"))
    (drover-test-register-simple 'recorder nil
                                 :recorded '(claude-code)
                                 :search-text (lambda (session)
                                                (when (equal (drover-session-engine-id session) "conv-a")
                                                  (list "Fix the giraffe redirect"
                                                        (concat "last " (make-string 500000 ?x))))))
    (should (equal (drover-search-test-ids "giraffe") '("a")))
    (should (equal (drover-search-test-ids "last") '("a")))
    ;; Bounded as taken in.
    (should (< (length (drover-search--entry-haystack
                        (gethash '(fake . "a") (drover-search--state-keys (drover-search--build)))))
               (* 3 drover-line-limit)))
    ;; A backend's own.
    (drover-register-backend 'fake :label "Fake" :sessions (lambda () drover-test-sessions)
                             :search-text (lambda (session)
                                            (and (equal (drover-session-id session) "b") '("okapi"))))
    (should (equal (drover-search-test-ids "okapi") '("b")))))

(ert-deftest drover-search-test-scoped-by-filter ()
  "Each member of the filter scopes the search as it scopes the list."
  (let ((now (float-time)))
    (drover-search-test-with
        (list (drover-test-session "a" :status 'idle :updated (- now 10) :project "drover" :agent "build"
                                   :model "Opus 5.5" :provider-name "Anthropic")
              (drover-test-session "b" :status 'waiting :updated (- now 20) :project "spyglass"
                                   :agent "plan" :model "GPT-5.5" :provider-name "OpenAI")
              (drover-test-session "c" :status 'done :updated (- now 400000) :project "drover"
                                   :agent "build" :model "Opus 5.5" :provider-name "Anthropic"))
      (drover-test-register-simple 'other
                                   (list (drover-session-create :backend 'other :id "o" :title "Session o"
                                                                :updated (- now 30) :host "core")))
      (dolist (case `((:project "drover" ("a" "c"))
                      (:statuses (waiting done) ("b" "c"))
                      (:agent "plan" ("b"))
                      (:model "Opus 5.5" ("a" "c"))
                      (:provider "OpenAI" ("b"))
                      (:harness other ("o"))
                      (:host ("core") ("o"))
                      (:host (local) ("a" "b" "c"))
                      (:since ,(- now 100) ("a" "b" "o"))
                      (:search "spy" ("b"))))
        (pcase-let ((`(,key ,value ,expected) case))
          (ert-info ((format "%s %S" key value))
            (let ((drover-filter (list key value)))
              (should (equal (drover-search-test-ids "") expected))
              (should (equal (drover-search-test-ids "session") expected))
              ;; Within the scope only.
              (should (equal (drover-search-test-ids "session b") (and (member "b" expected) '("b")))))))))))

(ert-deftest drover-search-test-older-and-archived ()
  "Older and archived sessions are searched while the list shows them."
  (drover-search-test-with (list (drover-test-session "live" :updated 50.0))
    (drover-register-backend 'fake :label "Fake" :sessions (lambda () drover-test-sessions)
                             :history (lambda (callback)
                                        (funcall callback (list (drover-test-session "old" :updated 5.0))))
                             :archived (lambda (callback)
                                         (funcall callback (list (drover-test-session "gone" :updated 9.0
                                                                                      :archived 1.0)))))
    (should (equal (drover-search-test-ids "session") '("live")))
    (drover-set-show-history t)
    (should (equal (drover-search-test-ids "session") '("live" "old")))
    (drover-set-show-archived t)
    (should (member "gone" (drover-search-test-ids "session")))
    (drover-set-show-history nil)
    (drover-set-show-archived nil)
    (should (equal (drover-search-test-ids "session") '("live")))))

(ert-deftest drover-search-test-sorted-like-the-list ()
  "Candidates come in the order the list draws its sessions, groups included."
  (let ((sessions (mapcar (lambda (n)
                            (drover-test-session (format "s%02d" n)
                                                 :status (nth (% n 4) '(idle waiting done working))
                                                 :updated (float (* 7 (% (* n 13) 31)))
                                                 :project (format "p%d" (% n 3))))
                          (number-sequence 1 30))))
    (drover-search-test-with sessions
      (dolist (settings '((nil updated t) (status urgency nil) (project title nil) (nil title t)))
        (pcase-let ((`(,group ,sort ,descending) settings))
          (let ((drover-group-by group)
                (drover-sort-by sort)
                (drover-sort-descending descending))
            (ert-info ((format "%S" settings))
              (should (equal (drover-search-test-ids "")
                             (mapcar #'drover-session-id
                                     (mapcan (lambda (group) (drover-tree-sessions (cdr group)))
                                             (drover-view-groups))))))))))))

(ert-deftest drover-search-test-completion ()
  "A completion UI gets the candidates matched as a whole, grouped and annotated."
  (drover-search-test-with (drover-search-test-sessions)
    (setq drover-search--current (drover-search--build))
    ;; Matching the project, which no title holds.
    (should (equal (drover-search-test-candidates "spyglass") '("Session b")))
    (should (equal (drover-search-test-candidates "zebra") '("Session b" "Session c")))
    (should (equal (try-completion "spyglass" #'drover-search--table) "Session b"))
    (should (eq (test-completion "Session b" #'drover-search--table) t))
    (should-not (test-completion "zebra" #'drover-search--table))
    ;; Words typed highlighted.
    (let* ((completion-lazy-hilit nil)
           (completion-category-overrides '((drover-session (styles drover-search))))
           (name (car (completion-all-completions "sess" #'drover-search--table nil 4))))
      (should (eq (get-text-property 0 'face name) 'completions-common-part)))
    (let ((metadata (completion-metadata "" #'drover-search--table nil)))
      (should (eq (completion-metadata-get metadata 'category) 'drover-session))
      (should (eq (completion-metadata-get metadata 'display-sort-function) #'identity))
      (should-not (completion-metadata-get metadata 'group-function))
      (let ((drover-group-by 'status))
        (should (completion-metadata-get (completion-metadata "" #'drover-search--table nil)
                                         'group-function))))
    (pcase-let ((`((,name ,prefix ,suffix)) (drover-search--affixation '("Session b"))))
      (should (equal name "Session b"))
      (should (string-search (opencode-session-status-glyph 'waiting) prefix))
      (should (string-search "spyglass:search" suffix))
      (should (string-search "run the zebra tests" suffix)))
    ;; A subagent has an arrow.
    (should (string-search "↳" (nth 1 (car (drover-search--affixation '("Session c1"))))))))

(ert-deftest drover-search-test-annotations-bounded ()
  "Titles, summaries, prompts and snippets of 500 kB annotate bounded."
  (let ((huge (make-string 500000 ?z)))
    (drover-search-test-with (list (drover-test-session "a" :detail (concat "zz " huge) :project huge
                                                        :branch huge))
      (setf (drover-session-title (car drover-test-sessions)) (concat "zz " huge))
      (setq drover-test-sessions (mapcar #'drover--bound-record drover-test-sessions))
      (setq drover-search--current (drover-search--build))
      (puthash '(fake . "a")
               (list :snippet (drover-bound-line (concat "zz " huge) drover-search-snippet-length)
                     :count 3)
               (drover-search--state-hits drover-search--current))
      (let ((candidates (drover-search-test-candidates "zz")))
        (should (= (length candidates) 1))
        (should (<= (length (car candidates)) (1+ drover-line-limit)))
        (pcase-let ((`((,_ ,prefix ,suffix)) (drover-search--affixation candidates)))
          (should (< (length prefix) 20))
          (should (< (length suffix) 400))
          (should (string-search "(+2)" suffix)))))))

(ert-deftest drover-search-test-unique-names ()
  "Sessions of one title are told apart, and each name chooses its own."
  (drover-search-test-with (list (drover-test-session "a" :updated 2.0) (drover-test-session "b" :updated 1.0))
    (dolist (session drover-test-sessions)
      (setf (drover-session-title session) "Same"))
    (let* ((state (drover-search--build))
           (names (mapcar #'drover-search--entry-name (drover-search-entries state ""))))
      (should (equal (mapcar #'substring-no-properties names) '("Same (a)" "Same (b)")))
      (should (equal (drover-session-id (drover-search--entry-session (drover-search--chosen state "Same (b)")))
                     "b"))
      ;; A text typed chooses its first match.
      (should (equal (drover-session-id (drover-search--entry-session (drover-search--chosen state "same")))
                     "a")))))

(ert-deftest drover-search-test-follows-changes ()
  "A session changing takes its record in place; one coming makes the candidates anew."
  (drover-search-test-with (drover-search-test-sessions)
    (drover-index)
    (let ((state (drover-search--build)))
      (setq drover-search--current state)
      (add-hook 'drover-changed-hook #'drover-search--changed)
      (should-not (drover-search-test-ids "ocelot" state))
      (setq drover-test-sessions
            (mapcar (lambda (session)
                      (if (equal (drover-session-id session) "a")
                          (drover-test-session "a" :status 'done :updated 100.0 :project "drover"
                                               :detail "Saw an ocelot")
                        session))
                    drover-test-sessions))
      (drover-backend-changed 'fake "a")
      (drover-changed-now)
      (should (equal (drover-search-test-ids "ocelot" state) '("a")))
      (should-not (drover-search--state-rebuild-timer state))
      (setq drover-test-sessions (append drover-test-sessions
                                         (list (drover-test-session "d" :updated 400.0 :detail "ocelot too"))))
      (drover-backend-changed 'fake "d")
      (drover-changed-now)
      (should (equal (drover-search-test-ids "ocelot" state) '("a")))
      (should (timerp (drover-search--state-rebuild-timer state)))
      (drover-search--rebuild)
      (should (equal (drover-search-test-ids "ocelot" state) '("d" "a"))))))

;;;; The command

(ert-deftest drover-search-test-command ()
  "RET opens the session chosen, C-l shows its row, C-' opens the menu with the text."
  (drover-search-test-with (drover-search-test-sessions)
    (let (prompt)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (string table &rest _)
                   (setq prompt string)
                   (should drover-search--current)
                   (car (all-completions "spyglass" table)))))
        (drover-search)
        (should (equal (car drover-test-calls) '(:open "b" nil)))
        (should-not drover-search--current)
        (should (equal prompt "Search sessions: "))
        (let ((drover-filter '(:project "drover")))
          (drover-search nil)
          (should (string-search "project drover" prompt)))
        ;; What the view settings of its menu set, the subagents shown too.
        (let ((drover-filter '(:project "drover"))
              (drover-show-subagents 'all)
              (drover-show-history t)
              (drover-show-archived t))
          (should (equal (drover-search--prompt)
                         "Search sessions (project drover, +subagents, +older, +archived): ")))
        (let ((drover-show-subagents 'none))
          (should (equal (drover-search--prompt) "Search sessions (-subagents): ")))))
    ;; Nothing matching.
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "nowhere")))
      (should-error (drover-search) :type 'user-error))
    ;; The menu, with the text typed.
    (let (menu)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) (setq drover-search--exit 'scope) "zeb"))
                ((symbol-function 'drover-search-menu) (lambda (input) (setq menu input))))
        (drover-search)
        (should (equal menu "zeb"))))
    ;; The row in the list.
    (let ((buffer (drover-list--buffer)))
      (unwind-protect
          (save-window-excursion
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_ table &rest _)
                         (setq drover-search--exit 'locate)
                         (car (all-completions "zebra crossing" table)))))
              (drover-search)
              (should (eq (current-buffer) buffer))
              (should (equal (drover-session-id (drover-session-at-point)) "c"))))
        (kill-buffer buffer)))))

(ert-deftest drover-search-test-keys ()
  "S searches; in its minibuffer, C-' and the menu's key open its menu, C-l shows the row."
  (should (eq (plist-get (cdr (transient-get-suffix 'drover-dispatch "S")) :command) 'drover-search))
  (should (eq (keymap-lookup drover-search-minibuffer-map "C-'") #'drover-search-scope))
  (should (eq (keymap-lookup drover-search-minibuffer-map "C-l") #'drover-search-locate))
  (should (eq (keymap-lookup drover-search-minibuffer-map "<remap> <drover-dispatch>")
              #'drover-search-scope))
  ;; From the menu, the keys that left the minibuffer search again.
  (let ((drover-search--menu-keys (list (kbd "C-'") (kbd "<f19>"))))
    (should (equal (mapcar (lambda (entry) (list (plist-get (cdr entry) :key) (plist-get (cdr entry) :command)))
                           (drover-search--menu-return-entries nil))
                   '(("C-'" drover-search-resume) ("<f19>" drover-search-resume))))))

;;;; Searching what was said

(defvar drover-search-test-runs nil
  "The searches the fake source was asked, newest first: (QUERY CALLBACK CANCELLED).")

(defun drover-search-test-source (query _scope callback)
  "A fake :search, keeping QUERY and CALLBACK, and a cancel noting it."
  (let ((run (list query callback nil)))
    (push run drover-search-test-runs)
    (lambda () (setf (nth 2 run) t))))

(defmacro drover-search-test-with-source (sessions &rest body)
  "Run BODY with SESSIONS of the fake backend, searched by the fake source."
  (declare (indent 1) (debug t))
  `(drover-search-test-with ,sessions
     (let ((drover-search-test-runs nil))
       (drover-test-register-simple 'texts nil :search #'drover-search-test-source)
       ,@body)))

(ert-deftest drover-search-test-debounced-and-cancelled ()
  "The backends are asked once typing pauses, the run before cancelled, its answers dropped."
  (drover-search-test-with-source (drover-search-test-sessions)
    (setq drover-search--current (drover-search--build))
    (dolist (query '("z" "ze" "zeb" "zebr"))
      (drover-search-test-candidates query))
    (should-not drover-search-test-runs)
    (should (drover-search-test-wait (lambda () drover-search-test-runs)))
    (should (equal (mapcar #'car drover-search-test-runs) '("zebr")))
    ;; Matches arrive, the candidates follow.
    (let ((first (car drover-search-test-runs)))
      (funcall (nth 1 first) (list (list :key '(fake . "a") :snippet "a zebra said")))
      (should (equal (drover-search-test-candidates "zebr") '("Session b" "Session c" "Session a")))
      ;; Typing on cancels the run; its late answers are dropped.
      (drover-search-test-candidates "zebra")
      (should (nth 2 first))
      ;; The match still holds for the text typed further.
      (should (equal (drover-search-test-candidates "zebra") '("Session b" "Session c" "Session a")))
      (funcall (nth 1 first) (list (list :key '(fake . "c1") :snippet "zebra")))
      (should-not (gethash '(fake . "c1") (drover-search--state-hits drover-search--current)))
      (should (drover-search-test-wait (lambda () (= (length drover-search-test-runs) 2))))
      (should (equal (car (car drover-search-test-runs)) "zebra"))
      ;; A text the match lacks forgets it.
      (drover-search-test-candidates "zebu")
      (should-not (gethash '(fake . "a") (drover-search--state-hits drover-search--current)))
      ;; Too short, nothing asked, "zebu" typed past before its pause.
      (drover-search-test-candidates "ze")
      (sleep-for (* 3 drover-search-delay))
      (should (= (length drover-search-test-runs) 2)))))

(ert-deftest drover-search-test-merged-and-bounded ()
  "Matches merge by session, whatever backend follows it, and are bounded."
  (let ((sessions (cons (drover-test-session "front" :engine 'claude-code :engine-id "conv")
                        (mapcar (lambda (n) (drover-test-session (format "s%03d" n) :updated (float n)))
                                (number-sequence 1 300)))))
    (drover-search-test-with-source sessions
      (let ((state (drover-search--build))
            (result nil))
        (setq drover-search--programmatic state)
        (setf (drover-search--state-done state) (lambda (_) (setq result t)))
        (drover-search--start state "word")
        (let ((callback (nth 1 (car drover-search-test-runs))))
          (funcall callback (list (list :engine 'claude-code :id "conv" :snippet "first")
                                  (list :engine 'claude-code :id "conv" :snippet "second")
                                  (list :engine 'claude-code :id "elsewhere" :snippet "out")
                                  (list :key '(fake . "s001") :snippet (make-string 500000 ?w))))
          (let ((hit (gethash '(fake . "front") (drover-search--state-hits state))))
            (should (equal (plist-get hit :snippet) "first"))
            (should (= (plist-get hit :count) 2)))
          (should (= (drover-search--state-outside state) 1))
          (should (<= (length (plist-get (gethash '(fake . "s001") (drover-search--state-hits state)) :snippet))
                      (1+ drover-search-snippet-length)))
          ;; Past the limit, the run stops.
          (funcall callback (mapcar (lambda (n) (list :key (cons 'fake (format "s%03d" (1+ (% n 300))))
                                                      :snippet "w"))
                                    (number-sequence 1 1000)))
          (should (= (drover-search--state-received state) drover-search-text-limit))
          (should (nth 2 (car drover-search-test-runs)))
          (should result)
          (should (<= (hash-table-count (drover-search--state-hits state)) drover-search-text-limit)))))))

(ert-deftest drover-search-test-failing-source ()
  "A backend failing is told once, and asked no more in this search."
  (drover-search-test-with (drover-search-test-sessions)
    (let ((calls 0)
          (messages nil))
      (drover-test-register-simple 'broken nil
                                   :search (lambda (&rest _)
                                             (cl-incf calls)
                                             (error "Broken: %s" (make-string 100000 ?x))))
      (drover-test-register-simple 'refusing nil
                                   :search (lambda (_query _scope callback)
                                             (funcall callback (list :failure (make-string 100000 ?y)))
                                             #'ignore))
      (cl-letf (((symbol-function 'message)
                 (lambda (format &rest args) (push (apply #'format-message format args) messages))))
        (let ((state (drover-search--build)))
          (drover-search--start state "first")
          (drover-search--start state "second")
          (should (= calls 1))
          (should (= (length messages) 2))
          (should (seq-every-p (lambda (message) (< (length message) 400)) messages))
          (should (equal (sort (mapcar #'car (drover-search--state-failed state))) '(broken refusing)))
          (should (string-search "Broken failed" (drover-search--status-text state)))
          (should-not (drover-search--state-runs state)))))))

(ert-deftest drover-search-test-text-programmatic ()
  "`drover-search-text' gives the sessions found, in order, once every backend finished."
  (drover-search-test-with-source (drover-search-test-sessions)
    (let ((result nil))
      (drover-search-text "zebra" (lambda (found) (setq result found)))
      (should-not result)
      (funcall (nth 1 (car drover-search-test-runs))
               (list (list :key '(fake . "a") :snippet "zebra one")
                     (list :key '(fake . "b") :snippet "zebra two")))
      (funcall (nth 1 (car drover-search-test-runs)) 'done)
      (should (equal (mapcar (lambda (entry) (drover-session-id (car entry))) (plist-get result :sessions))
                     '("b" "a")))
      (should (equal (nth 1 (car (plist-get result :sessions))) "zebra two"))
      ;; What it returns prints as little, whatever the sessions.
      (let ((cancel (drover-search-text "zebra" #'ignore)))
        (should (drover-test-printed-length cancel 2000))
        (funcall cancel)
        (should-not drover-search--programmatic)))))

(ert-deftest drover-search-test-prints-in-proportion ()
  "What a search keeps prints in proportion to its sessions."
  (let ((sessions (mapcar (lambda (n) (drover-test-session (format "s%02d" n) :updated (float n)))
                          (number-sequence 1 50))))
    (drover-search-test-with-source sessions
      (setq drover-search--current (drover-search--build))
      (drover-search-test-candidates "session")
      (drover-search-test-wait (lambda () drover-search-test-runs))
      (funcall (nth 1 (car drover-search-test-runs)) (list (list :key '(fake . "s01") :snippet "x")))
      (drover-test-prints-in-proportion
       50 (list (cons "a receiver" (nth 1 (car drover-search-test-runs)))
                (cons "a scope" (drover-search--scope drover-search--current)))))))

;;;; Transcripts and rollouts

(defconst drover-search-test-claude-a "11111111-1111-4111-8111-111111111111")
(defconst drover-search-test-claude-b "22222222-2222-4222-8222-222222222222")
(defconst drover-search-test-codex-a "019f0000-0000-7000-8000-0000000000aa")

(defun drover-search-test-tool-lines (id directory time text)
  "Return a tool call and its result of conversation ID saying TEXT, at TIME."
  (list (drover-fixture-line
         :parentUuid "x" :isSidechain :false
         :message (list :model "claude-opus-5-5" :id "msg_t" :type "message" :role "assistant"
                        :content (vector (list :type "tool_use" :id "toolu_1" :name "Bash"
                                               :input (list :command (concat "grep " text)))))
         :type "assistant" :uuid "t1" :timestamp (drover-fixture-time time) :cwd directory :sessionId id)
        (drover-fixture-line
         :parentUuid "t1" :isSidechain :false :type "user"
         :message (list :role "user"
                        :content (vector (list :tool_use_id "toolu_1" :type "tool_result"
                                               :content (concat text " in the output"))))
         :uuid "t2" :timestamp (drover-fixture-time time) :cwd directory :sessionId id)))

(defmacro drover-search-test-with-transcripts (&rest body)
  "Run BODY with Claude Code's and Codex's backends over conversations of fixtures.
Claude Code's A said \"quokka\" in its prompt and \"narwhal\" in a reply,
B only in tool calls and results; Codex's A \"axolotl\" in its prompt.
`directory' is where they ran."
  (declare (indent 0) (debug t))
  `(drover-test-with-homes
     (drover-search-test-with nil
       (let ((drover-claude-code-transcripts--users nil)
             (drover-claude-code-transcripts--index nil)
             (drover-claude-code-transcripts--agents (make-hash-table :test #'equal))
             (drover-claude-code-agents-interval nil)
             (drover-claude-code-history-days nil)
             (drover-codex-rollouts--users nil)
             (drover-codex-rollouts--index nil)
             (drover-codex-rollouts--names nil)
             (drover-codex-program (expand-file-name "no-codex" home))
             (drover-codex-history-days nil)
             (directory (file-name-as-directory (expand-file-name "work" home)))
             (start (- (float-time) 3600)))
         (setq drover--backends nil)
         (drover-claude-code-transcripts-install 'test)
         (drover-codex-rollouts-install 'test)
         (unwind-protect
             (progn
               (drover-fixture-claude drover-search-test-claude-a directory :start start
                                      :prompt "Teach the quokka\nto say \"hello\""
                                      :reply "The narwhal agrees: done.")
               (let ((file (drover-fixture-claude drover-search-test-claude-b directory :start (+ start 10)
                                                  :prompt "List the files" :reply "Here they are."
                                                  :filler 300000)))
                 (apply #'drover-fixture-claude-append file
                        (drover-search-test-tool-lines drover-search-test-claude-b directory (+ start 20)
                                                       "narwhal quokka axolotl")))
               (drover-fixture-codex drover-search-test-codex-a directory :start (+ start 30)
                                     :prompt "Find the axolotl" :reply "The axolotl is in src.")
               (drover-transcript-scan drover-claude-code-transcripts--index)
               (drover-transcript-scan drover-codex-rollouts--index)
               (drover-changed-now)
               ,@body)
           (drover-claude-code-transcripts-uninstall 'test)
           (drover-codex-rollouts-uninstall 'test))))))

(defun drover-search-test-text (query)
  "Return what `drover-search-text' finds for QUERY: (ID SNIPPET) of each session."
  (let ((result nil))
    (drover-search-text query (lambda (found) (setq result found)))
    (should (drover-search-test-wait (lambda () result) 10))
    (mapcar (lambda (entry) (list (drover-session-id (car entry)) (nth 1 entry)))
            (plist-get result :sessions))))

(ert-deftest drover-search-test-prompts-match-as-typed ()
  "The first and last prompts of Claude Code's and Codex's conversations match as typed."
  (drover-search-test-with-transcripts
    (should (equal (mapcar #'drover-session-id (drover-search-sessions "quokka"))
                   (list drover-search-test-claude-a)))
    (should (equal (mapcar #'drover-session-id (drover-search-sessions "axolotl"))
                   (list drover-search-test-codex-a)))
    ;; Replies are said, not told: only the backends' search finds them.
    (should-not (drover-search-sessions "narwhal"))))

(ert-deftest drover-search-test-ripgrep-transcripts ()
  "ripgrep finds what was said in prompts and replies, not in tool calls nor their results."
  (skip-unless (executable-find "rg"))
  (drover-search-test-with-transcripts
    (should (equal (drover-search-test-text "narwhal")
                   (list (list drover-search-test-claude-a "The narwhal agrees: done."))))
    ;; Escapes decoded, words across a line break.
    (should (equal (drover-search-test-text "quokka to say")
                   (list (list drover-search-test-claude-a "Teach the quokka to say \"hello\""))))
    (should (equal (drover-search-test-text "AXOLOTL")
                   (list (list drover-search-test-codex-a "Find the axolotl"))))
    (should-not (drover-search-test-text "in the output"))
    (should-not (drover-search-test-text "grep"))
    ;; Scoped: a filter leaving Codex out leaves its rollouts unsearched.
    (let ((drover-filter '(:harness claude-code)))
      (should-not (drover-search-test-text "axolotl")))
    ;; Files past what one command takes, as on Windows, are searched by
    ;; the runs after: here a file a run.
    (let ((drover-transcript-search-arguments-limit 10)
          (runs 0)
          (make-process (symbol-function 'make-process)))
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest arguments) (cl-incf runs) (apply make-process arguments))))
        (should (equal (drover-search-test-text "narwhal")
                       (list (list drover-search-test-claude-a "The narwhal agrees: done.")))))
      (should (> runs 2)))))

(ert-deftest drover-search-test-ripgrep-huge-lines ()
  "A line of 500 kB, a match at its end, gives a snippet bounded."
  (skip-unless (executable-find "rg"))
  (drover-search-test-with-transcripts
    (drover-fixture-claude-append
     (drover-fixture-claude-file drover-search-test-claude-a directory)
     (drover-fixture-claude-assistant drover-search-test-claude-a directory (+ start 40)
                                      (concat (make-string 500000 ?a) " the pangolin " (make-string 500000 ?b))))
    (drover-transcript-scan drover-claude-code-transcripts--index)
    (pcase-let ((`((,id ,snippet)) (drover-search-test-text "pangolin")))
      (should (equal id drover-search-test-claude-a))
      (should (string-search "pangolin" snippet))
      (should (<= (length snippet) (1+ drover-search-snippet-length))))))

(defun drover-search-test-script (home body)
  "Write an executable shell script of BODY in HOME, standing for ripgrep, and return it."
  (let ((file (expand-file-name "fake-rg" home)))
    (with-temp-file file
      (insert "#!/bin/sh\n" body "\n"))
    (set-file-modes file #o755)
    file))

(ert-deftest drover-search-test-ripgrep-scripts ()
  "Without ripgrep, the search is told once and matches what sessions tell; its errors are told."
  (skip-unless (memq system-type '(darwin gnu/linux berkeley-unix)))
  (drover-search-test-with-transcripts
    (let ((messages nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (format &rest args) (push (apply #'format-message format args) messages))))
        ;; Missing.
        (let ((drover-transcript-ripgrep (expand-file-name "no-rg" home))
              (drover-transcript--ripgrep-told nil))
          (should-not (drover-search-test-text "narwhal"))
          (should-not (drover-search-test-text "narwhal"))
          (should (= (seq-count (lambda (message) (string-search "is not found" message)) messages) 1)))
        ;; Failing.
        (setq messages nil)
        (let ((drover-transcript-ripgrep
               (drover-search-test-script home (format "echo 'rg: regex parse error: %s' >&2; exit 2"
                                                       (make-string 2000 ?e)))))
          (let ((result nil))
            (drover-search-text "narwhal" (lambda (found) (setq result found)))
            (should (drover-search-test-wait (lambda () result)))
            (should (= (length (plist-get result :failed)) 2))
            (should (seq-every-p (lambda (failure) (string-search "regex parse error" (cdr failure)))
                                 (plist-get result :failed)))
            (should (seq-every-p (lambda (message) (< (length message) 400)) messages))))
        ;; Output streamed in pieces, a line split across them.
        (let ((drover-transcript-ripgrep
               (drover-search-test-script
                home
                (format "for f in \"$@\"; do case \"$f\" in *%s*) printf '%%s\\0' \"$f\"; sleep 0.05; printf '7:the \\\\\"walrus\\\\\" said\\n';; esac; done"
                        drover-search-test-claude-a))))
          (should (equal (drover-search-test-text "walrus")
                         (list (list drover-search-test-claude-a "the \"walrus\" said")))))))))

(ert-deftest drover-search-test-no-block ()
  "The backends' search never keeps Emacs from its timers more than a few milliseconds.
Over 120 conversations of fixtures, some of megabytes, the longest any
function of the search runs, from timers, the process or the command,
stays under 5 ms, starting one run of ripgrep per backend at most, and
a timer ticking every hundredth of a second never waits much longer.
Starting a process is the system's work, as collecting garbage is the
whole Emacs's: well under a millisecond on macOS, 7 to 17 ms on Windows,
it is not counted.  Nor is waiting for a processor: the best of three
runs counts.  `make bench-search' measures it to the millisecond."
  (skip-unless (executable-find "rg"))
  (drover-search-test-with-transcripts
    (dotimes (n 120)
      (drover-fixture-claude (format "33333333-3333-4333-8333-%012d" n) directory :start (+ start n)
                             :prompt (format "Task %d about the capybara" n)
                             :reply (format "Reply %d: %s" n (if (zerop (% n 10)) (make-string 300000 ?r) "short"))
                             :filler (if (zerop (% n 7)) 400000 0)))
    (drover-transcript-scan drover-claude-code-transcripts--index)
    (drover-changed-now)
    ;; Ripgrep found as the first search finds it, once per Emacs: 7 to 11
    ;; ms on Windows, looking in each directory with each suffix.
    (should (drover-transcript--ripgrep))
    (let ((longest 0.0)
          (slowest nil)
          (starting 0.0)
          (starts 0)
          (most-starts 0)
          (functions '(drover-search--start drover-search--receive drover-transcript--search-filter
                                            drover-transcript--search-sentinel drover-search--start-later)))
      ;; What starting processes takes, all along.  Advising it may compile
      ;; a trampoline first, which takes a while: before what is timed.
      (advice-add 'make-process :around
                  (lambda (original &rest arguments)
                    (let ((begin (float-time)))
                      (prog1 (apply original arguments)
                        (setq starting (+ starting (- (float-time) begin))
                              starts (1+ starts)))))
                  '((name . drover-search-test-starting)))
      (dolist (function functions)
        (advice-add function :around
                    ;; Garbage collections are the whole Emacs's, not the search's.
                    (lambda (original &rest arguments)
                      (let ((begin (float-time))
                            (collecting gc-elapsed)
                            (started starting)
                            (count starts))
                        (prog1 (apply original arguments)
                          (let ((took (- (float-time) begin (- gc-elapsed collecting) (- starting started))))
                            (when (> took longest)
                              (setq longest took
                                    slowest function)))
                          (setq most-starts (max most-starts (- starts count))))))
                    '((name . drover-search-test-timing))))
      (unwind-protect
          ;; A function waits for a processor while the machine's are all
          ;; busy, as long as the system pleases, which only adds to what
          ;; it takes, the filter's own 0.3 ms at most on Windows: the
          ;; best of three runs stands for the search's own work.
          (let ((runs 0) (met nil) (lag 0.0) (collected 0.0) (spent 0.0))
            (while (and (not met) (< runs 3))
              (setq runs (1+ runs) longest 0.0 slowest nil most-starts 0)
              (let ((collecting gc-elapsed)
                    (started starting)
                    (count starts))
                (setq lag (drover-test-lag (lambda () (drover-search-test-text "reply")))
                      collected (- gc-elapsed collecting)
                      spent (- starting started))
                (message "Search of 122 transcripts, run %d: longest function %.2f ms (%s), longest gap %.1f ms, %.1f ms collecting, %d processes started in %.1f ms"
                         runs (* 1000 longest) slowest (* 1000 lag) (* 1000 collected) (- starts count) (* 1000 spent))
                ;; However busy the machine, never more processes at once.
                (should (<= most-starts (length (drover-search-sources))))
                (setq met (and (< longest 0.005) (< lag (+ 0.03 collected spent))))))
            (unless met
              (should (< longest 0.005))
              (should (< lag (+ 0.03 collected spent)))))
        (advice-remove 'make-process 'drover-search-test-starting)
        (dolist (function functions)
          (advice-remove function 'drover-search-test-timing))))))

(provide 'drover-search-test)
;;; drover-search-test.el ends here
