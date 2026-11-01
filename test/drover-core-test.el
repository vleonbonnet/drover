;;; drover-core-test.el --- Tests of the session model and backends  -*- lexical-binding: t; -*-

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

;;; Code:

(require 'drover-test-helpers)

(defun drover-test-ids (sessions)
  "Return the identifiers of SESSIONS."
  (mapcar #'drover-session-id sessions))

(ert-deftest drover-test-sort-urgent-first ()
  (let ((sessions (list (drover-test-session "idle" :status 'idle :updated 50.0)
                        (drover-test-session "done-new" :status 'done :since 30.0)
                        (drover-test-session "working" :status 'working :updated 10.0)
                        (drover-test-session "waiting" :status 'waiting :since 40.0)
                        (drover-test-session "done-old" :status 'done :since 20.0)
                        (drover-test-session "working-new" :status 'working :updated 60.0))))
    (should (equal (drover-test-ids (drover-sort sessions))
                   '("waiting" "done-old" "done-new" "working-new" "working" "idle")))))

(ert-deftest drover-test-sort-unknown-since-last ()
  (let ((sessions (list (drover-test-session "unknown" :status 'waiting)
                        (drover-test-session "known" :status 'waiting :since 5.0))))
    (should (equal (drover-test-ids (drover-sort sessions)) '("known" "unknown")))))

(ert-deftest drover-test-sort-leaves-input-alone ()
  (let ((sessions (list (drover-test-session "b" :status 'idle)
                        (drover-test-session "a" :status 'waiting))))
    (drover-sort sessions)
    (should (equal (drover-test-ids sessions) '("b" "a")))))

(ert-deftest drover-test-visible-sessions-hide-children ()
  (drover-test-with-backend (list (drover-test-session "root")
                                  (drover-test-session "child" :parent "root")
                                  (drover-test-session "ended" :parent "root" :status 'done)
                                  (drover-test-session "asking" :parent "root" :status 'waiting))
                            (should (equal (drover-test-ids (drover-visible-sessions)) '("root" "asking")))
                            (let ((drover-show-subagents 'all))
                              (should (equal (drover-test-ids (drover-visible-sessions)) '("root" "child" "ended" "asking"))))
                            (let ((drover-show-subagents 'none))
                              (should (equal (drover-test-ids (drover-visible-sessions)) '("root"))))))

(ert-deftest drover-test-attention-sessions ()
  (drover-test-with-backend (list (drover-test-session "idle")
                                  (drover-test-session "done" :status 'done :since 2.0)
                                  (drover-test-session "waiting" :status 'waiting :since 3.0))
    (should (equal (drover-test-ids (drover-attention-sessions)) '("waiting" "done")))))

(ert-deftest drover-test-actions-reach-backend ()
  (drover-test-with-backend (list (drover-test-session "s"))
    (let ((session (drover-find 'fake "s")))
      (drover-open session t)
      (drover-respond session)
      (drover-interrupt session)
      (drover-archive session)
      (drover-mark-seen session)
      (should (equal (reverse drover-test-calls)
                     '((:open "s" t) (:respond "s") (:interrupt "s") (:archive "s") (:mark-seen "s")))))))

(ert-deftest drover-test-recap ()
  "A recap comes from the backend once; a missing or failing :recap is a failure."
  (drover-test-with-backend nil
    (let ((results nil)
          (pending nil))
      (drover-register-backend 'later :label "Later" :sessions #'ignore
                               :recap (lambda (session callback)
                                        (push (cons (drover-session-id session) callback) pending)))
      (drover-register-backend 'broken :label "Broken" :sessions #'ignore
                               :recap (lambda (_session _callback) (error "No connection")))
      (drover-register-backend 'twice :label "Twice" :sessions #'ignore
                               :recap (lambda (_session callback)
                                        (funcall callback '(:prompt "Hi"))
                                        (funcall callback '(:prompt "Again"))))
      (drover-register-backend 'bare :label "Bare" :sessions #'ignore)
      (dolist (backend '(later broken twice bare))
        (drover-recap (drover-session-create :backend backend :id "x")
                      (lambda (recap) (push (cons backend recap) results))))
      ;; Asynchronous: nothing yet from `later'.
      (should (equal (reverse results)
                     '((broken :failure "No connection") (twice :prompt "Hi")
                       (bare :failure "Bare sessions tell no recap"))))
      (funcall (cdar pending) '(:reply "Done"))
      (should (equal (car results) '(later :reply "Done"))))))

(ert-deftest drover-test-unsupported-action ()
  (drover-test-with-backend nil
    (drover-register-backend 'bare :label "Bare" :sessions (lambda () nil))
    (let ((session (drover-session-create :backend 'bare :id "x")))
      (should-not (drover-supports-p session :respond))
      (should (drover-supports-p (drover-test-session "s") :respond))
      (should-error (drover-respond session) :type 'user-error))))

(ert-deftest drover-test-register-requires-sessions ()
  (drover-test-with-backend nil
    (should-error (drover-register-backend 'broken :label "Broken"))))

(ert-deftest drover-test-start-runs-once ()
  (drover-test-with-backend nil
    (drover-start)
    (drover-start)
    (should (equal drover-test-calls '((:start nil))))))

(ert-deftest drover-test-late-backend-starts-on-registration ()
  (drover-test-with-backend nil
                            (let ((started 0))
                              (drover-register-backend 'late :label "Late" :sessions #'ignore
                                                       :start (lambda () (cl-incf started)))
                              (should (= started 0))
                              (drover-start)
                              (should (= started 1))
                              (drover-register-backend 'later :label "Later" :sessions #'ignore
                                                       :start (lambda () (cl-incf started)))
                              (should (= started 2))
                              (should (memq 'later drover--started)))))

(ert-deftest drover-test-session-buffer ()
  (drover-test-with-backend nil
                            (let ((live (generate-new-buffer " live"))
                                  (dead (generate-new-buffer " dead")))
                              (unwind-protect
                                  (progn
                                    (kill-buffer dead)
                                    (drover-register-backend 'buffers :label "Buffers" :sessions #'ignore
                                                             :buffer (lambda (session)
                                                                       (pcase (drover-session-id session)
                                                                         ("live" live) ("dead" dead) ("error" (error "Boom")))))
                                    (cl-flet ((buffer-of (id)
                                                (drover-session-buffer (drover-session-create :backend 'buffers :id id))))
                                      (should (eq (buffer-of "live") live))
                                      (should-not (buffer-of "dead"))
                                      (should-not (buffer-of "none"))
                                      (should-not (buffer-of "error")))
                                    (should-not (drover-session-buffer (drover-test-session "s"))))
                                (kill-buffer live)))))

(ert-deftest drover-test-session-title-override ()
  (should (equal (drover-session-title (drover-test-session "s")) "Session s"))
  (should (equal (drover-session-title (drover-test-session "s" :title "Mine")) "Mine")))

(ert-deftest drover-test-failing-backend-contributes-nothing ()
  (drover-test-with-backend (list (drover-test-session "s"))
    (drover-register-backend 'broken :label "Broken" :sessions (lambda () (error "Boom")))
    (should (equal (drover-test-ids (drover-sessions)) '("s")))))

(ert-deftest drover-test-changes-coalesce ()
  (drover-test-with-backend nil
    (let ((runs 0))
      (add-hook 'drover-changed-hook (lambda () (cl-incf runs)))
      (drover-backend-changed 'fake "a")
      (drover-backend-changed 'fake "b")
      (drover-changed-now)
      (should (= runs 1))
      (drover-changed-now)
      (should (= runs 1)))))

(ert-deftest drover-test-format-age ()
  (should (equal (drover-format-age nil) ""))
  (should (equal (drover-format-age (- drover-test-now 42) drover-test-now) "42s"))
  (should (equal (drover-format-age (- drover-test-now 300) drover-test-now) "5m"))
  (should (equal (drover-format-age (- drover-test-now 7200) drover-test-now) "2h"))
  (should (equal (drover-format-age (- drover-test-now 172800) drover-test-now) "2d")))

(ert-deftest drover-test-session-summary ()
  (should (equal (drover-session-summary
                  (drover-test-session "s" :requests '((:kind question :summary "Which branch?"))))
                 "asks: Which branch?"))
  (should (equal (drover-session-summary
                  (drover-test-session "s" :requests '((:kind permission :summary "bash: make"))))
                 "permit: bash: make"))
  (should (equal (drover-session-summary (drover-test-session "s" :detail "Rate limited"))
                 "Rate limited")))

(ert-deftest drover-test-session-at-point ()
  (with-temp-buffer
    (should-error (drover-session-at-point) :type 'user-error)
    (let ((session (drover-test-session "s")))
      (setq drover-session-at-point-function (lambda () session))
      (should (eq (drover-session-at-point) session)))))

;;;; Face remapping

(ert-deftest drover-test-remap-default-face-stays-local ()
  "Remapping a view's default face never changes the global remapping.
A global `face-remapping-alist', such as one setting a stipple, shares
its list with every buffer-local value made from it."
  (let ((face-remapping-alist (list (list 'default (list :stipple "alpha:65%")))))
    (let ((global (copy-tree face-remapping-alist)))
      (with-temp-buffer
        (drover-remap-default-face 'bold)
        (should (memq 'bold (flatten-tree (alist-get 'default face-remapping-alist)))))
      (should (equal (default-value 'face-remapping-alist) global))
      (with-temp-buffer
        (should (equal face-remapping-alist global))))))

(ert-deftest drover-test-views-keep-the-global-remapping ()
  "Opening the session list leaves the global remapping alone."
  (require 'drover-list)
  (let ((face-remapping-alist (list (list 'default (list :stipple "alpha:65%")))))
    (let ((global (copy-tree face-remapping-alist)))
      (with-temp-buffer
        (drover-list-mode))
      (should (equal (default-value 'face-remapping-alist) global)))))

;;;; Measuring ink

(defmacro drover-test-with-fake-fonts (graphic &rest body)
  "Run BODY with fonts faked, on a graphic display when GRAPHIC is non-nil.
Every character advances 7 pixels, and draws 7 pixels of ink but R,
whose ink reaches 9 pixels left of it and 20 right; N has no glyph.
`fonts-asked' collects, newest first, what `font-at' was asked for:
\(INDEX WINDOW FACE REMAPPING), FACE being the face property there and
REMAPPING the face remapping in force."
  (declare (indent 1) (debug t))
  `(let ((fonts-asked nil))
     (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _) ,graphic))
               ((symbol-function 'buffer-text-pixel-size)
                (lambda (&rest _) (cons (* 7 (- (point-max) (point-min))) 1)))
               ((symbol-function 'font-at)
                (lambda (index &optional window string)
                  (push (list index window (get-text-property index 'face string) face-remapping-alist)
                        fonts-asked)
                  'fake-font))
               ((symbol-function 'font-get-glyphs)
                (lambda (font from _to object)
                  (should (eq font 'fake-font))
                  (let ((char (aref object from)))
                    (vector (pcase char
                              (?R (vector from from char 1 7 -9 20 10 2 nil))
                              (?N nil)
                              (_ (vector from from char 1 7 0 7 10 2 nil))))))))
       ,@body)))

(ert-deftest drover-test-text-ink-extent ()
  "Ink reaching past the advance counts, in the font each character is drawn in."
  (let ((remapping '((default drover-list default)))
        (window (selected-window)))
    (drover-test-with-fake-fonts t
      ;; R, 7 pixels in, reaches 27 pixels, past the advance of the
      ;; string, 21, and 2 pixels left of its start.
      (should (equal (drover-text-ink-extent (concat "a" (propertize "R" 'face 'bold) "b")
                                             remapping window)
                     '(2 . 27)))
      ;; Asked for each character, in the window, with the remapping in
      ;; force, the default face named last, so that its remapping counts.
      (should (equal (reverse fonts-asked)
                     `((0 ,window (default) ,remapping)
                       (1 ,window (bold default) ,remapping)
                       (2 ,window (default) ,remapping))))
      ;; Without ink beyond the advance, the advance.
      (should (equal (drover-text-ink-extent "ab" remapping window) '(0 . 14)))
      ;; A character without a glyph, or displayed as something else,
      ;; counts by its advance.
      (setq fonts-asked nil)
      (should (equal (drover-text-ink-extent (concat "N" (propertize "R" 'display "x")) remapping window)
                     '(0 . 14)))
      (should (equal (mapcar #'car fonts-asked) '(0)))
      ;; Faces as lists and as attributes.
      (setq fonts-asked nil)
      (drover-text-ink-extent (concat (propertize "a" 'face '(bold italic))
                                      (propertize "b" 'face '(:weight bold)))
                              remapping window)
      (should (equal (mapcar #'cadr (mapcar #'cdr (reverse fonts-asked)))
                     '((bold italic default) ((:weight bold) default)))))
    ;; On a text terminal, the advance alone.
    (drover-test-with-fake-fonts nil
      (should (equal (drover-text-ink-extent "aRb" remapping window) '(0 . 21)))
      (should-not fonts-asked))))

;;;; Trees

(defun drover-test-tree-shape (nodes)
  "Return trees NODES as nested lists of session identifiers."
  (mapcar (lambda (node)
            (cons (drover-session-id (drover-node-session node))
                  (drover-test-tree-shape (drover-node-children node))))
          nodes))

(ert-deftest drover-test-tree-nests-subagents ()
  (let ((nodes (drover-tree
                (list (drover-test-session "child" :parent "root" :status 'working :updated 5.0)
                      (drover-test-session "root" :status 'idle :updated 1.0)
                      (drover-test-session "grandchild" :parent "child" :status 'idle)
                      (drover-test-session "other" :status 'working :updated 3.0)))))
    (should (equal (drover-test-tree-shape nodes)
                   '(("root" ("child" ("grandchild"))) ("other"))))
    (should (eq (drover-node-status (car nodes)) 'working))
    (should (= (drover-node-updated (car nodes)) 5.0))))

(ert-deftest drover-test-tree-rolls-up-attention ()
  "A parent whose subagent waits for the user ranks with the waiting sessions."
  (let ((nodes (drover-tree
                (list (drover-test-session "busy" :status 'working)
                      (drover-test-session "parent" :status 'done :since 50.0)
                      (drover-test-session "asking" :parent "parent" :status 'waiting :since 30.0)
                      (drover-test-session "waiting" :status 'waiting :since 40.0)))))
    (should (equal (drover-test-tree-shape nodes)
                   '(("parent" ("asking")) ("waiting") ("busy"))))
    (should (eq (drover-node-status (car nodes)) 'waiting))
    (should (= (drover-node-since (car nodes)) 30.0))
    (should (equal (mapcar #'car (drover-group-trees-by-status nodes)) '(waiting working)))))

(ert-deftest drover-test-tree-orphans-and-cycles ()
  (let ((nodes (drover-tree
                (list (drover-test-session "orphan" :parent "gone")
                      (drover-test-session "self" :parent "self")
                      (drover-test-session "a" :parent "b")
                      (drover-test-session "b" :parent "a")))))
    ;; An orphan and a self-parent are trees of their own, and so is a cycle.
    (should (equal (sort (mapcar #'car (drover-test-tree-shape nodes)) #'string<)
                   '("a" "orphan" "self")))
    (should (equal (length (drover-tree-sessions nodes)) 4))))

(ert-deftest drover-test-tree-other-backend-is-not-a-parent ()
  (let ((nodes (drover-tree
                (list (drover-test-session "root")
                      (drover-session-create :backend 'other :id "child" :parent "root")))))
    (should (= (length nodes) 2))))

(ert-deftest drover-test-tree-visible ()
  "Subagents show as `drover-show-subagents' says: all, active, none."
  (let ((nodes (drover-tree
                (list (drover-test-session "root")
                      (drover-test-session "quiet" :parent "root" :status 'idle)
                      (drover-test-session "deep" :parent "quiet" :status 'working)
                      (drover-test-session "asking" :parent "root" :status 'waiting)
                      (drover-test-session "ended" :parent "root" :status 'done)
                      (drover-test-session "broke" :parent "root" :status 'failed)
                      (drover-test-session "retry" :parent "root" :status 'retrying)))))
    (let ((drover-show-subagents 'all))
      (should (equal (drover-tree-sessions (drover-tree-visible nodes))
                     (drover-tree-sessions nodes)))
      (should (= (drover-tree-hidden-count nodes) 0)))
    (let ((drover-show-subagents 'active))
      ;; Ended turns hide, unseen or not; a quiet subagent stays when a
      ;; session under it is active.
      (should (equal (drover-test-tree-shape (drover-tree-visible nodes))
                     '(("root" ("asking") ("retry") ("quiet" ("deep"))))))
      (should (= (drover-tree-hidden-count nodes) 2)))
    (let ((drover-show-subagents 'none))
      (should (equal (drover-test-tree-shape (drover-tree-visible nodes)) '(("root"))))
      (should (= (drover-tree-hidden-count nodes) 6)))
    ;; An unknown mode behaves as the default.
    (let ((drover-show-subagents 'bogus))
      (should (= (drover-tree-hidden-count nodes) 2)))))

(ert-deftest drover-test-subagent-modes-cycle ()
  "The modes cycle all, active, none; setting one redraws the views."
  (drover-test-with-backend nil
                            (let ((runs 0))
                              (add-hook 'drover-changed-hook (lambda () (cl-incf runs)))
                              (should (eq (drover-next-subagent-mode 'all) 'active))
                              (should (eq (drover-next-subagent-mode 'active) 'none))
                              (should (eq (drover-next-subagent-mode 'none) 'all))
                              (should (eq (drover-next-subagent-mode) 'none))
                              (drover-set-show-subagents 'all)
                              (should (eq drover-show-subagents 'all))
                              (should (= runs 1))
                              (should-error (drover-set-show-subagents t))
                              (should (eq (default-value 'drover-show-subagents) 'all))
                              (should (eq (eval (car (get 'drover-show-subagents 'standard-value)) t) 'active)))))

(ert-deftest drover-test-tree-active-subagents ()
  "Each session knows its subagents in the middle of a turn, at any depth."
  (let ((table (drover-tree-active-subagents
                (drover-tree
                 (list (drover-test-session "root" :status 'idle)
                       (drover-test-session "quiet" :parent "root" :status 'idle)
                       (drover-test-session "deep" :parent "quiet" :status 'working :updated 1.0)
                       (drover-test-session "asking" :parent "root" :status 'waiting :since 2.0)
                       (drover-test-session "ended" :parent "root" :status 'done)
                       (drover-test-session "retry" :parent "ended" :status 'retrying)
                       (drover-test-session "alone" :status 'working))))))
    ;; Most urgent first.
    (should (equal (drover-test-ids (gethash '(fake . "root") table)) '("asking" "retry" "deep")))
    (should (equal (drover-test-ids (gethash '(fake . "quiet") table)) '("deep")))
    (should (equal (drover-test-ids (gethash '(fake . "ended") table)) '("retry")))
    (dolist (id '("deep" "asking" "retry" "alone"))
      (should-not (gethash (cons 'fake id) table)))
    (should (= (hash-table-count table) 3))))

(ert-deftest drover-test-tree-keeps-order-of-ties ()
  "Trees and siblings that rank alike keep the backend's order, as `drover-sort' does."
  (let ((nodes (drover-tree (list (drover-test-session "a")
                                  (drover-test-session "b")
                                  (drover-test-session "a1" :parent "a")
                                  (drover-test-session "a2" :parent "a")
                                  (drover-test-session "c")))))
    (should (equal (drover-test-tree-shape nodes) '(("a" ("a1") ("a2")) ("b") ("c"))))))

(ert-deftest drover-test-tree-visible-ranks-by-what-shows ()
  "A hidden subagent does not place its parent in the subagent's group."
  (let ((nodes (drover-tree (list (drover-test-session "parent" :status 'idle)
                                  (drover-test-session "ended" :parent "parent" :status 'done)
                                  (drover-test-session "busy" :parent "parent" :status 'working)))))
    (should (eq (drover-node-status (car nodes)) 'done))
    ;; The finished subagent hides, so its parent is not listed as done.
    (let ((drover-show-subagents 'active))
      (should (eq (drover-node-status (car (drover-tree-visible nodes))) 'working)))
    (let ((drover-show-subagents 'none))
      (should (eq (drover-node-status (car (drover-tree-visible nodes))) 'idle)))
    (let ((drover-show-subagents 'all))
      (should (eq (drover-node-status (car (drover-tree-visible nodes))) 'done)))))

(ert-deftest drover-test-open-in-main-window ()
  "A list naming its main window opens sessions there, selected."
  (drover-test-with-backend (list (drover-test-session "a"))
    (save-window-excursion
      (delete-other-windows)
      (let* ((list-window (selected-window))
             (main (split-window list-window))
             (session (car drover-test-sessions)))
        (with-temp-buffer
          (set-window-buffer list-window (current-buffer))
          (setq-local drover-main-window-function (lambda () main))
          (drover-open session t)
          (should (eq (selected-window) main))
          (should (equal (car drover-test-calls) '(:open "a" nil))))
        ;; Without one, OTHER-WINDOW reaches the backend as given.
        (select-window list-window)
        (with-temp-buffer
          (drover-open session t)
          (should (eq (selected-window) list-window))
          (should (equal (car drover-test-calls) '(:open "a" t))))
        ;; A dead window is no main window.
        (with-temp-buffer
          (setq-local drover-main-window-function (lambda () nil))
          (drover-open session t)
          (should (equal (car drover-test-calls) '(:open "a" t))))))))

(ert-deftest drover-test-group-trees-by-status ()
  "Trees group by status in `drover-status-order', unknown statuses last."
  (let ((groups (drover-group-trees-by-status
                 (drover-tree (list (drover-test-session "w" :status 'working :updated 2.0)
                                    (drover-test-session "odd" :status 'mystery)
                                    (drover-test-session "n" :status 'waiting)
                                    (drover-test-session "w2" :status 'working :updated 1.0)
                                    (drover-test-session "odd2" :status 'stranger))))))
    (should (equal (mapcar #'car groups) '(waiting working mystery stranger)))
    (should (equal (mapcar (lambda (node) (drover-session-id (drover-node-session node)))
                           (alist-get 'working groups))
                   '("w" "w2")))))

(ert-deftest drover-test-group-trees-by-project ()
  (let ((groups (drover-group-trees-by-project
                 (drover-tree (list (drover-test-session "a1" :project "a" :status 'idle)
                                    (drover-test-session "b1" :project "b" :status 'waiting :since 1.0)
                                    (drover-test-session "child" :parent "a1" :project "b" :status 'done))))))
    (should (equal (mapcar #'car groups) '("b" "a")))
    (should (equal (drover-test-tree-shape (cdr (assoc "a" groups))) '(("a1" ("child")))))))

;;;; New backend operations

(ert-deftest drover-test-new-operations-reach-backend ()
  (drover-test-with-backend (list (drover-test-session "s"))
    (let ((session (drover-find 'fake "s")))
      (drover-rename session "New title")
      (drover-delete session)
      (drover-fork session)
      (drover-compact session)
      (should (equal (reverse drover-test-calls)
                     '((:rename "s" "New title") (:delete "s") (:fork "s") (:compact "s")))))))

(ert-deftest drover-test-new-operations-unsupported ()
  (drover-test-with-backend nil
    (drover-register-backend 'bare :label "Bare" :sessions #'ignore)
    (let ((session (drover-session-create :backend 'bare :id "x")))
      (dolist (operation '(:rename :delete :fork :compact))
        (should-not (drover-supports-p session operation)))
      (should-error (drover-rename session "t") :type 'user-error)
      (should-error (drover-delete session) :type 'user-error)
      (should-error (drover-fork session) :type 'user-error)
      (should-error (drover-compact session) :type 'user-error))))

(ert-deftest drover-test-parent-and-subagents ()
  (drover-test-with-backend (list (drover-test-session "root" :updated 1.0)
                                  (drover-test-session "idle" :parent "root" :status 'idle)
                                  (drover-test-session "asking" :parent "root" :status 'waiting)
                                  (drover-test-session "orphan" :parent "gone")
                                  (drover-session-create :backend 'other :id "foreign" :parent "root"))
    (let ((root (drover-find 'fake "root")))
      (should (equal (drover-test-ids (drover-subagents root)) '("asking" "idle")))
      (should (eq (drover-parent (drover-find 'fake "asking")) (drover-find 'fake "root")))
      (should-not (drover-parent root))
      (should-not (drover-parent (drover-find 'fake "orphan"))))))

;;;; Commands acting on the session at point

(defmacro drover-test-at-point (id &rest body)
  "Run BODY in a buffer whose session at point is the fake session ID."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (setq-local drover-session-at-point-function (lambda () (drover-find 'fake ,id)))
     ,@body))

(ert-deftest drover-test-session-at-point-noerror ()
  (with-temp-buffer
    (should-not (drover-session-at-point t))
    (setq drover-session-at-point-function #'ignore)
    (should-not (drover-session-at-point t))
    (should-error (drover-session-at-point) :type 'user-error)))

(ert-deftest drover-test-session-list-p ()
  (with-temp-buffer
    (should-not (drover-session-list-p))
    (setq drover-view-p t)
    (should (drover-session-list-p)))
  (with-temp-buffer
    (drover-session-keys-mode)
    (should (drover-session-list-p))))

(ert-deftest drover-test-commands-act-at-point ()
  (drover-test-with-backend (list (drover-test-session "s" :title "Old"))
    (drover-test-at-point "s"
      (cl-letf (((symbol-function 'y-or-n-p) #'always)
                ((symbol-function 'yes-or-no-p) #'always)
                ((symbol-function 'read-string)
                 (lambda (prompt _initial _history default)
                   (should (string-match-p "Old" prompt))
                   (should (equal default "Old"))
                   "  Brand new  ")))
        (dolist (command '(drover-respond drover-interrupt drover-archive drover-mark-seen
                                          drover-rename drover-delete))
          (call-interactively command)))
      (should (equal (reverse drover-test-calls)
                     '((:respond "s") (:interrupt "s") (:archive "s") (:mark-seen "s")
                       (:rename "s" "Brand new") (:delete "s")))))))

(ert-deftest drover-test-commands-confirm ()
  (drover-test-with-backend (list (drover-test-session "s"))
    (drover-test-at-point "s"
      (let ((asked nil))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) (push (cons 'y prompt) asked) nil))
                  ((symbol-function 'yes-or-no-p) (lambda (prompt) (push (cons 'yes prompt) asked) nil)))
          (should-error (call-interactively #'drover-interrupt) :type 'user-error)
          (should-error (call-interactively #'drover-archive) :type 'user-error)
          (should-error (call-interactively #'drover-delete) :type 'user-error))
        (should-not drover-test-calls)
        ;; Deleting is for good, so the answer is typed in full.
        (should (equal (reverse asked)
                       '((y . "Interrupt Session s? ") (y . "Archive Session s? ")
                         (yes . "Delete Session s for good? "))))))))

(ert-deftest drover-test-rename-needs-a-title ()
  (drover-test-with-backend (list (drover-test-session "s"))
    (drover-test-at-point "s"
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "   ")))
        (should-error (call-interactively #'drover-rename) :type 'user-error))
      (should-not drover-test-calls))))

(ert-deftest drover-test-commands-without-session ()
  (with-temp-buffer
    (dolist (command '(drover-respond drover-interrupt drover-archive drover-mark-seen
                                      drover-rename drover-delete))
      (should-error (call-interactively command) :type 'user-error))))

;;;; Prompt actions

(defvar drover-test-actions nil
  "Prompt actions performed, newest first, as (ACTION BUFFER WINDOW).")

(defun drover-test-action (action)
  "Return a command recording that it performed ACTION."
  (lambda ()
    (interactive)
    (push (list action (current-buffer) (selected-window)) drover-test-actions)))

(defmacro drover-test-with-buffers (sessions buffers &rest body)
  "Run BODY with the fake backend serving SESSIONS, showing them in BUFFERS.
BUFFERS is an alist (ID . BUFFER) of live buffers, killed afterwards."
  (declare (indent 2) (debug t))
  `(drover-test-with-backend ,sessions
     (let ((buffers ,buffers)
           (drover-test-actions nil)
           (drover-default-prompt-buffer-function nil))
       (unwind-protect
           (progn
             (setf (alist-get 'fake drover--backends)
                   (append (list :buffer (lambda (session)
                                           (cdr (assoc (drover-session-id session) buffers))))
                           (alist-get 'fake drover--backends)))
             ,@body)
         (mapc (lambda (entry) (when (buffer-live-p (cdr entry)) (kill-buffer (cdr entry))))
               buffers)))))

(ert-deftest drover-test-prompt-define ()
  (with-temp-buffer
    (drover-prompt-define 'send #'ignore)
    (drover-prompt-define 'attach #'ignore)
    (drover-prompt-define 'send #'identity)
    (should (equal drover-prompt-actions '((send . identity) (attach . ignore))))
    (drover-prompt-define 'send nil)
    (should (equal drover-prompt-actions '((attach . ignore))))
    (should (drover-prompt-defines-p (current-buffer) 'attach))
    (should-not (drover-prompt-defines-p (current-buffer) 'send))
    (let ((other (current-buffer)))
      (with-temp-buffer
        ;; The actions belong to their buffer.
        (should-not drover-prompt-actions)
        (should (drover-prompt-defines-p other 'attach))))))

(ert-deftest drover-test-prompt-target-current-buffer ()
  (with-temp-buffer
    (should-not (drover-prompt-target 'send))
    (drover-prompt-define 'send #'ignore)
    (should (eq (drover-prompt-target 'send) (current-buffer)))
    (should-not (drover-prompt-target 'revert))))

(ert-deftest drover-test-prompt-target-session-buffer-in-list ()
  (drover-test-with-buffers (list (drover-test-session "s"))
      (list (cons "s" (generate-new-buffer " s")))
    (with-current-buffer (cdr (assoc "s" buffers))
      (drover-prompt-define 'agent (drover-test-action 'agent))
      (drover-prompt-define 'revert (drover-test-action 'revert))
      (drover-prompt-define 'fork (drover-test-action 'fork)))
    (drover-test-at-point "s"
      ;; Outside a list, the session's buffer does not count.
      (should-not (drover-prompt-target 'agent))
      (setq-local drover-view-p t)
      (should (eq (drover-prompt-target 'agent) (cdr (assoc "s" buffers))))
      ;; Editing a prompt needs the prompt in sight: never from a list.
      (should-not (drover-prompt-target 'revert))
      (should-not (drover-prompt-target 'send))
      ;; In a list, forking acts on the whole session through its backend.
      (should-not (drover-prompt-target 'fork))
      (drover-prompt-run 'agent)
      (should (equal (car (car drover-test-actions)) 'agent))
      (should (eq (nth 1 (car drover-test-actions)) (cdr (assoc "s" buffers)))))))

(ert-deftest drover-test-prompt-target-default-buffer ()
  (let ((default (generate-new-buffer " default"))
        (drover-default-prompt-buffer-function nil))
    (unwind-protect
        (progn
          (with-current-buffer default
            (drover-prompt-define 'model #'ignore)
            (drover-prompt-define 'send #'ignore))
          (with-temp-buffer
            (should-not (drover-prompt-target 'model))
            (setq drover-default-prompt-buffer-function (lambda () default))
            (dolist (action '(model))
              (should (eq (drover-prompt-target action) default)))
            ;; Only agent, model, effort and directory fall back on it,
            ;; and only those it defines.
            (should-not (drover-prompt-target 'send))
            (should-not (drover-prompt-target 'agent))
            (with-current-buffer default
              (drover-prompt-define 'agent #'ignore)
              (drover-prompt-define 'effort #'ignore)
              (drover-prompt-define 'directory #'ignore))
            (dolist (action '(agent effort directory))
              (should (eq (drover-prompt-target action) default)))
            ;; The current buffer comes first.
            (drover-prompt-define 'model #'ignore)
            (should (eq (drover-prompt-target 'model) (current-buffer)))
            (setq drover-default-prompt-buffer-function #'ignore)
            (should-not (drover-prompt-target 'agent))))
      (kill-buffer default))))

(ert-deftest drover-test-prompt-target-last-answer ()
  (let ((older (generate-new-buffer " older"))
        (newer (generate-new-buffer " newer"))
        (drover-test-actions nil))
    (unwind-protect
        (save-window-excursion
          (dolist (buffer (list older newer))
            (with-current-buffer buffer
              (drover-prompt-define 'last-answer (drover-test-action 'last-answer))))
          (with-temp-buffer
            (switch-to-buffer older)
            (switch-to-buffer newer)
            (switch-to-buffer (get-buffer-create " elsewhere"))
            (should (eq (drover-prompt-target 'last-answer) newer))
            (drover-prompt-run 'last-answer)
            ;; Shown, then performed there.
            (should (eq (window-buffer (selected-window)) newer))
            (should (equal (car drover-test-actions) (list 'last-answer newer (selected-window))))))
      (kill-buffer older)
      (kill-buffer newer)
      (when (get-buffer " elsewhere") (kill-buffer " elsewhere")))))

(ert-deftest drover-test-prompt-run-in-shown-window ()
  "A target buffer shown in a window runs the action there, at its point."
  (drover-test-with-buffers (list (drover-test-session "s"))
      (list (cons "s" (generate-new-buffer " s")))
    (let ((target (cdr (assoc "s" buffers))))
      (with-current-buffer target
        (drover-prompt-define 'model (drover-test-action 'model)))
      (save-window-excursion
        (let ((window (split-window)))
          (set-window-buffer window target)
          (drover-test-at-point "s"
            (setq-local drover-view-p t)
            (let ((origin (selected-window)))
              (drover-prompt-run 'model)
              (should (equal (car drover-test-actions) (list 'model target window)))
              (should (eq (selected-window) origin)))))))))

(ert-deftest drover-test-prompt-run-without-target ()
  (with-temp-buffer
    (should-error (drover-prompt-run 'send) :type 'user-error)
    (should-error (drover-prompt-run 'last-answer) :type 'user-error)))

(ert-deftest drover-test-fork-and-compact-dispatch ()
  (drover-test-with-backend (list (drover-test-session "s"))
    (let ((drover-test-actions nil))
      ;; In a prompt buffer, at point.
      (drover-test-at-point "s"
        (drover-prompt-define 'fork (drover-test-action 'fork))
        (drover-prompt-define 'compact (drover-test-action 'compact))
        (call-interactively #'drover-fork)
        (call-interactively #'drover-compact)
        (should (equal (mapcar #'car drover-test-actions) '(compact fork)))
        (should-not drover-test-calls))
      ;; In a list, the whole session.
      (drover-test-at-point "s"
        (setq-local drover-view-p t)
        (call-interactively #'drover-fork)
        (call-interactively #'drover-compact)
        (should (equal (reverse drover-test-calls) '((:fork "s") (:compact "s")))))
      ;; Elsewhere, nothing.
      (drover-test-at-point "s"
        (should-error (call-interactively #'drover-fork) :type 'user-error)
        (should-error (call-interactively #'drover-compact) :type 'user-error)))))

;;;; View settings

(defun drover-test-filtered (filter sessions)
  "Return the identifiers of SESSIONS that match FILTER."
  (drover-test-ids (seq-filter (lambda (session) (drover-filter-match-p session filter))
                               sessions)))

(ert-deftest drover-test-filter-project ()
  (let ((sessions (list (drover-test-session "a" :project "drover")
                        (drover-test-session "b" :project "drover-extra")
                        (drover-test-session "c"))))
    ;; Exact, as `drover-session-project' returns it.
    (should (equal (drover-test-filtered '(:project "drover") sessions) '("a")))
    (should (equal (drover-test-filtered '(:project "Drover") sessions) nil))))

(ert-deftest drover-test-filter-statuses ()
  (let ((sessions (list (drover-test-session "w" :status 'waiting)
                        (drover-test-session "d" :status 'done)
                        (drover-test-session "i" :status 'idle))))
    (should (equal (drover-test-filtered '(:statuses (waiting done)) sessions) '("w" "d")))
    (should (equal (drover-test-filtered '(:statuses (working)) sessions) nil))))

(ert-deftest drover-test-filter-agent-and-model ()
  (let ((sessions (list (drover-test-session "a" :agent "build" :model "opus")
                        (drover-test-session "b" :agent "plan" :model "opus")
                        (drover-test-session "c"))))
    (should (equal (drover-test-filtered '(:agent "build") sessions) '("a")))
    (should (equal (drover-test-filtered '(:model "opus") sessions) '("a" "b")))
    (should (equal (drover-test-filtered '(:model "opus" :agent "plan") sessions) '("b")))
    (should (drover-filter-active-p '(:agent "build")))))

(ert-deftest drover-test-filter-provider ()
  "The provider filter matches the provider's name, not its display."
  (let ((sessions (list (drover-test-session "a" :provider "A" :provider-name "Anthropic")
                        (drover-test-session "b" :provider-name "OpenAI")
                        (drover-test-session "c" :provider "Anthropic"))))
    (should (equal (drover-test-filtered '(:provider "Anthropic") sessions) '("a")))
    (should (equal (drover-test-filtered '(:provider "OpenAI") sessions) '("b")))
    (should (drover-filter-active-p '(:provider "OpenAI")))
    (drover-test-with-backend nil
      (drover-set-filter :provider "OpenAI")
      (should (equal drover-filter '(:provider "OpenAI"))))))

(ert-deftest drover-test-filter-since ()
  (let ((sessions (list (drover-test-session "old" :updated 99.0)
                        (drover-test-session "edge" :updated 100.0)
                        (drover-test-session "new" :updated 150.0)
                        (drover-test-session "never"))))
    ;; Updated at or after.
    (should (equal (drover-test-filtered '(:since 100.0) sessions) '("edge" "new")))))

(ert-deftest drover-test-filter-search ()
  (let ((sessions (list (drover-test-session "title" :title "Fix AUTH flow")
                        (drover-test-session "summary" :requests '((:kind question :summary "Use OAuth?")))
                        (drover-test-session "detail" :detail "auth server down")
                        (drover-test-session "project" :project "authz")
                        (drover-test-session "branch" :branch "feature/Auth")
                        (drover-test-session "agent" :agent "auth-agent")
                        (drover-test-session "none"))))
    ;; Case-insensitive, in the title, summary, project and branch.
    (should (equal (drover-test-filtered '(:search "auth") sessions)
                   '("title" "summary" "detail" "project" "branch")))
    ;; A literal string, not a regexp.
    (should (equal (drover-test-filtered '(:search "a.th") sessions) nil))
    (should (equal (length (drover-test-filtered '(:search "") sessions)) 7))))

(ert-deftest drover-test-filter-every-member ()
  (let ((sessions (list (drover-test-session "match" :project "p" :status 'done :updated 10.0 :title "x auth")
                        (drover-test-session "project" :project "q" :status 'done :updated 10.0 :title "auth")
                        (drover-test-session "status" :project "p" :status 'idle :updated 10.0 :title "auth")
                        (drover-test-session "since" :project "p" :status 'done :updated 1.0 :title "auth")
                        (drover-test-session "search" :project "p" :status 'done :updated 10.0 :title "x"))))
    (should (equal (drover-test-filtered '(:project "p" :statuses (done) :since 5.0 :search "auth") sessions)
                   '("match")))))

(ert-deftest drover-test-filter-active-p ()
  (let ((drover-filter nil))
    (should-not (drover-filter-active-p))
    (should-not (drover-filter-active-p '(:project nil :search "")))
    (should (drover-filter-active-p '(:search "x")))
    (setq drover-filter '(:statuses (done)))
    (should (drover-filter-active-p))))

(defun drover-test-family ()
  "Return sessions of a root, its subagents, and another tree."
  (list (drover-test-session "root" :project "drover" :status 'idle :updated 1.0)
        (drover-test-session "explore" :parent "root" :project "drover" :status 'idle :title "auth explorer")
        (drover-test-session "deep" :parent "explore" :project "drover" :status 'done :since 5.0)
        (drover-test-session "build" :parent "root" :project "drover" :status 'working)
        (drover-test-session "other" :project "other" :status 'working :title "auth elsewhere")))

(ert-deftest drover-test-filter-trees-keeps-ancestors ()
  (let ((trees (drover-tree (drover-test-family))))
    ;; A matching subagent keeps the sessions above it, and nothing else.
    (should (equal (drover-test-tree-shape (drover-filter-trees trees '(:statuses (done))))
                   '(("root" ("explore" ("deep"))))))
    (should (equal (drover-test-tree-shape (drover-filter-trees trees '(:search "auth")))
                   '(("other") ("root" ("explore")))))
    (should-not (drover-filter-trees trees '(:project "nowhere")))
    ;; Trees rank by what remains.
    (should (eq (drover-node-status (car (drover-filter-trees trees '(:statuses (idle))))) 'idle))))

(ert-deftest drover-test-view-trees-without-filter ()
  (drover-test-with-backend (drover-test-family)
    (should (equal (drover-test-tree-shape (drover-view-trees))
                   (drover-test-tree-shape (drover-tree-visible (drover-tree (drover-test-family))))))
    ;; The explorer is quiet, and its own subagent ended: both hide.
    (should (equal (drover-test-tree-shape (drover-view-trees))
                   '(("root" ("build")) ("other"))))
    (should (= (drover-view-hidden-count) 2))
    (should (equal (drover-view-subagent-counts) '(3 . 2)))
    (let ((drover-show-subagents 'none))
      (should (equal (drover-view-subagent-counts) '(3 . 3))))
    (let ((drover-show-subagents 'all))
      (should (equal (drover-view-subagent-counts) '(3 . 0))))))

(ert-deftest drover-test-view-trees-show-matching-subagents ()
  (drover-test-with-backend (drover-test-family)
    ;; Hidden subagents show when they explain why their tree is listed.
    (setq drover-filter '(:statuses (working)))
    (should (equal (drover-test-tree-shape (drover-view-trees))
                   '(("root" ("build")) ("other"))))
    (should (= (drover-view-hidden-count) 0))
    (setq drover-filter '(:search "auth"))
    (should (equal (drover-test-tree-shape (drover-view-trees))
                   '(("other") ("root" ("explore")))))
    ;; Even when no subagent shows: the match explains its parent.
    (let ((drover-show-subagents 'none))
      (should (equal (drover-test-tree-shape (drover-view-trees))
                     '(("other") ("root" ("explore")))))
      (setq drover-filter '(:statuses (working)))
      (should (equal (drover-test-tree-shape (drover-view-trees))
                     '(("root" ("build")) ("other")))))))

(ert-deftest drover-test-view-trees-matching-root-hides-subagents ()
  "Subagents of a matching session show as the mode says, matching or not."
  (drover-test-with-backend (drover-test-family)
    (setq drover-filter '(:project "drover"))
    (should (equal (drover-test-tree-shape (drover-view-trees))
                   '(("root" ("build")))))
    (should (= (drover-view-hidden-count) 2))
    (let ((drover-show-subagents 'all))
      (should (equal (drover-test-tree-shape (drover-view-trees))
                     '(("root" ("explore" ("deep")) ("build")))))
      (should (= (drover-view-hidden-count) 0)))
    (let ((drover-show-subagents 'none))
      (should (equal (drover-test-tree-shape (drover-view-trees)) '(("root"))))
      (should (= (drover-view-hidden-count) 3)))))

(ert-deftest drover-test-view-trees-explicit-sessions ()
  (drover-test-with-backend nil
    (let ((sessions (list (drover-test-session "a" :status 'done :since 1.0)
                          (drover-test-session "b" :status 'idle))))
      (setq drover-filter '(:statuses (done)))
      (should (equal (drover-test-tree-shape (drover-view-trees sessions)) '(("a"))))
      (should (= (drover-view-hidden-count sessions) 0)))))

(ert-deftest drover-test-settings-redraw-at-once ()
  (drover-test-with-backend nil
    (let ((runs 0))
      (add-hook 'drover-changed-hook (lambda () (cl-incf runs)))
      (drover-backend-changed 'fake)
      (drover-set-filter :project "drover")
      ;; The pending change ran with it.
      (should (= runs 1))
      (should-not drover--change-timer)
      (should (equal drover-filter '(:project "drover")))
      (drover-set-filter :statuses '(done))
      (drover-set-filter :project "other")
      (should (equal drover-filter '(:statuses (done) :project "other")))
      (drover-set-filter :project nil)
      (drover-set-filter :search "")
      (should (equal drover-filter '(:statuses (done))))
      (drover-set-filter :agent "build")
      (should (equal drover-filter '(:statuses (done) :agent "build")))
      (drover-set-filter :agent nil)
      (drover-set-group-by 'project)
      (should (eq drover-group-by 'project))
      (dolist (grouping '(branch agent model updated nil))
        (drover-set-group-by grouping)
        (should (eq drover-group-by grouping)))
      (should (= runs 13))
      (should-error (drover-set-filter :colour "red"))
      (should-error (drover-set-group-by 'colour)))))

;;;; Prompt caches

(ert-deftest drover-test-cache-fraction ()
  (let ((now drover-test-now))
    (cl-flet ((session (&rest properties) (apply #'drover-test-session "s" properties)))
      ;; Four minutes left of five.
      (let ((live (session :cache-expires (+ now 240) :cache-ttl 300)))
        (should (= (drover-session-cache-remaining live now) 240))
        (should (= (drover-session-cache-fraction live now) 0.8))
        ;; It shrinks as time passes.
        (should (= (drover-session-cache-fraction live (+ now 90)) 0.5))
        (should (< (drover-session-cache-fraction live (+ now 200))
                   (drover-session-cache-fraction live (+ now 100))))
        ;; Expired, at its expiry or after.
        (should-not (drover-session-cache-fraction live (+ now 240)))
        (should-not (drover-session-cache-remaining live (+ now 300))))
      ;; Unknown expiry or lifetime.
      (should-not (drover-session-cache-fraction (session) now))
      (should-not (drover-session-cache-remaining (session :cache-ttl 300) now))
      (should-not (drover-session-cache-fraction (session :cache-expires (+ now 60)) now))
      (should (= (drover-session-cache-remaining (session :cache-expires (+ now 60)) now) 60))
      (should-not (drover-session-cache-fraction (session :cache-expires (+ now 60) :cache-ttl 0) now))
      ;; More left than a lifetime, as clocks disagree: full.
      (should (= (drover-session-cache-fraction (session :cache-expires (+ now 400) :cache-ttl 300) now)
                 1.0)))))

;;;; What sessions run

(ert-deftest drover-test-cache-description ()
  "A prompt cache tells when it expires, its window and the share of the prompt it holds."
  (let ((now drover-test-now))
    (cl-flet ((session (&rest properties) (apply #'drover-test-session "s" properties)))
      (should (equal (drover-session-cache-description
                      (session :cache-expires (+ now 240) :cache-ttl 3600
                               :cache-tokens 120000 :cache-prompt-tokens 130000)
                      now)
                     "expires in 4m (1h window), 120k of 130k tokens cached"))
      ;; A service not telling the whole prompt, or the tokens at all.
      (should (equal (drover-session-cache-description
                      (session :cache-expires (+ now 40) :cache-ttl 300 :cache-tokens 900) now)
                     "expires in 40s (5m window), 900 tokens cached"))
      (should (equal (drover-session-cache-description (session :cache-expires (+ now 40)) now)
                     "expires in 40s"))
      (should-not (drover-session-cache-description (session :cache-expires (- now 1) :cache-ttl 300) now))
      (should-not (drover-session-cache-description (session) now)))))

(ert-deftest drover-test-context-fraction ()
  "A context fills a share of its model's, beyond 1 past it, and tells its tier."
  (cl-flet ((session (&rest properties) (apply #'drover-test-session "s" properties)))
    (should (= (drover-session-context-fraction (session :context-tokens 50000 :context-limit 200000)) 0.25))
    (should (= (drover-session-context-fraction (session :context-tokens 300000 :context-limit 200000)) 1.5))
    (should-not (drover-session-context-fraction (session :context-tokens 50000)))
    (should-not (drover-session-context-fraction (session :context-limit 200000)))
    (should-not (drover-session-context-fraction (session :context-tokens 5 :context-limit 0)))
    (should (equal (drover-session-context-description (session :context-tokens 50000 :context-limit 200000))
                   "50k of 200k tokens (25%)"))
    (should (equal (drover-session-context-description (session :context-tokens 950))
                   "950 tokens"))
    (should (equal (drover-session-context-description
                    (session :context-tokens 1200000 :context-limit 1000000 :context-tier '(over 200000 "2×")))
                   "1.2M of 1M tokens (120%)\nPast 200k tokens: requests cost 2×"))
    (should (equal (drover-session-context-description
                    (session :context-tokens 186000 :context-limit 200000 :context-tier '(near 200000 "2×")))
                   "186k of 200k tokens (93%)\nNear 200k tokens, past which requests cost 2×"))
    (should-not (drover-session-context-description (session :context-limit 200000)))))

(ert-deftest drover-test-format-tokens ()
  (should (equal (mapcar #'drover-format-tokens
                         '(0 950 999 1000 8400 9940 9960 182345 999499 999500 1200000 12345678))
                 '("0" "950" "999" "1k" "8.4k" "9.9k" "10k" "182k" "999k" "1M" "1.2M" "12M"))))

(ert-deftest drover-test-prefetch ()
  "Each backend is asked once for its own sessions, in order; one failing spares the others."
  (drover-test-with-backend (list (drover-test-session "a") (drover-test-session "b"))
    (let ((other nil))
      (drover-test-register-simple 'other nil
                                   :prefetch (lambda (sessions)
                                               (push (mapcar #'drover-session-id sessions) other)))
      (drover-test-register-simple 'broken nil :prefetch (lambda (_) (error "Down")))
      (drover-test-register-simple 'plain nil)
      (let ((x (drover-session-create :backend 'other :id "x"))
            (y (drover-session-create :backend 'other :id "y"))
            (z (drover-session-create :backend 'broken :id "z"))
            (p (drover-session-create :backend 'plain :id "p")))
        (let ((inhibit-message t))
          (drover-prefetch (list (drover-test-session "b") x z p (drover-test-session "a") y)))
        (should (equal drover-test-prefetched '(("b" "a"))))
        (should (equal other '(("x" "y"))))
        ;; Nothing to ask, nothing asked.
        (drover-prefetch nil)
        (should (= (length drover-test-prefetched) 1))))))

;;;; Sorting

(defun drover-test-sortable ()
  "Return trees to sort: three roots, one with three subagents."
  (drover-tree
   (list (drover-test-session "b" :title "Bravo" :status 'waiting :since 1.0 :agent "plan"
                              :created 30.0 :updated 5.0)
         (drover-test-session "b2" :parent "b" :title "zulu" :status 'idle :created 3.0 :updated 50.0)
         (drover-test-session "b1" :parent "b" :title "Alpha" :status 'idle :created 2.0 :updated 40.0)
         (drover-test-session "b3" :parent "b" :title "mike" :status 'working :created 1.0 :updated 45.0)
         (drover-test-session "c" :title "charlie" :status 'working :agent "build" :created 10.0
                              :updated 20.0)
         (drover-test-session "a" :title "alpha" :status 'idle :created 20.0 :updated 10.0))))

(ert-deftest drover-test-sort-trees-by-urgency ()
  (let ((trees (drover-test-sortable)))
    (should (equal (drover-test-tree-shape trees) '(("b" ("b3") ("b2") ("b1")) ("c") ("a"))))
    ;; The default order is that of the trees, untouched.
    (should (eq (drover-sort-trees trees 'urgency) trees))
    (let ((drover-sort-by 'urgency) (drover-sort-descending nil))
      (should (eq (drover-sort-trees trees) trees)))
    ;; Reversed: least urgent first, subagents still under their parent.
    (should (equal (drover-test-tree-shape (drover-sort-trees trees 'urgency t))
                   '(("a") ("c") ("b" ("b1") ("b2") ("b3")))))))

(ert-deftest drover-test-sort-trees-keeps-subagents-under-parent ()
  (let ((trees (drover-test-sortable)))
    ;; Roots sorted, and siblings among themselves, ignoring case.
    (should (equal (drover-test-tree-shape (drover-sort-trees trees 'title))
                   '(("a") ("b" ("b1") ("b3") ("b2")) ("c"))))
    (should (equal (drover-test-tree-shape (drover-sort-trees trees 'title t))
                   '(("c") ("b" ("b2") ("b3") ("b1")) ("a"))))
    (should (equal (drover-test-tree-shape (drover-sort-trees trees 'created))
                   '(("c") ("a") ("b" ("b3") ("b1") ("b2")))))
    ;; The last activity of a tree is its latest.
    (should (equal (drover-test-tree-shape (drover-sort-trees trees 'updated t))
                   '(("b" ("b2") ("b3") ("b1")) ("c") ("a"))))
    ;; The trees keep their ranks.
    (should (eq (drover-node-status (car (drover-sort-trees trees 'title t))) 'working))
    (should (equal (drover-test-tree-shape trees) '(("b" ("b3") ("b2") ("b1")) ("c") ("a"))))))

(ert-deftest drover-test-sort-trees-missing-values-last ()
  "Sessions without a value come last, both ways; ties keep urgency order."
  (let ((trees (drover-test-sortable)))
    (should (equal (mapcar #'car (drover-test-tree-shape (drover-sort-trees trees 'agent)))
                   '("c" "b" "a")))
    (should (equal (mapcar #'car (drover-test-tree-shape (drover-sort-trees trees 'agent t)))
                   '("b" "c" "a")))
    ;; No project anywhere: the order of urgency stands.
    (should (equal (mapcar #'car (drover-test-tree-shape (drover-sort-trees trees 'project t)))
                   '("b" "c" "a")))
    (should-error (drover-sort-trees trees 'colour))))

(ert-deftest drover-test-sort-trees-by-provider ()
  "Trees sort by the name of their provider, sessions without one last."
  (let ((trees (drover-tree (list (drover-test-session "x" :status 'waiting :provider-name "openai")
                                  (drover-test-session "y" :status 'done)
                                  (drover-test-session "z" :provider "icon" :provider-name "Anthropic")))))
    (should (equal (mapcar #'car (drover-test-tree-shape (drover-sort-trees trees 'provider)))
                   '("z" "x" "y")))
    (should (equal (mapcar #'car (drover-test-tree-shape (drover-sort-trees trees 'provider t)))
                   '("x" "z" "y")))))

(ert-deftest drover-test-set-sort ()
  (drover-test-with-backend nil
    (let ((runs 0))
      (add-hook 'drover-changed-hook (lambda () (cl-incf runs)))
      (drover-set-sort 'title t)
      (should (eq drover-sort-by 'title))
      (should (eq drover-sort-descending t))
      (drover-set-sort 'created)
      (should-not drover-sort-descending)
      (should (= runs 2))
      (should-error (drover-set-sort 'colour)))))

;;;; Grouping

(ert-deftest drover-test-group-trees-by-fields ()
  (let ((trees (drover-tree
                (list (drover-test-session "a" :branch "main" :agent "build" :model "opus"
                                           :status 'idle)
                      (drover-test-session "b" :branch "dev" :agent "plan" :status 'waiting :since 1.0)
                      (drover-test-session "kid" :parent "a" :branch "dev" :agent "plan" :status 'done)
                      (drover-test-session "c" :branch "main" :model "opus" :status 'working)))))
    ;; By the root of each tree; a group comes with its most urgent tree.
    (should (equal (mapcar (lambda (group) (cons (car group) (drover-test-tree-shape (cdr group))))
                           (drover-group-trees trees 'branch))
                   '(("dev" ("b")) ("main" ("a" ("kid")) ("c")))))
    (should (equal (mapcar #'car (drover-group-trees trees 'agent)) '("plan" "build" nil)))
    (should (equal (mapcar #'car (drover-group-trees trees 'model)) '(nil "opus")))
    (should (equal (mapcar #'car (drover-group-trees trees 'provider)) '(nil)))
    (should (equal (mapcar #'car (drover-group-trees trees 'status)) '(waiting done working)))
    (should (equal (mapcar #'car (drover-group-trees trees 'project)) '(nil)))
    (should (equal (drover-group-trees trees nil) (list (cons nil trees))))
    (should-not (drover-group-trees nil nil))
    (should-error (drover-group-trees trees 'colour))))

(ert-deftest drover-test-group-trees-by-provider ()
  "Trees go with the provider name of their root, a group with its most urgent tree."
  (let ((trees (drover-tree
                (list (drover-test-session "a" :provider "A" :provider-name "Anthropic" :status 'idle)
                      (drover-test-session "b" :provider-name "OpenAI" :status 'waiting :since 1.0)
                      (drover-test-session "kid" :parent "a" :provider-name "OpenAI" :status 'done)
                      (drover-test-session "c" :provider "A" :provider-name "Anthropic" :status 'working)
                      (drover-test-session "d" :status 'idle)))))
    (should (equal (mapcar (lambda (group) (cons (car group) (drover-test-tree-shape (cdr group))))
                           (drover-group-trees trees 'provider))
                   '(("OpenAI" ("b")) ("Anthropic" ("a" ("kid")) ("c")) (nil ("d")))))
    (should (equal (drover-group-label 'provider nil) "No provider"))))

(ert-deftest drover-test-activity-buckets ()
  (let ((zone (getenv "TZ")))
    (unwind-protect
        (let ((now drover-test-now)
              (hour 3600.0))
          (set-time-zone-rule "UTC")
          ;; drover-test-now is 08:00 UTC.
          (should (eq (drover-activity-bucket now now) 'today))
          (should (eq (drover-activity-bucket (- now (* 8 hour)) now) 'today))
          (should (eq (drover-activity-bucket (- now (* 8 hour) 1) now) 'yesterday))
          (should (eq (drover-activity-bucket (- now (* 32 hour)) now) 'yesterday))
          (should (eq (drover-activity-bucket (- now (* 32 hour) 1) now) 'week))
          ;; A week, today included: five days before yesterday.
          (should (eq (drover-activity-bucket (- now (* 8 hour) (* 6 24 hour)) now) 'week))
          (should (eq (drover-activity-bucket (- now (* 8 hour) (* 6 24 hour) 1) now) 'older))
          (should-not (drover-activity-bucket nil now)))
      (set-time-zone-rule zone))))

(ert-deftest drover-test-group-trees-by-activity ()
  (let ((zone (getenv "TZ")))
    (unwind-protect
        (let* ((now drover-test-now)
               (trees (drover-tree
                       (list (drover-test-session "old" :updated (- now (* 30 86400)))
                             (drover-test-session "never")
                             ;; The most urgent tree, yet its group comes after newer ones.
                             (drover-test-session "asking" :status 'waiting :since 1.0
                                                  :updated (- now (* 40 86400)))
                             (drover-test-session "today" :updated (- now 60))
                             (drover-test-session "parent" :updated (- now (* 3 86400)))
                             ;; A subagent active today brings its tree with it.
                             (drover-test-session "kid" :parent "parent" :updated (- now 120))
                             (drover-test-session "week" :updated (- now (* 4 86400)))))))
          (set-time-zone-rule "UTC")
          (should (equal (mapcar (lambda (group) (cons (car group) (mapcar #'car (drover-test-tree-shape (cdr group)))))
                                 (drover-group-trees trees 'updated now))
                         '((today "today" "parent") (week "week") (older "asking" "old")
                           (nil "never")))))
      (set-time-zone-rule zone))))

(ert-deftest drover-test-view-groups ()
  "Groups keep their order; trees sort within each group."
  (drover-test-with-backend (list (drover-test-session "w2" :title "Zed" :status 'waiting :since 1.0)
                                  (drover-test-session "w1" :title "Ann" :status 'waiting :since 2.0)
                                  (drover-test-session "i" :title "Bob" :status 'idle))
    (should (equal (mapcar (lambda (group) (cons (car group) (mapcar #'car (drover-test-tree-shape (cdr group)))))
                           (drover-view-groups))
                   '(((status . waiting) "w2" "w1") ((status . idle) "i"))))
    (setq drover-sort-by 'title)
    (should (equal (mapcar (lambda (group) (cons (car group) (mapcar #'car (drover-test-tree-shape (cdr group)))))
                           (drover-view-groups))
                   '(((status . waiting) "w1" "w2") ((status . idle) "i"))))
    (setq drover-group-by nil)
    (should (equal (mapcar (lambda (group) (cons (car group) (mapcar #'car (drover-test-tree-shape (cdr group)))))
                           (drover-view-groups))
                   '((nil "w1" "i" "w2"))))
    (setq drover-test-sessions nil)
    (should-not (drover-view-groups))))

;;;; Older sessions

(defvar drover-test-history-calls nil
  "Callbacks the fake backend's :history received, newest first.")

(defun drover-test-register-history ()
  "Give the fake backend a :history function recording its callbacks."
  (setf (alist-get 'fake drover--backends)
        (append (list :history (lambda (callback) (push callback drover-test-history-calls)))
                (alist-get 'fake drover--backends))))

(ert-deftest drover-test-history-merges-older-sessions ()
  (drover-test-with-backend (list (drover-test-session "live" :status 'working :title "Live"))
    (let ((drover-test-history-calls nil)
          (runs 0))
      (should-not (drover-history-available-p))
      (drover-test-register-history)
      (should (drover-history-available-p))
      (add-hook 'drover-changed-hook (lambda () (cl-incf runs)))
      (drover-set-show-history t)
      (should drover-show-history)
      (should (= (length drover-test-history-calls) 1))
      (should (= runs 1))
      ;; Nothing yet: the live sessions alone.
      (should (equal (drover-test-ids (drover-known-sessions)) '("live")))
      (funcall (car drover-test-history-calls)
               (list (drover-test-session "live" :status 'idle :title "Stale record")
                     (drover-test-session "old" :status 'idle :updated 1.0)
                     (drover-test-session "old" :status 'idle :title "Duplicate")
                     (drover-test-session "old-kid" :parent "old")))
      (drover-changed-now)
      (should (= runs 2))
      ;; The live record wins; older ones follow, once each.
      (let ((known (drover-known-sessions)))
        (should (equal (drover-test-ids known) '("live" "old" "old-kid")))
        (should (equal (drover-session-title (car known)) "Live")))
      ;; Older sessions are found, listed, and have subagents.
      (should (equal (drover-session-title (drover-find 'fake "old")) "Session old"))
      (should (equal (drover-session-title (drover-find 'fake "live")) "Live"))
      (should (equal (drover-test-ids (drover-subagents (drover-find 'fake "old"))) '("old-kid")))
      (should (equal (drover-test-tree-shape (drover-view-trees)) '(("live") ("old"))))
      ;; The live sessions stay the backends' own.
      (should (equal (drover-test-ids (drover-sessions)) '("live")))
      (should (equal (drover-test-ids (drover-attention-sessions)) nil))
      ;; Hidden, they are forgotten.
      (drover-set-show-history nil)
      (should-not drover--history)
      (should (equal (drover-test-ids (drover-known-sessions)) '("live")))
      (should-not (drover-find 'fake "old"))
      (should (= runs 3)))))

(ert-deftest drover-test-history-drops-late-answers ()
  (drover-test-with-backend nil
    (let ((drover-test-history-calls nil))
      (drover-test-register-history)
      (drover-set-show-history t)
      (let ((first (car drover-test-history-calls)))
        ;; Fetched again, as a refresh does: only the last answer counts.
        (drover-history-fetch)
        (funcall first (list (drover-test-session "first")))
        (should-not drover--history)
        (funcall (car drover-test-history-calls) (list (drover-test-session "second")))
        (should (equal (drover-test-ids (drover-known-sessions)) '("second")))
        ;; An answer arriving once hidden is dropped.
        (drover-history-fetch)
        (drover-set-show-history nil)
        (funcall (car drover-test-history-calls) (list (drover-test-session "late")))
        (should-not drover--history)
        (should-not (drover-known-sessions))))))

(ert-deftest drover-test-history-of-unregistered-backend ()
  (drover-test-with-backend nil
    (let ((drover-test-history-calls nil))
      (drover-test-register-history)
      (drover-set-show-history t)
      (funcall (car drover-test-history-calls) (list (drover-test-session "old")))
      (should (drover-known-sessions))
      (drover-unregister-backend 'fake)
      (should-not drover--history)
      (should-not (drover-known-sessions)))))

(ert-deftest drover-test-history-failing-backend ()
  "A backend whose :history fails leaves the others fetching."
  (drover-test-with-backend nil
    (let ((drover-test-history-calls nil))
      (drover-register-backend 'broken :label "Broken" :sessions #'ignore
                               :history (lambda (_) (error "No service")))
      (drover-test-register-history)
      (drover-set-show-history t)
      (should (= (length drover-test-history-calls) 1)))))

;;;; Engines of sessions

(ert-deftest drover-test-session-engine-key ()
  (should-not (drover-session-engine-key (drover-test-session "s")))
  (should-not (drover-session-engine-key (drover-test-session "s" :engine 'opencode)))
  (should-not (drover-session-engine-key (drover-test-session "s" :engine-id "ses_1")))
  (should (equal (drover-session-engine-key (drover-test-session "s" :engine 'opencode :engine-id "ses_1"))
                 '(opencode . "ses_1"))))

(ert-deftest drover-test-backend-engines ()
  (drover-test-with-backend nil
    (should (equal (drover-backend-engines 'fake) '(fake)))
    (let ((drover-test-backend-engines '(opencode codex)))
      (should (equal (drover-backend-engines (drover-test-session "s")) '(opencode codex))))
    (drover-test-register-simple 'listed nil :engines '(claude-code))
    (drover-test-register-simple 'broken nil :engines (lambda () (error "Boom")))
    (drover-test-register-simple 'none nil)
    (should (equal (drover-backend-engines 'listed) '(claude-code)))
    (should-not (drover-backend-engines 'broken))
    (should-not (drover-backend-engines 'none))))

(defun drover-test-other (id &rest properties)
  "Return a session ID of the backend `other', with PROPERTIES."
  (apply #'drover-session-create :backend 'other :id id properties))

(ert-deftest drover-test-duplicate-sessions-merge ()
  "Two backends showing one conversation list it once, the first registered's."
  (drover-test-with-backend (list (drover-test-session "a" :engine 'opencode :engine-id "ses_1")
                                  (drover-test-session "b" :engine 'opencode :engine-id "ses_2")
                                  (drover-test-session "c" :engine 'opencode)
                                  (drover-test-session "d"))
    (drover-test-register-simple
     'other (list (drover-test-other "x" :engine 'opencode :engine-id "ses_1")
                  (drover-test-other "y" :engine 'codex :engine-id "ses_2")
                  (drover-test-other "z" :engine 'opencode)
                  (drover-test-other "w")))
    (cl-flet ((keys () (mapcar #'drover-session-key (drover-sessions))))
      ;; An unknown engine or identifier is no evidence.
      (should (equal (keys) '((other . "y") (other . "z") (other . "w")
                              (fake . "a") (fake . "b") (fake . "c") (fake . "d"))))
      ;; Each backend still finds its own record.
      (should (drover-find 'other "x"))
      (should (eq (drover-session-backend (drover-find-engine-session 'opencode "ses_1")) 'fake))
      (should (eq (drover-session-backend (drover-find-engine-session 'codex "ses_2")) 'other))
      (should-not (drover-find-engine-session 'opencode "ses_3"))
      (should-not (drover-find-engine-session 'opencode nil))
      ;; Registering again keeps the order of registration.
      (drover-test-register)
      (should (equal (keys) '((other . "y") (other . "z") (other . "w")
                              (fake . "a") (fake . "b") (fake . "c") (fake . "d")))))))

(ert-deftest drover-test-duplicate-sessions-prefer-a-buffer ()
  "The backend whose buffer for the conversation was selected last keeps it."
  (let ((buffer (generate-new-buffer " other"))
        (own (generate-new-buffer " fake")))
    (unwind-protect
        (drover-test-with-backend (list (drover-test-session "a" :engine 'opencode :engine-id "ses_1"))
          (drover-test-register-simple
           'other (list (drover-test-other "x" :engine 'opencode :engine-id "ses_1")
                        (drover-test-other "x2" :engine 'opencode :engine-id "ses_1"))
           :buffer (lambda (session) (and (equal (drover-session-id session) "x2") buffer)))
          ;; Records of one backend stay distinct sessions.
          (should (equal (mapcar #'drover-session-key (drover-sessions))
                         '((other . "x") (other . "x2"))))
          (should (eq (drover-session-backend (drover-find-engine-session 'opencode "ses_1")) 'other))
          ;; Both with a buffer: the one selected last.
          (setf (alist-get 'fake drover--backends)
                (append (list :buffer (lambda (_) own)) (alist-get 'fake drover--backends)))
          (bury-buffer buffer)
          (switch-to-buffer own)
          (should (equal (mapcar #'drover-session-key (drover-sessions)) '((fake . "a"))))
          (switch-to-buffer buffer)
          (should (equal (mapcar #'drover-session-key (drover-sessions))
                         '((other . "x") (other . "x2"))))
          ;; No buffer any more: the first registered.
          (kill-buffer buffer)
          (kill-buffer own)
          (should (equal (mapcar #'drover-session-key (drover-sessions)) '((fake . "a")))))
      (dolist (b (list buffer own))
        (when (buffer-live-p b)
          (kill-buffer b))))))

(ert-deftest drover-test-tree-parent-shown-by-another-backend ()
  "A subagent stays under its parent when another backend shows the parent."
  (let ((buffer (generate-new-buffer " other")))
    (unwind-protect
        (drover-test-with-backend (list (drover-test-session "ses_1" :engine 'opencode :engine-id "ses_1")
                                        (drover-test-session "ses_2" :parent "ses_1"
                                                             :engine 'opencode :engine-id "ses_2"))
          (drover-test-register-simple
           'other (list (drover-test-other "tok" :engine 'opencode :engine-id "ses_1"))
           :buffer (lambda (_) buffer))
          (switch-to-buffer buffer)
          (let ((nodes (drover-tree (drover-sessions))))
            (should (= (length nodes) 1))
            (should (equal (drover-session-key (drover-node-session (car nodes))) '(other . "tok")))
            (should (equal (mapcar (lambda (node) (drover-session-key (drover-node-session node)))
                                   (drover-node-children (car nodes)))
                           '((fake . "ses_2"))))))
      (kill-buffer buffer))))

(ert-deftest drover-test-duplicate-sessions-in-history ()
  "An older record of a conversation a backend follows live is not listed."
  (drover-test-with-backend (list (drover-test-session "a" :engine 'opencode :engine-id "ses_1"))
    (let ((drover-test-history-calls nil)
          (answer nil))
      (drover-test-register-simple
       'other nil :history (lambda (callback) (setq answer callback)))
      (drover-test-register-history)
      (drover-set-show-history t)
      (funcall answer (list (drover-test-other "x" :engine 'opencode :engine-id "ses_1")
                            (drover-test-other "y" :engine 'opencode :engine-id "ses_2")))
      (funcall (car drover-test-history-calls)
               (list (drover-test-session "b" :engine 'opencode :engine-id "ses_2")
                     (drover-test-session "c" :engine 'opencode :engine-id "ses_3")))
      ;; The live record wins over an older one of another backend; older
      ;; records of one conversation merge, the first registered winning.
      (should (equal (mapcar #'drover-session-key (drover-known-sessions))
                     '((fake . "a") (fake . "b") (fake . "c"))))
      (should (eq (drover-session-backend (drover-find-engine-session 'opencode "ses_2")) 'fake)))))

(ert-deftest drover-test-fake-prompt-operations ()
  "The fake backend's prompt operations record their calls, as tests expect."
  (drover-test-with-backend (list (drover-test-session "s"))
    (let ((session (drover-find 'fake "s"))
          (created nil)
          (failed nil)
          (applied 0))
      (should (equal (drover-backend-engines 'fake) '(fake)))
      (should (eq (drover-backend-call session :check "Hi" '(:agent "plan")) 'fake-token))
      (drover-backend-call session :apply-options '(:model nil) (lambda () (cl-incf applied)))
      (drover-backend-call session :send "Hi" 'fake-token)
      (should (= applied 1))
      (funcall (drover-backend-property 'fake :create) "/work/" 'fake '(:title "New")
               (lambda (new) (setq created new)) (lambda (message) (setq failed message)))
      (should (equal (drover-session-id created) "new-1"))
      (should (eq (drover-session-engine created) 'fake))
      (should (equal (drover-session-title created) "New"))
      ;; Served from then on.
      (should (drover-find 'fake "new-1"))
      (let ((drover-test-create-error "No service"))
        (funcall (drover-backend-property 'fake :create) "/work/" 'fake nil #'ignore
                 (lambda (message) (setq failed message))))
      (should (equal failed "No service"))
      (should (equal (reverse drover-test-calls)
                     '((:check "s" "Hi" (:agent "plan")) (:apply-options "s" (:model nil))
                       (:send "s" "Hi" fake-token) (:create "/work/" fake (:title "New"))
                       (:create "/work/" fake nil))))
      (setq drover-test-options '(("s" :model nil :variant high))
            drover-test-settable '(("s" :model)))
      (should (equal (drover-backend-call session :options) '(:model nil :variant high)))
      (should (equal (drover-backend-call session :settable) '(:model)))
      (should (equal (drover-backend-call created :settable) '(:agent :model :variant))))))

(ert-deftest drover-test-buffer-session ()
  (drover-test-with-backend (list (drover-test-session "s"))
    (let ((buffer (generate-new-buffer " session")))
      (should-not (drover-buffer-session buffer))
      (with-current-buffer buffer
        (setq-local drover-session-at-point-function (lambda () (drover-find 'fake "s"))))
      (should (equal (drover-session-id (drover-buffer-session buffer)) "s"))
      (kill-buffer buffer)
      (should-not (drover-buffer-session buffer))
      (should-not (drover-buffer-session nil)))))

;;;; Warnings and steps

(defmacro drover-test-with-warnings (&rest body)
  "Run BODY collecting the messages of `display-warning' in `warnings'.
They are newest first, as (TYPE MESSAGE LEVEL)."
  (declare (indent 0) (debug t))
  `(let ((warnings nil))
     (cl-letf (((symbol-function 'display-warning)
                (lambda (type message &optional level _buffer)
                  (push (list type message level) warnings))))
       ,@body)))

(ert-deftest drover-test-warn ()
  (drover-test-with-warnings
    (drover-warn "%s refused %s" "Session" "plan")
    (should (equal warnings '((drover "Session refused plan" :warning))))))

(ert-deftest drover-test-run-steps-in-order ()
  (drover-test-with-warnings
    (let ((ran nil)
          (done 0)
          (pending nil))
      (drover-run-steps (list (cons "agent" (lambda (next) (push 'agent ran) (funcall next)))
                              (cons "model" (lambda (next) (push 'model ran) (setq pending next)))
                              (cons "effort" (lambda (next) (push 'effort ran) (funcall next))))
                        (lambda () (cl-incf done)))
      ;; A step waits for its request to settle.
      (should (equal ran '(model agent)))
      (should (= done 0))
      (funcall pending)
      (should (equal ran '(effort model agent)))
      (should (= done 1))
      ;; A second call does nothing.
      (funcall pending)
      (should (= done 1))
      (should-not warnings)
      (drover-run-steps nil (lambda () (cl-incf done)))
      (should (= done 2)))))

(ert-deftest drover-test-run-steps-signal ()
  "A step that signals is reported, and the next one runs."
  (drover-test-with-warnings
    (let ((ran nil)
          (done 0))
      (drover-run-steps (list (cons "model" (lambda (_next) (error "No such model")))
                              (cons "effort" (lambda (next) (push 'effort ran) (funcall next))))
                        (lambda () (cl-incf done)))
      (should (equal ran '(effort)))
      (should (= done 1))
      (should (equal warnings
                     '((drover "Could not set model: No such model; sending the prompt anyway"
                               :warning))))
      ;; Signalling once settled warns nothing more.
      (setq warnings nil)
      (drover-run-steps (list (cons "model" (lambda (next) (funcall next) (error "Late"))))
                        (lambda () (cl-incf done)))
      (should (= done 2))
      (should-not warnings))))

(ert-deftest drover-test-run-steps-timeout ()
  "A step not settled in time is reported, and its late reply ignored."
  (drover-test-with-warnings
    (let ((drover-step-timeout 0.01)
          (done 0)
          (late nil))
      (drover-run-steps (list (cons "agent" (lambda (next) (setq late next))))
                        (lambda () (cl-incf done)))
      (should (= done 0))
      (with-timeout (5 (error "The step never timed out"))
        (while (zerop done)
          (accept-process-output nil 0.01)))
      (should (= done 1))
      (should (equal warnings
                     '((drover "Timed out setting agent; sending the prompt anyway" :warning))))
      (funcall late)
      (should (= done 1)))))

;;;; Git branches

(ert-deftest drover-test-git-branch ()
  (let* ((root (file-name-as-directory (make-temp-file "drover-git" t)))
         (main (expand-file-name "main/" root))
         (linked (expand-file-name "linked/" root))
         (plain (expand-file-name "plain/" root))
         (drover--branches (make-hash-table :test #'equal)))
    (unwind-protect
        (cl-flet ((write (file content)
                    (make-directory (file-name-directory file) t)
                    (with-temp-file file (insert content))))
          (write (expand-file-name ".git/HEAD" main) "ref: refs/heads/feature/x\n")
          (make-directory (expand-file-name "src" main) t)
          (write (expand-file-name ".git/worktrees/linked/HEAD" main) "ref: refs/heads/side\n")
          (write (expand-file-name ".git" linked)
                 (concat "gitdir: " (expand-file-name ".git/worktrees/linked" main) "\n"))
          (make-directory plain t)
          ;; Read without Git, from a subdirectory too.
          (should (equal (drover-git-branch (expand-file-name "src" main)) "feature/x"))
          (should (equal (drover-git-branch linked) "side"))
          (should-not (drover-git-branch plain))
          (should-not (drover-git-branch (expand-file-name "missing/" root)))
          (should-not (drover-git-branch nil))
          ;; Kept for `drover-branch-ttl' seconds.
          (write (expand-file-name ".git/worktrees/linked/HEAD" main) "ref: refs/heads/renamed\n")
          (should (equal (drover-git-branch (directory-file-name linked)) "side"))
          (let ((drover-branch-ttl 0))
            (should (equal (drover-git-branch linked) "renamed")))
          ;; Detached: a short commit.
          (write (expand-file-name ".git/HEAD" main) "0123456789abcdef\n")
          (let ((drover-branch-ttl 0))
            (should (equal (drover-git-branch main) "0123456"))))
      (delete-directory root t))))

;;;; The default prompt buffer

(ert-deftest drover-test-default-prompt-buffer-function ()
  "Drover's own prompt is the default prompt buffer, loaded on demand."
  (should (eq (eval (car (get 'drover-default-prompt-buffer-function 'standard-value)) t)
              #'drover-prompt-default-buffer))
  (should (fboundp 'drover-prompt-default-buffer)))

(provide 'drover-core-test)
;;; drover-core-test.el ends here
