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

(ert-deftest drover-test-group-by-status ()
  (let ((groups (drover-group-by-status
                 (list (drover-test-session "w" :status 'working)
                       (drover-test-session "n" :status 'waiting)
                       (drover-test-session "w2" :status 'working)))))
    (should (equal (mapcar #'car groups) '(waiting working)))
    (should (equal (drover-test-ids (alist-get 'working groups)) '("w" "w2")))))

(ert-deftest drover-test-group-by-project ()
  (let ((groups (drover-group-by-project
                 (list (drover-test-session "a1" :project "a" :status 'idle)
                       (drover-test-session "b1" :project "b" :status 'waiting)
                       (drover-test-session "x" :status 'working)
                       (drover-test-session "a2" :project "a" :status 'done)))))
    (should (equal (mapcar #'car groups) '("b" "a" nil)))
    (should (equal (drover-test-ids (cdr (assoc "a" groups))) '("a2" "a1")))))

(ert-deftest drover-test-visible-sessions-hide-children ()
  (drover-test-with-backend (list (drover-test-session "root")
                                  (drover-test-session "child" :parent "root")
                                  (drover-test-session "asking" :parent "root" :status 'waiting))
    (should (equal (drover-test-ids (drover-visible-sessions)) '("root" "asking")))
    (let ((drover-show-children t))
      (should (equal (drover-test-ids (drover-visible-sessions)) '("root" "child" "asking"))))))

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
  "Opening the dashboard and the sidebar leaves the global remapping alone."
  (require 'drover-dashboard)
  (require 'drover-sidebar)
  (let ((face-remapping-alist (list (list 'default (list :stipple "alpha:65%")))))
    (let ((global (copy-tree face-remapping-alist)))
      (dolist (mode '(drover-dashboard-mode drover-sidebar-mode))
        (with-temp-buffer
          (funcall mode)))
      (should (equal (default-value 'face-remapping-alist) global)))))

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
  (let* ((nodes (drover-tree
                 (list (drover-test-session "root")
                       (drover-test-session "quiet" :parent "root" :status 'idle)
                       (drover-test-session "asking" :parent "root" :status 'waiting)
                       (drover-test-session "deep" :parent "quiet" :status 'done)))))
    (let ((drover-show-children nil))
      ;; A quiet subagent stays when a session under it needs the user.
      (should (equal (drover-test-tree-shape (drover-tree-visible nodes))
                     '(("root" ("asking") ("quiet" ("deep"))))))
      (should (= (drover-tree-hidden-count nodes) 0)))
    (let ((drover-show-children nil)
          (nodes (drover-tree (list (drover-test-session "root")
                                    (drover-test-session "quiet" :parent "root")))))
      (should (equal (drover-test-tree-shape (drover-tree-visible nodes)) '(("root"))))
      (should (= (drover-tree-hidden-count nodes) 1)))
    (let ((drover-show-children t))
      (should (equal (drover-tree-sessions (drover-tree-visible nodes))
                     (drover-tree-sessions nodes))))))

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
  (let* ((drover-show-children nil)
         (nodes (drover-tree (list (drover-test-session "parent" :status 'idle)
                                   (drover-test-session "busy" :parent "parent" :status 'working)))))
    (should (eq (drover-node-status (car nodes)) 'working))
    (should (eq (drover-node-status (car (drover-tree-visible nodes))) 'idle))
    (let ((drover-show-children t))
      (should (eq (drover-node-status (car (drover-tree-visible nodes))) 'working)))))

(ert-deftest drover-test-group-trees-by-project ()
  (let ((groups (drover-group-trees-by-project
                 (drover-tree (list (drover-test-session "a1" :project "a" :status 'idle)
                                    (drover-test-session "b1" :project "b" :status 'waiting :since 1.0)
                                    (drover-test-session "child" :parent "a1" :project "b" :status 'done))))))
    (should (equal (mapcar #'car groups) '("b" "a")))
    (should (equal (drover-test-tree-shape (cdr (assoc "a" groups))) '(("a1" ("child")))))))

(provide 'drover-core-test)
;;; drover-core-test.el ends here
