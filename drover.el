;;; drover.el --- Manage the sessions of coding agents  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; Author: Valentin Leon <valentin@leon.click>
;; Maintainer: Valentin Leon <valentin@leon.click>
;; Keywords: tools, llm, convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (magit-section "4.0") (transient "0.13"))
;; URL: https://github.com/vleonbonnet/drover

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

;; Drover gathers the sessions of every coding agent front end into one
;; place: which ones need you, which run, which finished while you were
;; elsewhere.  Front ends plug in as backends (see `drover-core'); the
;; first is opencode.el, loaded with it.  `drover-attention-mode' shows
;; how many sessions need you in the mode line and `drover-next' visits
;; the most urgent; `drover-list' lists every session in a window, and
;; `drover-list-side' beside your work, laid out for the window's width.
;; `drover-dispatch' is a menu of every command, on sessions and on the
;; prompt you write to an agent; bind it globally, for instance:
;;
;;   (keymap-global-set "C-'" #'drover-dispatch)

;;; Code:

(require 'drover-core)
(require 'drover-ui)
(require 'drover-keys)

(with-eval-after-load 'opencode
  (require 'drover-opencode))

(provide 'drover)
;;; drover.el ends here
