;;; drover-search.el --- Searching sessions, by what they tell and what was said  -*- lexical-binding: t; -*-

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

;; `drover-search', S in `drover-dispatch', reads a text in the
;; minibuffer and offers the sessions matching it, those the list would
;; show, in the list's order, scoped by the view settings the list
;; shares: `drover-filter', the older and archived sessions, the
;; subagents shown.  RET opens the session chosen, as RET in the list;
;; C-l shows its row in the list; C-', or the key of `drover-dispatch',
;; leaves the minibuffer for a menu of those settings, on the keys the
;; list's menu gives them, then RET or C-' searches again with what was
;; typed.
;;
;; A session matches when every word typed is in its title, summary,
;; project, branch, or in the texts its backends tell for searching
;; (:search-text), such as the first and last prompts of a Claude Code
;; conversation: that much is in memory, matched as each key is typed.
;; Backends may search more, what was said in their conversations, with
;; :search: once typing pauses, each is asked in the background, the
;; run before cancelled; the sessions their matches name join those
;; offered, a snippet of the match in their annotation.  Claude Code's
;; transcripts and Codex's rollouts are searched with ripgrep (see
;; `drover-transcript-search').
;;
;; The candidates work with any completion UI: a completion table
;; matched by a completion style of its own, `drover-search', which the
;; search puts first for its category, `drover-session'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'drover-core)
(require 'drover-ui)
(require 'drover-keys)
(require 'drover-model)
(require 'opencode-look)

;; Reloaded into an Emacs running an older drover, load its core anew.
(unless (and (boundp 'drover-core-generation) (>= drover-core-generation 12))
  (load "drover-core" nil t))

(declare-function drover-list-reveal "drover-list" (session))

(defgroup drover-search nil
  "Searching agent sessions."
  :group 'drover)

(defcustom drover-search-delay 0.25
  "Seconds of pause in typing before the backends search what was said.
Each key typed cancels the search running and starts the wait anew."
  :type 'number
  :group 'drover-search)

(defcustom drover-search-text-minimum 3
  "Characters a search needs before the backends search what was said.
Shorter, it matches what the list tells of sessions alone."
  :type 'natnum
  :group 'drover-search)

(defcustom drover-search-text-limit 200
  "Matches at most a search of what was said takes from the backends.
The backends searching are stopped once they reported that many."
  :type 'natnum
  :group 'drover-search)

(defconst drover-search-snippet-length 120
  "Characters at most of the snippet of a match a candidate shows.")

(defconst drover-search-title-width 60
  "Columns at most the titles of the candidates are padded to.")

(defconst drover-search-location-width 32
  "Columns at most the project and branch of the candidates are padded to.")

(defconst drover-search-refresh-delay 0.1
  "Seconds the candidates wait to show matches arriving, gathering others.")

(defconst drover-search-rebuild-delay 1.0
  "Seconds after sessions changed before the candidates are made anew.")

;;;; State

(cl-defstruct (drover-search--entry (:constructor drover-search--entry-make)
                                    (:copier nil))
  "One session a search may offer."
  (session nil :documentation "The `drover-session' record.")
  (key nil :documentation "Its `drover-session-key'.")
  (name nil :documentation "The candidate's string, unique among them.")
  (haystack nil :documentation "What words typed are looked for in, downcased.")
  (depth 0 :documentation "How deep it is in its tree, 0 at the top.")
  (visible nil :documentation "Non-nil when the list shows it, matching or not.")
  (group nil :documentation "The title of its group in the list, or nil."))

(cl-defstruct (drover-search--state (:constructor drover-search--state-make)
                                    (:copier nil))
  "A search in progress: its candidates, and the backends searching for it."
  (stamp nil :documentation "The layout, see `drover-layout'.")
  (entries nil :documentation "The entry of every session, in the list's order, a vector.")
  (names nil :documentation "Hash table of the entries by name.")
  (keys nil :documentation "Hash table of the entries by session key.")
  (engines nil :documentation "Hash table of the entries by engine key.")
  (widths nil :documentation "(TITLE . LOCATION), the columns annotations pad to.")
  (minibuffer nil :documentation "The minibuffer reading the search, or nil.")
  (query nil :documentation "The text typed last, trimmed.")
  (words nil :documentation "The words of QUERY, downcased.")
  (generation 0 :documentation "The number of the latest run of the backends.")
  (runs nil :documentation "The backends searching, an alist (BACKEND . CANCEL).")
  (text-query nil :documentation "The text the backends were asked for last.")
  (hits nil :documentation "Hash table of the matches of text by session key.
Each is a plist of :snippet, :count, :backend and :position.")
  (outside 0 :documentation "Matches naming sessions the search does not offer.")
  (received 0 :documentation "Matches the backends reported in this run.")
  (failed nil :documentation "The backends that failed, an alist (BACKEND . MESSAGE).")
  (delay-timer nil :documentation "The timer asking the backends once typing paused.")
  (refresh-timer nil :documentation "The timer showing matches arrived.")
  (rebuild-timer nil :documentation "The timer making the candidates anew.")
  (overlay nil :documentation "The overlay telling how the backends search.")
  (starting nil :documentation "Non-nil while the backends are being asked.")
  (asked 0 :documentation "How many backends searched in the latest run.")
  (done nil :documentation "Function called once every backend finished, or nil.
For `drover-search-text'."))

(drover-note-layout 'drover-search--entry 'drover-search--state)

(defvar drover-search--current nil
  "The search the minibuffer reads, or nil.")

(defvar drover-search--programmatic nil
  "The search `drover-search-text' runs, while no minibuffer reads one.")

(defvar drover-search-history nil
  "Texts searched for, for the minibuffer's history.")

(defvar drover-search--exit nil
  "What leaving the minibuffer of the search does.
Nil opens the session chosen, `locate' shows its row in the list,
`scope' opens the menu of what the search searches.")

(defun drover-search--live (&optional state)
  "Return STATE, or the search in progress, when of the current layout, else nil."
  (let ((state (or state drover-search--current)))
    (and state
         (drover-layout-current-p state 'drover-search--state)
         state)))

;;;; Candidates

(defun drover-search--sessions ()
  "Return the sessions a search offers, those lists show, before filtering."
  (let ((live (drover-index-sessions)))
    (if (or drover-show-history drover-show-archived)
        (drover-known-sessions live)
      live)))

(defun drover-search--texts (session backends)
  "Return the texts the backends tell for searching SESSION, bounded.
BACKENDS is a hash table caching, by engine, the backends whose
:search-text may know it: its own, and those recording its engine's
conversations (see :recorded in `drover-register-backend')."
  (let* ((engine (and (not (drover-session-host session)) (drover-session-engine session)))
         (own (drover-session-backend session))
         (texts nil))
    (dolist (backend (cons own (and engine
                                    (with-memoization (gethash engine backends)
                                      (drover--recording-backends engine)))))
      (when-let* ((function (drover-backend-property backend :search-text))
                  ((functionp function)))
        (dolist (text (drover-with-reporting "search texts"
                        (funcall function session)))
          (when-let* ((line (drover-bound-line text)))
            (push line texts)))))
    (delete-dups texts)))

(defun drover-search--haystack (session texts)
  "Return what words are looked for in for SESSION, with TEXTS, downcased."
  (downcase (mapconcat #'identity
                       (delq nil (append (list (drover-session-title session)
                                               (drover-session-summary session)
                                               (drover-session-project session)
                                               (drover-session-branch session))
                                         texts))
                       "\n")))

(defun drover-search--group-title (group)
  "Return the title of GROUP, as `drover-view-arrange' gives it, or nil."
  (and group (drover-group-label (car group) (cdr group))))

(defun drover-search--build (&optional now)
  "Return a new search over the sessions lists show, at NOW.
Every session of the trees `drover-filter' keeps is an entry, in the
order the list shows them: each tree after `drover-view-arrange',
subagents under their parent, those the list hides marked so."
  (let* ((now (or now (float-time)))
         (trees (drover-tree (drover-search--sessions)))
         (kept trees)
         (visible nil))
    (if (drover-filter-active-p)
        (pcase-let ((`(,cut . ,shown) (drover--filter-trees trees drover-filter)))
          (setq kept cut
                visible (drover-tree-visible
                         cut (lambda (node)
                               (gethash (drover-session-key (drover-node-session node)) shown)))))
      (setq visible (drover-tree-visible trees)))
    (let ((shown (make-hash-table :test #'equal))
          (titles (make-hash-table :test #'equal))
          (backends (make-hash-table :test #'eq))
          (entries nil)
          (names (make-hash-table :test #'equal))
          (keys (make-hash-table :test #'equal))
          (engines (make-hash-table :test #'equal))
          (title-width 0)
          (location-width 0))
      (dolist (session (drover-tree-sessions visible))
        (puthash (drover-session-key session) t shown))
      (pcase-dolist (`(,group . ,nodes) (drover-view-arrange kept now))
        (let ((title (drover-search--group-title group)))
          (cl-labels ((walk (node depth)
                        (let* ((session (drover-node-session node))
                               (key (drover-session-key session)))
                          (unless (gethash key keys)
                            (let ((entry (drover-search--entry-make
                                          :session session :key key :depth depth
                                          :visible (gethash key shown)
                                          :group title
                                          :haystack (drover-search--haystack
                                                     session (drover-search--texts session backends)))))
                              (puthash key entry keys)
                              (when-let* ((engine-key (drover-session-engine-key session)))
                                (unless (gethash engine-key engines)
                                  (puthash engine-key entry engines)))
                              (cl-incf (gethash (format "%s" (drover-session-display-title session)) titles 0))
                              (push entry entries)))
                          (dolist (child (drover-node-children node))
                            (walk child (1+ depth))))))
            (dolist (node nodes)
              (walk node 0)))))
      (setq entries (nreverse entries))
      (dolist (entry entries)
        (let* ((session (drover-search--entry-session entry))
               (title (format "%s" (drover-session-display-title session)))
               (base (if (> (gethash title titles 0) 1)
                         (concat title (drover-face (format " (%s)" (drover-session-id session)) 'drover-dim))
                       title))
               (name base)
               (counter 1))
          (while (gethash name names)
            (setq counter (1+ counter)
                  name (format "%s <%d>" base counter)))
          (puthash name entry names)
          (setf (drover-search--entry-name entry) name)
          (setq title-width (max title-width (min drover-search-title-width (string-width name)))
                location-width (max location-width
                                    (min drover-search-location-width
                                         (string-width (or (drover-session-location session) "")))))))
      (drover-search--state-make
       :stamp (drover-layout 'drover-search--state 'drover-search--entry)
       :entries (vconcat entries) :names names :keys keys :engines engines
       :widths (cons title-width location-width)
       :hits (make-hash-table :test #'equal)))))

(defun drover-search--words (query)
  "Return the words of QUERY, downcased."
  (split-string (downcase (or query "")) "[ \t\n]+" t))

(defun drover-search--match-p (entry words)
  "Return non-nil when every one of WORDS is in what ENTRY tells."
  (let ((haystack (drover-search--entry-haystack entry))
        (match t))
    (while (and match words)
      (setq match (string-search (car words) haystack)
            words (cdr words)))
    (and match t)))

(defun drover-search-entries (state query)
  "Return the entries of STATE matching QUERY, in the list's order.
An entry matches when every word of QUERY is in what it tells, or when
a backend found QUERY in what its session said (see the hits of
STATE).  A subagent the list hides shows when it matches and no session
above it does, as the list shows such a subagent under a filter.
Without words, the entries the list shows.  It allocates nothing but
the list it returns, as it runs at every key typed."
  (let* ((words (drover-search--words query))
         (hits (drover-search--state-hits state))
         (any-hits (> (hash-table-count hits) 0))
         (entries (drover-search--state-entries state))
         (count (length entries))
         (matched (make-vector 64 nil))
         (index 0)
         (result nil))
    (while (< index count)
      (let ((entry (aref entries index)))
        (if (null words)
            (when (drover-search--entry-visible entry)
              (push entry result))
          (let* ((depth (min 63 (drover-search--entry-depth entry)))
                 (match (or (drover-search--match-p entry words)
                            (and any-hits (gethash (drover-search--entry-key entry) hits) t)))
                 (above nil)
                 (level 0))
            (while (and (not above) (< level depth))
              (setq above (aref matched level)
                    level (1+ level)))
            (aset matched depth match)
            (when (and match (or (drover-search--entry-visible entry) (not above)))
              (push entry result)))))
      (setq index (1+ index)))
    (nreverse result)))

(defun drover-search--candidates (query)
  "Return the names of the candidates of the search in progress for QUERY.
The text typed changing cancels what the backends search, and searches
it anew once typing pauses."
  (when-let* ((state (drover-search--live)))
    (drover-search--typed state query)
    (mapcar #'drover-search--entry-name (drover-search-entries state query))))

;;;; Searching what was said

(defun drover-search-sources ()
  "Return the backends searching what was said in their sessions.
Those registered with a function as :search."
  (seq-filter (lambda (backend) (functionp (drover-backend-property backend :search)))
              (drover-backends)))

(defun drover-search--resolve (state match)
  "Return the entry of STATE the session MATCH names, or nil.
MATCH is a plist naming a session by :key, its `drover-session-key',
or by :engine and :id, and :host for a session of another host."
  (let ((key (plist-get match :key))
        (engine (plist-get match :engine))
        (id (plist-get match :id))
        (host (plist-get match :host)))
    (or (and (consp key) (gethash key (drover-search--state-keys state)))
        (and engine id
             (gethash (if host (cons host (cons engine id)) (cons engine id))
                      (drover-search--state-engines state))))))

(defun drover-search--scope (state)
  "Return the scope the backends search within for STATE, a plist."
  (list :filter drover-filter
        :history drover-show-history
        :archived drover-show-archived
        :limit drover-search-text-limit
        :wanted (let ((link (drover-link state)))
                  (lambda (match)
                    (when-let* ((state (drover-linked link)))
                      (and (drover-search--resolve state match) t))))))

(defun drover-search--cancel (state)
  "Stop the backends searching for STATE, and forget their answers to come."
  (cl-incf (drover-search--state-generation state))
  (pcase-dolist (`(,backend . ,cancel) (drover-search--state-runs state))
    (when (functionp cancel)
      (drover-with-reporting (format "search of %s" backend)
        (funcall cancel))))
  (setf (drover-search--state-runs state) nil)
  (when (timerp (drover-search--state-delay-timer state))
    (cancel-timer (drover-search--state-delay-timer state))
    (setf (drover-search--state-delay-timer state) nil)))

(defun drover-search--keep-hits (state query)
  "Keep the hits of STATE whose snippets hold QUERY, a text typed further.
Those no longer matching are forgotten."
  (let ((hits (drover-search--state-hits state))
        (needle (downcase query))
        (gone nil))
    (maphash (lambda (key hit)
               (unless (and (not (string-empty-p needle))
                            (string-search needle (downcase (or (plist-get hit :snippet) ""))))
                 (push key gone)))
             hits)
    (dolist (key gone)
      (remhash key hits))))

(defun drover-search--typed (state query)
  "Note that QUERY is typed for STATE, asking the backends anew once it pauses."
  (let ((query (string-trim (or query ""))))
    (unless (equal query (drover-search--state-query state))
      (setf (drover-search--state-query state) query
            (drover-search--state-words state) (drover-search--words query))
      (unless (equal query (drover-search--state-text-query state))
        (drover-search--cancel state)
        (drover-search--keep-hits state query)
        (setf (drover-search--state-text-query state) nil)
        (when (and (>= (length query) drover-search-text-minimum) (drover-search-sources))
          (let ((generation (drover-search--state-generation state)))
            (setf (drover-search--state-delay-timer state)
                  (run-with-timer drover-search-delay nil
                                  #'drover-search--start-later generation))))
        (drover-search--show-status state)))))

(defun drover-search--start-later (generation)
  "Ask the backends for the search in progress, unless typed on since GENERATION."
  (drover-with-reporting "search"
    (when-let* ((state (drover-search--live)))
      (when (= generation (drover-search--state-generation state))
        (setf (drover-search--state-delay-timer state) nil)
        (drover-search--start state (drover-search--state-query state))))))

(defun drover-search--start (state query)
  "Ask every backend searching what was said for QUERY, for STATE.
Those failed before in this search are not asked again."
  (drover-search--cancel state)
  (setf (drover-search--state-text-query state) query
        (drover-search--state-received state) 0
        (drover-search--state-asked state) 0
        (drover-search--state-outside state) 0)
  (let ((generation (drover-search--state-generation state))
        (scope (drover-search--scope state)))
    (setf (drover-search--state-starting state) t)
    (dolist (backend (drover-search-sources))
      (unless (assq backend (drover-search--state-failed state))
        ;; Listed first, so that a backend answering at once, before it
        ;; returns, ends its run.
        (let ((run (list backend)))
          (push run (drover-search--state-runs state))
          (let ((cancel (condition-case err
                            (funcall (drover-backend-property backend :search) query scope
                                     (drover-search--receiver state generation backend))
                          (error (drover-search--fail state backend (drover-error-message err))
                                 nil))))
            (cond ((not (memq run (drover-search--state-runs state)))
                   (when (functionp cancel)
                     (drover-with-reporting (format "search of %s" backend)
                       (funcall cancel))))
                  (cancel (setcdr run cancel)
                          (cl-incf (drover-search--state-asked state)))
                  (t (setf (drover-search--state-runs state)
                           (delq run (drover-search--state-runs state)))))))))
    (setf (drover-search--state-starting state) nil))
  (drover-search--show-status state)
  (drover-search--maybe-done state))

(defun drover-search--receiver (state generation backend)
  "Return the function BACKEND reports to, for run GENERATION of STATE.
It holds STATE through a link (see `drover-link'), so that it prints
as a name: what comes for a run since replaced is dropped."
  (let ((link (drover-link state)))
    (lambda (result)
      (drover-with-reporting (format "search of %s" backend)
        (when-let* ((state (drover-linked link))
                    ((drover-search--live state)))
          (when (= generation (drover-search--state-generation state))
            (drover-search--receive state backend result)))))))

(defun drover-search--receive (state backend result)
  "Take RESULT of BACKEND into STATE.
RESULT is a list of matches, the symbol `done', or (:failure MESSAGE)."
  (cond
   ((eq result 'done)
    (setf (drover-search--state-runs state)
          (assq-delete-all backend (drover-search--state-runs state)))
    (drover-search--show-status state)
    (drover-search--maybe-done state))
   ((and (consp result) (eq (car result) :failure))
    (drover-search--fail state backend (or (drover-bound-line (format "%s" (cadr result))
                                                              drover-error-length)
                                           "failed")))
   ((listp result)
    (let ((hits (drover-search--state-hits state))
          (added nil))
      (dolist (match result)
        (when (and (listp match)
                   (< (drover-search--state-received state) drover-search-text-limit))
          (cl-incf (drover-search--state-received state))
          (if-let* ((entry (drover-search--resolve state match)))
              (let* ((key (drover-search--entry-key entry))
                     (hit (gethash key hits)))
                (if hit
                    (plist-put hit :count (1+ (plist-get hit :count)))
                  (setq added t)
                  (puthash key (list :snippet (drover-bound-line (plist-get match :snippet)
                                                                 drover-search-snippet-length)
                                     :count 1 :backend backend
                                     :position (plist-get match :position))
                           hits)))
            (cl-incf (drover-search--state-outside state)))))
      ;; Enough: the backends stop, what they found staying.
      (when (>= (drover-search--state-received state) drover-search-text-limit)
        (drover-search--cancel state))
      (when added
        (drover-search--refresh-soon state))
      (drover-search--show-status state)
      (drover-search--maybe-done state)))))

(defun drover-search--fail (state backend message)
  "Note that BACKEND failed to search for STATE, telling MESSAGE once.
It is not asked again in this search."
  (unless (assq backend (drover-search--state-failed state))
    (push (cons backend message) (drover-search--state-failed state))
    (let ((inhibit-message (and (drover-search--state-minibuffer state) t)))
      (drover-message "Drover: %s could not search: %s" (drover-backend-label backend) message)))
  (setf (drover-search--state-runs state)
        (assq-delete-all backend (drover-search--state-runs state)))
  (drover-search--show-status state)
  (drover-search--maybe-done state))

(defun drover-search--maybe-done (state)
  "Call the function waiting for STATE's backends once none runs."
  (when (and (null (drover-search--state-runs state))
             (null (drover-search--state-delay-timer state))
             (not (drover-search--state-starting state))
             (drover-search--state-done state))
    (let ((done (drover-search--state-done state)))
      (setf (drover-search--state-done state) nil)
      (funcall done state))))

;;;; Showing what comes

(defun drover-search--refresh-soon (state)
  "Show the candidates of STATE again in a moment, gathering more matches."
  (when (and (buffer-live-p (drover-search--state-minibuffer state))
             (not (timerp (drover-search--state-refresh-timer state))))
    (setf (drover-search--state-refresh-timer state)
          (run-with-timer drover-search-refresh-delay nil #'drover-search--refresh))))

(defun drover-search--refresh ()
  "Show the candidates of the search in progress again."
  (drover-with-reporting "search"
    (when-let* ((state (drover-search--live)))
      (setf (drover-search--state-refresh-timer state) nil)
      (drover--refresh-completion (drover-search--state-minibuffer state)))))

(defun drover-search--status-text (state)
  "Return how the backends search for STATE, for the minibuffer, or nil."
  (let ((parts nil)
        (hits (hash-table-count (drover-search--state-hits state))))
    (cond ((drover-search--state-runs state)
           (push (if (> hits 0) (format "%d in text, searching…" hits) "searching text…") parts))
          ((and (drover-search--state-text-query state) (> (drover-search--state-asked state) 0))
           (push (format "%d in text" hits) parts)))
    (when (> (drover-search--state-outside state) 0)
      (push (format "%d out of scope" (drover-search--state-outside state)) parts))
    (pcase-dolist (`(,backend . ,_) (drover-search--state-failed state))
      (push (format "%s failed" (drover-backend-label backend)) parts))
    (and parts (concat "[" (string-join (nreverse parts) ", ") "] "))))

(defun drover-search--show-status (state)
  "Tell in the minibuffer of STATE how the backends' search stands."
  (let ((minibuffer (drover-search--state-minibuffer state)))
    (when (buffer-live-p minibuffer)
      (with-current-buffer minibuffer
        (let ((overlay (drover-search--state-overlay state))
              (text (drover-search--status-text state)))
          (unless (overlayp overlay)
            (setq overlay (make-overlay (minibuffer-prompt-end) (minibuffer-prompt-end) nil t t))
            (setf (drover-search--state-overlay state) overlay))
          (move-overlay overlay (minibuffer-prompt-end) (minibuffer-prompt-end))
          (overlay-put overlay 'before-string (and text (propertize text 'face 'drover-dim))))))))

;;;; Sessions changing while searching

(defun drover-search--changed ()
  "Follow the sessions changed in the search in progress.
On `drover-changed-hook' while a search is in progress.  A session
changed takes its new record and texts in place, its place staying, so
that candidates do not move under the user's eyes; sessions coming or
going, or anything having changed, make the candidates anew in a
moment (see `drover-search-rebuild-delay')."
  (drover-with-reporting "search"
    (when-let* ((state (drover-search--live)))
      (let ((changed drover-changed-sessions)
            (keys (drover-search--state-keys state))
            (backends (make-hash-table :test #'eq))
            (anew (eq drover-changed-sessions t))
            (updated nil))
        (unless anew
          (dolist (key changed)
            (let ((entry (gethash key keys))
                  (record (drover-find (car key) (cdr key))))
              (cond ((and entry record)
                     (setf (drover-search--entry-session entry) record
                           (drover-search--entry-haystack entry)
                           (drover-search--haystack record (drover-search--texts record backends))
                           updated t))
                    ((or entry record) (setq anew t))))))
        (when (and anew (not (timerp (drover-search--state-rebuild-timer state))))
          (setf (drover-search--state-rebuild-timer state)
                (run-with-timer drover-search-rebuild-delay nil #'drover-search--rebuild)))
        (when updated
          (drover-search--refresh-soon state))))))

(defun drover-search--rebuild ()
  "Make the candidates of the search in progress anew, and show them.
The text typed and what the backends found stay."
  (drover-with-reporting "search"
    (when-let* ((state (drover-search--live)))
      (setf (drover-search--state-rebuild-timer state) nil)
      (let ((fresh (drover-search--build)))
        (setf (drover-search--state-entries state) (drover-search--state-entries fresh)
              (drover-search--state-names state) (drover-search--state-names fresh)
              (drover-search--state-keys state) (drover-search--state-keys fresh)
              (drover-search--state-engines state) (drover-search--state-engines fresh)
              (drover-search--state-widths state) (drover-search--state-widths fresh)))
      (drover--refresh-completion (drover-search--state-minibuffer state)))))

;;;; Completion

(defun drover-search--entry-named (name)
  "Return the entry of the search in progress named NAME, or nil."
  (when-let* ((state (drover-search--live)))
    (gethash name (drover-search--state-names state))))

(defun drover-search--group (name transform)
  "Return the title of the group of candidate NAME, or NAME if TRANSFORM."
  (if transform
      name
    (when-let* ((entry (drover-search--entry-named name)))
      (drover-search--entry-group entry))))

(defun drover-search--pad (string width)
  "Return STRING cut or padded with spaces to WIDTH columns."
  (let ((string (if (> (string-width string) width)
                    (truncate-string-to-width string width nil nil "…")
                  string)))
    (concat string (make-string (max 0 (- width (string-width string))) ?\s))))

(defun drover-search--highlight-words (string words face)
  "Give the occurrences of WORDS in STRING FACE, destructively, and return it."
  (let ((case-fold-search t))
    (dolist (word words)
      (let ((start 0)
            (regexp (regexp-quote word)))
        (while (and (< start (length string)) (string-match regexp string start))
          (add-face-text-property (match-beginning 0) (match-end 0) face nil string)
          (setq start (max (1+ start) (match-end 0)))))))
  string)

(defun drover-search--snippet (hit words)
  "Return the snippet of HIT for an annotation, WORDS highlighted."
  (let ((snippet (copy-sequence (or (plist-get hit :snippet) "")))
        (count (plist-get hit :count)))
    (concat (drover-search--highlight-words
             (propertize snippet 'face 'completions-annotations) words 'completions-common-part)
            (if (> count 1) (propertize (format " (+%d)" (1- count)) 'face 'drover-dim) ""))))

(defun drover-search--affixation (names)
  "Return NAMES with their prefixes and suffixes, as the search shows them.
Before a name, the status of its session, its harness, and an arrow
for a subagent; after, its project and branch, its host when of another
host, its age, and the snippet of a match of what was said, else why it
has its status."
  (if-let* ((state (drover-search--live)))
      (let* ((widths (drover-search--state-widths state))
             (hits (drover-search--state-hits state))
             (words (drover-search--state-words state))
             (now (float-time)))
        (mapcar
         (lambda (name)
           (if-let* ((entry (gethash name (drover-search--state-names state))))
               (let* ((session (drover-search--entry-session entry))
                      (host (drover-session-host session))
                      (hit (gethash (drover-search--entry-key entry) hits))
                      (summary (drover-session-summary session)))
                 (list name
                       (concat (opencode-session-status-glyph (drover-session-status session)) " "
                               (drover-harness-glyph (drover-session-harness session)) " "
                               (if (> (drover-search--entry-depth entry) 0) (drover-face "↳ " 'drover-dim) ""))
                       (concat (make-string (max 0 (- (car widths) (string-width name))) ?\s)
                               "  "
                               (drover-face (drover-search--pad (or (drover-session-location session) "")
                                                                (cdr widths))
                                            'drover-dim)
                               (if host (concat "  " (drover-face (drover-host-label host) 'drover-dim)) "")
                               "  "
                               (drover-face (string-pad (opencode-format-age
                                                         (or (drover-session-since session)
                                                             (drover-session-updated session))
                                                         now)
                                                        3 nil t)
                                            'drover-dim)
                               (cond (hit (concat "  " (drover-search--snippet hit words)))
                                     (summary
                                      (concat "  " (propertize (or (drover-bound-line
                                                                    summary drover-search-snippet-length)
                                                                   "")
                                                               'face 'completions-annotations)))
                                     (t "")))))
             (list name "" "")))
         names))
    (mapcar (lambda (name) (list name "" "")) names)))

(defun drover-search--table (string predicate action)
  "Complete STRING with PREDICATE for ACTION in the search in progress.
Its candidates are those of `drover-search-entries' for STRING, the
text typed, matched as a whole: the completion style `drover-search'
offers them as they are."
  (pcase action
    ('metadata
     `(metadata (category . drover-session)
                (display-sort-function . identity)
                (cycle-sort-function . identity)
                (group-function . ,(and drover-group-by #'drover-search--group))
                (affixation-function . drover-search--affixation)))
    (`(boundaries . ,_) nil)
    ('lambda (and (drover-search--entry-named string) t))
    (_ (let ((names (seq-filter (lambda (name) (or (null predicate) (funcall predicate name)))
                                (drover-search--candidates string))))
         (if (eq action t)
             names
           (cond ((null names) nil)
                 ((and (null (cdr names)) (equal (car names) string)) t)
                 ((null (cdr names)) (car names))
                 (t string)))))))

(defun drover-search--highlight (name)
  "Highlight in NAME, destructively, the words typed in the search in progress."
  (if-let* ((state (drover-search--live)))
      (drover-search--highlight-words name (drover-search--state-words state) 'completions-common-part)
    name))

(defun drover-search--try-completion (string table predicate point)
  "Complete STRING in TABLE with PREDICATE, POINT in it, as `drover-search' does.
The text typed is not a prefix: it completes only to the one candidate
it matches."
  (let ((completion (try-completion string table predicate)))
    (if (stringp completion)
        (cons completion (if (equal completion string) point (length completion)))
      completion)))

(defun drover-search--all-completions (string table predicate _point)
  "Return the candidates of TABLE for STRING with PREDICATE, highlighted.
As the style `drover-search': the table matches STRING as a whole."
  (when-let* ((names (all-completions string table predicate)))
    (if (bound-and-true-p completion-lazy-hilit)
        (setq completion-lazy-hilit-fn #'drover-search--highlight)
      (setq names (mapcar (lambda (name) (drover-search--highlight (copy-sequence name))) names)))
    (nconc names 0)))

(add-to-list 'completion-styles-alist
             '(drover-search drover-search--try-completion drover-search--all-completions
                             "Match drover's sessions as `drover-search' does."))

;;;; The command

(defvar-keymap drover-search-minibuffer-map
  :doc "Keys of the minibuffer of `drover-search'."
  "C-l" #'drover-search-locate
  "C-'" #'drover-search-scope
  "<remap> <drover-dispatch>" #'drover-search-scope)

(defun drover-search-scope ()
  "Leave the search for the menu of what it searches, keeping the text typed.
The menu sets the view settings the list shares, which scope the
search; its RET, or C-', searches again with the text."
  (interactive)
  (setq drover-search--exit 'scope)
  (exit-minibuffer))

(defun drover-search-locate ()
  "Show the row of the session chosen in drover's list, rather than open it."
  (interactive)
  (setq drover-search--exit 'locate)
  (let ((command (key-binding (kbd "RET"))))
    (call-interactively (if (commandp command) command #'exit-minibuffer))))

(defun drover-search--setup (state)
  "Set the current buffer, the minibuffer, up for searching STATE."
  (setf (drover-search--state-minibuffer state) (current-buffer))
  (use-local-map (make-composed-keymap drover-search-minibuffer-map (current-local-map)))
  (drover-search--show-status state))

(defun drover-search--prompt ()
  "Return the prompt of the search, telling what scopes it."
  (let ((scope (delq nil (list (drover-filter-description)
                               (and drover-show-history "+older")
                               (and drover-show-archived "+archived")))))
    (format "Search sessions%s: "
            (if scope (format " (%s)" (string-join scope ", ")) ""))))

(defun drover-search--finish (state)
  "End STATE: its backends stop, its timers and its overlay go."
  (drover-search--cancel state)
  (dolist (timer (list (drover-search--state-refresh-timer state)
                       (drover-search--state-rebuild-timer state)))
    (when (timerp timer)
      (cancel-timer timer)))
  (setf (drover-search--state-refresh-timer state) nil
        (drover-search--state-rebuild-timer state) nil)
  (when (overlayp (drover-search--state-overlay state))
    (delete-overlay (drover-search--state-overlay state)))
  (setf (drover-search--state-overlay state) nil
        (drover-search--state-minibuffer state) nil))

(defun drover-search--chosen (state input)
  "Return the entry of STATE INPUT chooses, or nil.
A candidate's name chooses its entry; another text, the first
candidate it matches."
  (or (gethash input (drover-search--state-names state))
      (car (drover-search-entries state input))))

(defun drover-search--session (entry)
  "Return the record of the session of ENTRY as its backend has it now."
  (let ((session (drover-search--entry-session entry)))
    (or (drover-find (drover-session-backend session) (drover-session-id session))
        session)))

;;;###autoload
(defun drover-search (&optional initial other-window)
  "Search the sessions, and open the one chosen.
The sessions offered are those the list shows, in its order, scoped by
the view settings it shares: `drover-filter', the older and archived
sessions while shown, the subagents shown.  Those matching every word
typed in their title, summary, project, branch, or texts their backends
tell, such as the prompts of Claude Code's conversations, show as you
type; once typing pauses, the backends that can search what was said
in their conversations, those of `drover-search-sources', add the
sessions where they found the text, a snippet beside them.

RET opens the session chosen as RET in the list does, in another
window with a prefix argument OTHER-WINDOW;
\\<drover-search-minibuffer-map>\\[drover-search-locate] shows its row in the list instead.
\\[drover-search-scope], or the key of `drover-dispatch', leaves for the
menu of the settings scoping the search, on the keys the list's menu
gives them, from which RET searches again.  INITIAL is the text to
start with."
  (interactive (list nil current-prefix-arg))
  (drover-start)
  (when-let* ((state (drover-search--live)))
    (if (buffer-live-p (drover-search--state-minibuffer state))
        (user-error "A search of sessions is in progress")
      (drover-search--finish state)))
  (let* ((state (drover-search--build))
         ;; No other style: one finding nothing would ask the table for
         ;; other texts than the one typed.
         (completion-styles '(drover-search))
         (completion-category-overrides (cons '(drover-session (styles drover-search))
                                              completion-category-overrides))
         (exit nil)
         (input nil)
         (entry nil))
    (setq drover-search--current state)
    (add-hook 'drover-changed-hook #'drover-search--changed)
    (unwind-protect
        (let ((drover-search--exit nil))
          (setq input (minibuffer-with-setup-hook (lambda () (drover-search--setup state))
                        (completing-read (drover-search--prompt) #'drover-search--table nil nil
                                         initial 'drover-search-history))
                exit drover-search--exit)
          (unless (eq exit 'scope)
            (setq entry (drover-search--chosen state input))))
      (drover-search--finish state)
      (remove-hook 'drover-changed-hook #'drover-search--changed)
      (setq drover-search--current nil))
    (cond ((eq exit 'scope) (drover-search-menu input))
          ((null entry) (user-error "No session matches %s" input))
          ((eq exit 'locate) (drover-list-reveal (drover-search--session entry)))
          (t (drover-open (drover-search--session entry) other-window)))))

;;;; The menu of what it searches

(defun drover-search--menu-input ()
  "Return the text the search menu searches again with."
  (or (transient-scope 'drover-search-menu) ""))

(defun drover-search--menu-description ()
  "Return the title of the search menu's first group, naming the text."
  (let ((input (drover-search--menu-input)))
    (if (string-empty-p input)
        "Search"
      (concat "Search " (propertize (drover--menu-value (format "\"%s\"" input) 40)
                                    'face 'transient-value)))))

(defvar drover-search--menu-keys nil
  "The keys searching again from the search menu, besides RET.
C-' and the keys of `drover-dispatch', which open the menu from the
minibuffer of the search.")

(defun drover-search--menu-return-entries (_children)
  "Return the entries of the search menu searching again on its other keys.
See `drover-search--menu-keys'."
  (transient-parse-suffixes 'drover-search-menu
                            (mapcar (lambda (key)
                                      (list (key-description key) "Search" 'drover-search-resume))
                                    drover-search--menu-keys)))

(defun drover-search-resume ()
  "Search again, with the text typed before the menu opened."
  (interactive)
  (drover-search (drover-search--menu-input)))

(transient-define-prefix drover-search-menu (&optional input)
  "Choose what the search of sessions searches, then search INPUT again.
The entries are the view settings of `drover-dispatch', on the same
keys: the filter, the older and archived sessions, the subagents shown.
They scope the list too, which shares them.  RET, C-' or the key of
`drover-dispatch', those leaving the minibuffer for it, search again
with the text typed."
  :class 'drover--menu-prefix
  :refresh-suffixes t
  [:class drover--menu-flow
   [:description drover-search--menu-description
    ("RET" "Search" drover-search-resume)
    ("U" "subagents" drover-toggle-subagents
     :class drover--setting
     :getter (lambda () (default-value 'drover-show-subagents))
     :setter drover-set-show-subagents
     :chooser drover-next-subagent-mode
     :options ,drover-subagent-mode-labels)
    ("-o" drover-view--history :inapt-if-not drover-history-available-p)
    ("-z" drover-view--archived :inapt-if-not drover-archived-available-p)]
   ["Filter"
    ("=p" drover-view--filter-project)
    ("=s" drover-view--filter-statuses)
    ("=a" drover-view--filter-agent)
    ("=m" drover-view--filter-model)
    ("=v" drover-view--filter-provider)]
   [("=H" drover-view--filter-harness)
    ("=h" drover-view--filter-host)
    ("=d" drover-view--filter-since)
    ("=f" drover-view--filter-search)
    ("=c" "clear" drover-filter-clear :transient t)]]
  ;; The keys leaving the minibuffer for the menu come back.
  [:class transient-column
   :hide always
   :setup-children drover-search--menu-return-entries]
  (interactive)
  (setq drover-search--menu-keys (delete-dups (cons (kbd "C-'") (drover--dispatch-own-keys))))
  (transient-setup 'drover-search-menu nil nil :scope (or input "")
                   :display-action drover-menu-display-action))

;;;; Without the minibuffer

(defun drover-search-sessions (query)
  "Return the sessions matching QUERY, in the list's order.
They are those `drover-search' offers as QUERY is typed, before the
backends search what was said."
  (mapcar #'drover-search--entry-session (drover-search-entries (drover-search--build) query)))

(defun drover-search-text (query callback)
  "Search what was said in the sessions for QUERY, then call CALLBACK.
Every backend of `drover-search-sources' is asked, as `drover-search'
asks them, at once.  CALLBACK gets a plist, once all of them finished:
:sessions, a list of (SESSION SNIPPET COUNT) in the list's order;
:outside, how many matches named sessions the search does not offer;
:failed, an alist (BACKEND . MESSAGE).  Return a function stopping the
search, after which CALLBACK is not called.  One search runs at a time:
a new one stops the one before."
  (when-let* ((old (drover-search--live drover-search--programmatic)))
    (drover-search--finish old))
  (let ((state (drover-search--build)))
    (setf (drover-search--state-done state)
          (lambda (state)
            (when (eq state drover-search--programmatic)
              (setq drover-search--programmatic nil))
            (let ((hits (drover-search--state-hits state))
                  (found nil))
              (seq-doseq (entry (drover-search--state-entries state))
                (when-let* ((hit (gethash (drover-search--entry-key entry) hits)))
                  (push (list (drover-search--entry-session entry) (plist-get hit :snippet)
                              (plist-get hit :count))
                        found)))
              (funcall callback (list :sessions (nreverse found)
                                      :outside (drover-search--state-outside state)
                                      :failed (drover-search--state-failed state))))))
    (setq drover-search--programmatic state)
    (drover-search--start state (string-trim query))
    ;; Held through a link: the caller keeps it, and Emacs prints it, as
    ;; the server prints a value evaluated, whole otherwise.
    (let ((link (drover-link state)))
      (lambda ()
        (when-let* ((state (drover-linked link)))
          (when (eq drover-search--programmatic state)
            (setq drover-search--programmatic nil))
          (setf (drover-search--state-done state) nil)
          (drover-search--finish state))))))

(defun drover-search-unload-function ()
  "Stop the searches in progress and forget the completion style, unloading."
  (dolist (state (list (drover-search--live) (drover-search--live drover-search--programmatic)))
    (when state
      (drover-search--finish state)))
  (setq drover-search--current nil
        drover-search--programmatic nil)
  (remove-hook 'drover-changed-hook #'drover-search--changed)
  (setq completion-styles-alist (assq-delete-all 'drover-search completion-styles-alist))
  nil)

(provide 'drover-search)
;;; drover-search.el ends here
