;;; drover-transcript.el --- Conversations engines record in files  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; Author: Valentin Leon <valentin@leon.click>
;; Keywords: tools

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

;; Engines such as Claude Code and Codex record each conversation in a
;; file of JSON lines, which they append to as it goes on: a user may
;; have thousands of them, some of megabytes.  A `drover-transcript-index'
;; keeps what drover reads of each, by file, as of its size,
;; modification time and inode, so that a file is read again only once
;; it changed, and then only as much as the engine's reader needs: the
;; head and the tail, or what was appended since.
;;
;; The index finds what changed by listing its directories again every
;; `drover-transcript-scan-interval' seconds, a file status per file,
;; in slices of a few milliseconds run from timers, stopping as input
;; comes: file notifications would tell less, as macOS's kqueue reports
;; the entries of a directory coming and going, not a file appended to,
;; and watching each file would take a descriptor per conversation.  A
;; file a backend has reason to look at, such as a conversation whose
;; terminal just closed, is looked at first (`drover-transcript-check').
;;
;; What the index read is kept between Emacs sessions in
;; `drover-state-directory', so that an Emacs starting reads only what
;; changed meanwhile: the file is a cache, checked against every file's
;; status, and ignored when unreadable.

;;; Code:

(require 'cl-lib)
(require 'iso8601)
(require 'project)
(require 'seq)
(require 'subr-x)
(require 'drover-core)

;; Reloaded into an Emacs running an older drover, load its core anew.
(unless (and (boundp 'drover-core-generation) (>= drover-core-generation 1))
  (load "drover-core" nil t))

(defcustom drover-transcript-scan-interval 15
  "Seconds between two looks at the files engines record conversations in.
Each look lists their directories, a file status per conversation, in
slices that give way to input; a file found changed is read again."
  :type 'number
  :group 'drover)

(defconst drover-transcript-slice 0.008
  "Seconds a slice of the index's work takes at most.")

(defconst drover-transcript-slice-delay 0.05
  "Seconds between two slices of the index's work.")

(defconst drover-transcript-full-look-every 8
  "How many looks of an index make one that checks every file.
The others check the files that changed in the last
`drover-transcript-hot-age' seconds, and list the names of the rest,
to find those coming and going: a conversation of days ago seldom goes
on, and its file status costs as much as a recent one's.")

(defconst drover-transcript-hot-age 86400
  "Seconds since its last change a file is checked at every look.")

(defconst drover-transcript-save-interval 300
  "Seconds at least between two writes of what the index read.
The file is a cache, checked against the files: written seldom, it
costs little, and is written again as Emacs exits.")

(defconst drover-transcript-window 65536
  "Bytes of the head and of the tail of a file engines' readers read.
Claude Code's own list of conversations reads as much.")

(defconst drover-transcript-text-limit 300
  "Characters of a title or prompt kept of a conversation.")

;;;; Entries

(cl-defstruct (drover-transcript (:constructor drover-transcript--create)
                                 (:copier nil))
  "What drover read of a file recording a conversation."
  (file nil :documentation "The file's absolute name.")
  (size nil :documentation "Its size when read, in bytes.")
  (mtime nil :documentation "Its modification time when read, as a float.")
  (inode nil :documentation "Its inode number when read, which a file replaced changes.")
  (data nil :documentation "What the engine's reader read of it, a plist, or nil."))

(drover-note-layout 'drover-transcript)

(cl-defstruct (drover-transcript-index (:constructor drover-transcript-index--create)
                                       (:copier nil))
  "The files recording an engine's conversations, and what was read of each."
  ;; First, whatever the version: see `drover-layout-current-p'.
  (stamp (drover-layout 'drover-transcript-index 'drover-transcript)
          :documentation "The layouts of the types it was made of, as `drover-layout' tells them.")
  (name nil :documentation "A symbol naming the index, and the file it is kept in.")
  (roots nil :documentation "Function of no argument returning where the files are.
A list of (DIRECTORY DEPTH REGEXP): the files whose names match REGEXP
in the directories DEPTH levels below DIRECTORY, 0 for DIRECTORY
itself.")
  (read nil :documentation "Function of a FILE, its ATTRIBUTES and the ENTRY read before, or nil.
It returns what it reads of FILE, a plist of Lisp data, or nil.")
  (on-change nil :documentation "Function of a FILE, the DATA read before and the DATA read now.
Called when what was read of FILE changed; the latter is nil once FILE
is gone.")
  (on-cycle nil :documentation "Function of no argument called once every directory was listed.")
  (key nil :documentation "Function of the DATA read of a file returning what names it, or nil.
Its backend finds the file of a conversation by that name, such as its
identifier, with `drover-transcript-keyed-file'.")
  (entries (make-hash-table :test #'equal) :documentation "Entries by file.")
  (files (make-hash-table :test #'equal)
         :documentation "The files of the entries, by what KEY names them.")
  (directories nil :documentation "The directories left to list in this look, as roots are.")
  (queue nil :documentation "The files left to read, first first.")
  (later nil :documentation "The files to read after QUEUE, last first.")
  (queued (make-hash-table :test #'equal) :documentation "The files of QUEUE.")
  (seen nil :documentation "The files listed in this look, a hash table, or nil between looks.")
  (full nil :documentation "Non-nil when this look checks every file.")
  (listings (make-hash-table :test #'equal)
            :documentation "Directories of files as last listed, as (MTIME . FILES), by name.")
  (looks 0 :documentation "How many looks began.")
  (complete t :documentation "Nil when a directory of this look could not be listed.")
  (timer nil :documentation "Timer of the next slice or look.")
  (active nil :documentation "Non-nil while the index looks at its files from time to time.")
  (loaded nil :documentation "Non-nil once what was kept of the index was read.")
  (dirty nil :documentation "Non-nil when entries changed since they were kept.")
  (saved 0 :documentation "When the entries were kept last, as a float time.")
  (warned nil :documentation "Non-nil once a problem with the kept entries was reported."))

(drover-note-layout 'drover-transcript-index)

(defun drover-transcript-index-current-p (index)
  "Return non-nil when INDEX is an index of the layouts defined now.
One made before drover's files were loaded anew, of other slots, is to
be dropped: its timers stop, and its backend makes another."
  (drover-layout-current-p index 'drover-transcript-index))

(defun drover-transcript-index-create (name roots read on-change &optional on-cycle key)
  "Return an index NAME of the files ROOTS tells, read with READ.
ON-CHANGE and ON-CYCLE are told of changes and of each look's end, and
KEY names what was read of a file.  See `drover-transcript-index' for
each."
  (drover-transcript-index--create :name name :roots roots :read read
                                   :on-change on-change :on-cycle on-cycle :key key))

(defun drover-transcript-entry (index file)
  "Return the entry INDEX keeps of FILE, or nil."
  (gethash file (drover-transcript-index-entries index)))

(defun drover-transcript-lookup (index file)
  "Return what INDEX read of FILE, or nil."
  (when-let* ((entry (drover-transcript-entry index file)))
    (drover-transcript-data entry)))

(defun drover-transcript-id (data)
  "Return the identifier of the conversation read as DATA, its :id, or nil.
A key function of an index, see `drover-transcript-index'."
  (plist-get data :id))

(defun drover-transcript-keyed-file (index key)
  "Return the file of INDEX whose data its key function names KEY, or nil.
Of two, it is the one read last, or of those kept between Emacs
sessions, the one changed last."
  (gethash key (drover-transcript-index-files index)))

(defun drover-transcript--rekey (index file old new)
  "Note that what was read of FILE of INDEX went from OLD to NEW, for its key.
The table of files by key follows every entry, those read and those
kept between Emacs sessions alike, which the backend is not told of."
  (when-let* ((key (drover-transcript-index-key index)))
    (let ((files (drover-transcript-index-files index))
          (old-key (and old (funcall key old)))
          (new-key (and new (funcall key new))))
      (when (and old-key (not (equal old-key new-key)) (equal (gethash old-key files) file))
        (remhash old-key files))
      (when new-key
        (puthash new-key file files)))))

(defun drover-transcript-map (function index)
  "Call FUNCTION with the file and data of each entry of INDEX that has data."
  (maphash (lambda (file entry)
             (when-let* ((data (drover-transcript-data entry)))
               (funcall function file data)))
           (drover-transcript-index-entries index)))

;;;; What is kept between Emacs sessions

(defconst drover-transcript--version 1
  "Version of the format of the files the indexes are kept in.")

(defun drover-transcript-cache-file (index)
  "Return the file INDEX is kept in, in `drover-state-directory'."
  (expand-file-name (format "%s.eld" (drover-transcript-index-name index)) drover-state-directory))

(defun drover-transcript--warn (index format-string &rest args)
  "Report a problem with the file INDEX is kept in, once.
FORMAT-STRING and ARGS are as for `format-message'."
  (unless (drover-transcript-index-warned index)
    (setf (drover-transcript-index-warned index) t)
    (apply #'drover-warn format-string args)))

(defun drover-transcript--entry-p (entry)
  "Return non-nil when ENTRY is an entry as kept, (FILE SIZE MTIME INODE DATA)."
  (pcase entry
    (`(,(pred stringp) ,(pred natnump) ,(pred numberp) ,_ ,(pred plistp)) t)))

(defun drover-transcript--load-key (index file mtime data)
  "Key FILE of INDEX, kept as DATA read when the file's time was MTIME.
Of two files of one key, the one changed last keeps it."
  (when-let* ((key (drover-transcript-index-key index))
              (name (and data (funcall key data))))
    (let* ((files (drover-transcript-index-files index))
           (other (gethash name files))
           (entry (and other (drover-transcript-entry index other))))
      (unless (and entry (>= (drover-transcript-mtime entry) mtime))
        (puthash name file files)))))

(defun drover-transcript-load (index)
  "Read what was kept of INDEX, unless read already.
Entries read since are kept over those of the file."
  (unless (drover-transcript-index-loaded index)
    (setf (drover-transcript-index-loaded index) t)
    (let ((file (drover-transcript-cache-file index))
          (entries (drover-transcript-index-entries index)))
      (condition-case err
          (let ((kept (drover-read-data file drover-transcript--version :files)))
            (unless (seq-every-p #'drover-transcript--entry-p kept)
              (error "Malformed entries"))
            (pcase-dolist (`(,name ,size ,mtime ,inode ,data) kept)
              (unless (gethash name entries)
                (puthash name (drover-transcript--create :file name :size size :mtime mtime
                                                         :inode inode :data data)
                         entries)
                (drover-transcript--load-key index name mtime data))))
        (error
         (drover-transcript--warn index "Ignoring what drover kept of %s in %s: %s"
                                  (drover-transcript-index-name index)
                                  (abbreviate-file-name file) (drover-error-message err)))))))

(defun drover-transcript-save (index &optional force)
  "Keep the entries of INDEX in its file, when they changed.
Unless FORCE, not before `drover-transcript-save-interval' seconds
passed since they were kept last.  A failure is reported once."
  (when (and (drover-transcript-index-dirty index)
             (or force (> (- (float-time) (drover-transcript-index-saved index))
                          drover-transcript-save-interval)))
    (let ((entries nil)
          (file (drover-transcript-cache-file index)))
      (maphash (lambda (name entry)
                 (push (list name (drover-transcript-size entry) (drover-transcript-mtime entry)
                             (drover-transcript-inode entry) (drover-transcript-data entry))
                       entries))
               (drover-transcript-index-entries index))
      (setf (drover-transcript-index-dirty index) nil
            (drover-transcript-index-saved index) (float-time))
      (condition-case err
          (drover-write-data file (format "What drover read of %s" (drover-transcript-index-name index))
                             drover-transcript--version :files
                             (sort entries :key #'car :in-place t))
        (error (drover-transcript--warn index "Could not keep what drover read of %s in %s: %s"
                                        (drover-transcript-index-name index)
                                        (abbreviate-file-name file) (drover-error-message err)))))))

;;;; Looking at the files

(defun drover-transcript--stamp (attributes)
  "Return (SIZE MTIME INODE) of a file of ATTRIBUTES."
  (list (file-attribute-size attributes)
        (float-time (file-attribute-modification-time attributes))
        (file-attribute-inode-number attributes)))

(defun drover-transcript--current-p (entry attributes)
  "Return non-nil when ENTRY was read from the file as ATTRIBUTES describe it."
  (and entry
       (equal (list (drover-transcript-size entry) (drover-transcript-mtime entry)
                    (drover-transcript-inode entry))
              (drover-transcript--stamp attributes))))

(defun drover-transcript--enqueue (index file &optional first)
  "Queue FILE for INDEX to read, FIRST before the others when non-nil."
  (let ((queued (drover-transcript-index-queued index)))
    (cond ((not (gethash file queued))
           (puthash file t queued)
           (if first
               (push file (drover-transcript-index-queue index))
             (push file (drover-transcript-index-later index))))
          (first
           (setf (drover-transcript-index-queue index)
                 (cons file (delete file (drover-transcript-index-queue index)))
                 (drover-transcript-index-later index)
                 (delete file (drover-transcript-index-later index)))))))

(defun drover-transcript--note (index file attributes)
  "Note FILE of ATTRIBUTES listed by INDEX, queued to read when it changed."
  (when-let* ((seen (drover-transcript-index-seen index)))
    (puthash file t seen))
  (unless (drover-transcript--current-p (drover-transcript-entry index file) attributes)
    (drover-transcript--enqueue index file)))

(defun drover-transcript--hot-p (entry now)
  "Return non-nil when ENTRY, read before, is to be checked at every look at NOW.
That is when its file changed lately, or was never read."
  (or (null entry)
      (> (drover-transcript-mtime entry) (- now drover-transcript-hot-age))))

(defun drover-transcript--list (index directory depth regexp)
  "List DIRECTORY, DEPTH levels above the files of INDEX matching REGEXP.
Files are checked as the look says (see
`drover-transcript-full-look-every'): all of them, or those changed
lately and those new, the others only noted as there, their directory
listed again only once its modification time tells files came or went."
  (condition-case nil
      (cond
       ((> depth 0)
        (dolist (entry (directory-files-and-attributes directory t nil t))
          (let ((name (car entry)))
            (when (and (eq (file-attribute-type (cdr entry)) t)
                       (not (member (file-name-nondirectory name) '("." ".."))))
              (push (list name (1- depth) regexp) (drover-transcript-index-directories index))))))
       ((drover-transcript-index-full index)
        (let ((mtime (file-attribute-modification-time (file-attributes directory)))
              (files nil))
          (dolist (entry (directory-files-and-attributes directory t regexp t))
            (unless (file-attribute-type (cdr entry))
              (push (car entry) files)
              (drover-transcript--note index (car entry) (cdr entry))))
          (puthash directory (cons mtime files) (drover-transcript-index-listings index))))
       (t
        (let* ((listings (drover-transcript-index-listings index))
               (mtime (file-attribute-modification-time (or (file-attributes directory)
                                                            (signal 'file-missing (list directory)))))
               (listing (gethash directory listings))
               (files (if (equal (car listing) mtime)
                          (cdr listing)
                        (cdr (puthash directory (cons mtime (directory-files directory t regexp t))
                                      listings))))
               (now (float-time))
               (seen (drover-transcript-index-seen index)))
          (dolist (name files)
            (if (drover-transcript--hot-p (drover-transcript-entry index name) now)
                (if-let* ((attributes (file-attributes name)))
                    (unless (file-attribute-type attributes)
                      (drover-transcript--note index name attributes))
                  ;; Gone since listed: the next listing tells.
                  (remhash directory listings))
              (when seen
                (puthash name t seen)))))))
    ;; A directory missing holds no file.
    (file-missing (remhash directory (drover-transcript-index-listings index)))
    ;; Entries of a directory not listed are kept: their files may be there.
    (file-error (setf (drover-transcript-index-complete index) nil))))

(defun drover-transcript--change (index file old new)
  "Tell INDEX's backend that what was read of FILE went from OLD to NEW."
  (unless (equal old new)
    (drover-transcript--rekey index file old new)
    (when-let* ((function (drover-transcript-index-on-change index)))
      (drover-with-reporting "transcript change"
        (funcall function file old new)))))

(defun drover-transcript--read (index file)
  "Read FILE again for INDEX, if it changed, and tell what changed."
  (remhash file (drover-transcript-index-queued index))
  (let* ((entries (drover-transcript-index-entries index))
         (entry (gethash file entries))
         (attributes (file-attributes file)))
    (cond
     ((null attributes)
      (when entry
        (remhash file entries)
        (setf (drover-transcript-index-dirty index) t)
        (drover-transcript--change index file (drover-transcript-data entry) nil)))
     ((drover-transcript--current-p entry attributes))
     (t
      (let ((data (condition-case err
                      (funcall (drover-transcript-index-read index) file attributes entry)
                    ;; A file that cannot be read is noted as read, to be
                    ;; read again once it changes, not on every look.
                    ((debug error)
                     (drover-report-failure (format "reading %s" (drover-transcript-index-name index)) err
                                          (abbreviate-file-name file))
                     nil))))
        (pcase-let ((`(,size ,mtime ,inode) (drover-transcript--stamp attributes)))
          (puthash file (drover-transcript--create :file file :size size :mtime mtime
                                                   :inode inode :data data)
                   entries))
        (setf (drover-transcript-index-dirty index) t)
        (drover-transcript--change index file (and entry (drover-transcript-data entry)) data))))))

(defun drover-transcript--begin (index)
  "Begin a look of INDEX at its files, unless one is under way.
It checks every file when its turn came (see
`drover-transcript-full-look-every'), the first look among them."
  (unless (drover-transcript-index-seen index)
    (setf (drover-transcript-index-full index)
          (zerop (% (drover-transcript-index-looks index) drover-transcript-full-look-every)))
    (cl-incf (drover-transcript-index-looks index))
    (setf (drover-transcript-index-seen index) (make-hash-table :test #'equal)
          (drover-transcript-index-complete index) t
          (drover-transcript-index-directories index)
          (copy-sequence (drover-with-reporting "transcript roots"
                           (funcall (drover-transcript-index-roots index)))))))

(defun drover-transcript--finish (index)
  "End the look of INDEX: the files it did not list are gone."
  (let ((seen (drover-transcript-index-seen index))
        (entries (drover-transcript-index-entries index))
        (gone nil))
    (setf (drover-transcript-index-seen index) nil)
    (when (drover-transcript-index-complete index)
      (maphash (lambda (file _) (unless (gethash file seen) (push file gone))) entries)
      (dolist (file gone)
        (let ((entry (gethash file entries)))
          (remhash file entries)
          (setf (drover-transcript-index-dirty index) t)
          (drover-transcript--change index file (drover-transcript-data entry) nil))))
    (when-let* ((function (drover-transcript-index-on-cycle index)))
      (drover-with-reporting "transcript cycle"
        (funcall function)))
    (drover-transcript-save index)))

(defun drover-transcript--step (index)
  "Do one step of the work of INDEX, and return non-nil when work remains.
Files to read come first, then directories to list; with neither, the
look ends."
  (unless (drover-transcript-index-queue index)
    (setf (drover-transcript-index-queue index) (nreverse (drover-transcript-index-later index))
          (drover-transcript-index-later index) nil))
  (cond ((drover-transcript-index-queue index)
         (drover-transcript--read index (pop (drover-transcript-index-queue index)))
         t)
        ((drover-transcript-index-directories index)
         (apply #'drover-transcript--list index (pop (drover-transcript-index-directories index)))
         t)
        ((drover-transcript-index-seen index)
         (drover-transcript--finish index)
         nil)))

(defun drover-transcript--schedule (index delay function)
  "Run FUNCTION with INDEX after DELAY seconds, instead of what was to run."
  (when (timerp (drover-transcript-index-timer index))
    (cancel-timer (drover-transcript-index-timer index)))
  (setf (drover-transcript-index-timer index) (run-at-time delay nil function index)))

(defun drover-transcript--work (index)
  "Work for INDEX for a slice, then schedule the next slice or look.
It runs from a timer: an error is reported briefly, and the next look
comes at its time.  An index of other layouts, made before drover's
files were loaded anew, does nothing more."
  (when (drover-transcript-index-current-p index)
    (setf (drover-transcript-index-timer index) nil)
    (when (drover-transcript-index-active index)
      (let ((more t))
        (unwind-protect
            (drover-with-reporting (format "index of %s" (drover-transcript-index-name index))
              (let ((deadline (+ (float-time) drover-transcript-slice)))
                (while (and (setq more (drover-transcript--step index))
                            (< (float-time) deadline)
                            (not (input-pending-p))))))
          (when (drover-transcript-index-active index)
            (if more
                (drover-transcript--schedule index drover-transcript-slice-delay #'drover-transcript--work)
              (drover-transcript--schedule index drover-transcript-scan-interval
                                           #'drover-transcript--look))))))))

(defun drover-transcript--look (index)
  "Begin a look of INDEX at its files, and work at it."
  (when (drover-transcript-index-current-p index)
    (drover-with-reporting (format "index of %s" (drover-transcript-index-name index))
      (drover-transcript--begin index))
    (drover-transcript--work index)))

(defun drover-transcript-start (index)
  "Read what was kept of INDEX, and look at its files from now on.
The first look begins at once, in slices; the next ones every
`drover-transcript-scan-interval' seconds."
  (drover-transcript-load index)
  (unless (drover-transcript-index-active index)
    (setf (drover-transcript-index-active index) t)
    (drover-transcript--schedule index 0 #'drover-transcript--look)))

(defun drover-transcript-stop (index)
  "Stop looking at the files of INDEX, and keep what it read."
  (setf (drover-transcript-index-active index) nil)
  (when (timerp (drover-transcript-index-timer index))
    (cancel-timer (drover-transcript-index-timer index)))
  (setf (drover-transcript-index-timer index) nil)
  (drover-transcript-save index t))

(defun drover-transcript-check (index file)
  "Look at FILE of INDEX before anything else, if it changed since it was read.
Return non-nil when it is to be read again.  Its status is all this
costs now; it is read in the next slice, which comes soon, and its
backend told then."
  (let ((attributes (file-attributes file))
        (entry (drover-transcript-entry index file)))
    (when (and (or attributes entry)
               (not (and attributes (drover-transcript--current-p entry attributes))))
      (drover-transcript--enqueue index file t)
      (when (drover-transcript-index-active index)
        (drover-transcript--schedule index drover-transcript-slice-delay #'drover-transcript--work))
      t)))

(defun drover-transcript-look-soon (index)
  "Begin the next look of INDEX at its files soon, rather than at its time.
For a change made to them, such as an engine's command moving a file."
  (when (drover-transcript-index-active index)
    (drover-transcript--begin index)
    (drover-transcript--schedule index drover-transcript-slice-delay #'drover-transcript--work)))

(defun drover-transcript-scan (index &optional quick)
  "Look at every file of INDEX now, reading those that changed, and return.
Slices and timers aside, it is what a full look does, or with QUICK one
checking only the files changed lately (see
`drover-transcript-full-look-every'): for commands and tests that need
the index current."
  (drover-transcript-load index)
  (drover-transcript--begin index)
  (setf (drover-transcript-index-full index) (not quick))
  (while (drover-transcript--step index)))

;;;; Reading files

(defun drover-transcript-insert (file beg end)
  "Insert the bytes BEG to END of FILE, decoded as UTF-8, at point.
A character cut at either end is left undecoded; lines it is on fail
to parse, and readers skip them."
  (let ((coding-system-for-read 'utf-8-unix))
    (insert-file-contents file nil beg end)))

(defun drover-transcript-insert-ends (file size &optional old-size)
  "Insert the head and tail of FILE, of SIZE bytes, into the current buffer.
Return the position where the tail begins.  The head is the first
`drover-transcript-window' bytes; the tail the last ones, or, with
OLD-SIZE, those appended since FILE was OLD-SIZE bytes long, at most
as many: then no head is inserted, and the tail begins at the buffer's
start.  Head and tail are inserted once when they overlap."
  (let ((window drover-transcript-window))
    (if old-size
        (progn (drover-transcript-insert file (max old-size (- size window)) size)
               (point-min))
      (if (<= size (* 2 window))
          (progn (drover-transcript-insert file 0 size)
                 (point-min))
        (drover-transcript-insert file 0 window)
        (goto-char (point-max))
        (insert "\n")
        (let ((tail (point)))
          (drover-transcript-insert file (- size window) size)
          tail)))))

(defun drover-transcript--string-scan (bound)
  "Scan the JSON string whose opening quote is at point, up to BOUND.
Return (END . CLOSED): END where the scan stopped, past the closing
quote when CLOSED is non-nil, else at BOUND, at the end of the line or
of the buffer, the string being cut there.  Point does not move.  A
loop rather than a regexp, whose matcher overflows its stack on
strings of tens of kilobytes."
  (save-excursion
    (forward-char 1)
    (let ((closed nil)
          (done nil))
      (while (not done)
        (skip-chars-forward "^\"\\\\\n" bound)
        (cond ((>= (point) bound) (setq done t))
              ((eq (char-after) ?\\)
               (if (>= (+ (point) 2) bound)
                   (setq done t)
                 (forward-char 2)))
              ((eq (char-after) ?\")
               (forward-char 1)
               (setq closed t done t))
              (t (setq done t))))
      (cons (min (point) bound) closed))))

(defun drover-transcript-string-at ()
  "Return the JSON string at point, decoded, and move past it; nil when none.
Point is at its opening quote."
  (when (eq (char-after) ?\")
    (pcase-let ((`(,end . ,closed) (drover-transcript--string-scan (point-max))))
      (when closed
        (prog1 (ignore-errors (json-parse-string (buffer-substring-no-properties (point) end)))
          (goto-char end))))))

(defun drover-transcript-string-prefix (&optional limit)
  "Return the start of the JSON string at point, decoded, or nil.
Point is at its opening quote.  Unlike `drover-transcript-string-at',
the string may be cut, by the window it was read in, and only its first
LIMIT characters are decoded, by default `drover-text-limit': a prompt
pasting a log is a line of hundreds of kilobytes, longer than any
window.  Point does not move."
  (when (eq (char-after) ?\")
    (let* ((limit (or limit drover-text-limit))
           ;; An escaped character takes up to six.
           (bound (min (point-max) (+ (point) 1 (* 6 limit))))
           (scan (drover-transcript--string-scan bound))
           (text (buffer-substring-no-properties (point) (car scan))))
      (unless (cdr scan)
        ;; What a cut leaves: half an escape, bytes of half a character.
        (setq text (replace-regexp-in-string
                    (rx (or (seq "\\" (? "u" (** 0 3 xdigit))) (+ (any (#x3fff80 . #x3fffff)))) eos)
                    "" text t t))
        (setq text (concat text "\"")))
      (let ((decoded (ignore-errors (json-parse-string text))))
        ;; A high surrogate cut from its pair does not decode: drop it.
        (when (and (not decoded) (not (cdr scan))
                   (string-match "\\\\u[dD][89abAB][[:xdigit:]]\\{2\\}\"\\'" text))
          (setq decoded (ignore-errors
                          (json-parse-string (concat (substring text 0 (match-beginning 0)) "\"")))))
        (and (stringp decoded)
             (if (> (length decoded) limit) (substring decoded 0 limit) decoded))))))

(defun drover-transcript-sniff (key &optional backward bound)
  "Return the next string value of KEY in the current buffer, from point.
Search BACKWARD when non-nil, up to BOUND.  KEY is a JSON member name,
whose value is found without parsing the line it is on, as the engines'
own readers do, so that lines too long to parse whole still tell it.
Point moves to the match, or stays when there is none."
  (let ((regexp (concat "\"" (regexp-quote key) "\":[ \t]*\""))
        (start (point))
        (value nil))
    (while (and (not value)
                (if backward
                    (re-search-backward regexp bound t)
                  (re-search-forward regexp bound t)))
      (save-excursion
        (goto-char (1- (match-end 0)))
        (setq value (drover-transcript-string-at))))
    (unless value
      (goto-char start))
    value))

(defun drover-transcript-line ()
  "Return the line at point parsed as a JSON object, an alist, or nil."
  (let ((record (ignore-errors
                  (json-parse-string (buffer-substring-no-properties (line-beginning-position)
                                                                     (line-end-position))
                                     :object-type 'alist :array-type 'list
                                     :null-object nil :false-object nil))))
    (and (consp record) (consp (car record)) record)))

(defun drover-transcript-lines (marker &optional backward beg end)
  "Return the lines containing MARKER, a string, parsed, between BEG and END.
They come in the order of the buffer, or BACKWARD; lines that do not
parse, as one cut by a window, are left out.  BEG and END default to
the buffer's bounds."
  (let ((records nil)
        (beg (or beg (point-min)))
        (end (or end (point-max))))
    (save-excursion
      (goto-char (if backward end beg))
      (while (if backward
                 (search-backward marker beg t)
               (search-forward marker end t))
        (when-let* ((record (drover-transcript-line)))
          (push record records))
        (if backward (beginning-of-line) (end-of-line))))
    (nreverse records)))

(defun drover-transcript-last (marker &optional predicate beg end)
  "Return the last line containing MARKER that parses, between BEG and END.
With PREDICATE, the last one for which it returns non-nil."
  (save-excursion
    (goto-char (or end (point-max)))
    (let ((beg (or beg (point-min)))
          (found nil))
      (while (and (not found) (search-backward marker beg t))
        (let ((record (drover-transcript-line)))
          (when (and record (or (null predicate) (funcall predicate record)))
            (setq found record)))
        (beginning-of-line))
      found)))

(defun drover-transcript-time (string)
  "Return the ISO 8601 time STRING as a float time, or nil."
  (and (stringp string)
       (ignore-errors (float-time (encode-time (iso8601-parse string t))))))

(defun drover-transcript-text (string)
  "Return the first line of STRING that is not blank, trimmed and cut, or nil.
It is cut to `drover-transcript-text-limit' characters, ending in an
ellipsis when cut (see `drover-bound-line'); nil for a blank or missing
STRING."
  (drover-bound-line string drover-transcript-text-limit))

(defvar drover-transcript--projects (make-hash-table :test #'equal)
  "Projects of directories, as `drover-transcript-project' found them.")

(defun drover-transcript-project (directory)
  "Return the name of the project of DIRECTORY, or nil.
Found once per Emacs session, as finding a project may walk a slow file
system: readers ask for it as they read, out of commands."
  (when directory
    (let ((cached (gethash directory drover-transcript--projects 'none)))
      (if (not (eq cached 'none))
          cached
        (puthash directory
                 (and (file-directory-p directory)
                      (ignore-errors
                        (when-let* ((project (project-current nil directory)))
                          (project-name project))))
                 drover-transcript--projects)))))

(provide 'drover-transcript)
;;; drover-transcript.el ends here
