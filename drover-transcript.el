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

(defconst drover-transcript-look-back (* 16 drover-transcript-window)
  "Bytes before a tail a reader reads at most looking for a record there.
Engines append records of their own to a conversation, as they open it
again or attach files to a turn, some longer than the tail's window:
the conversation's last message may be further back.")

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
  (version nil :documentation "The version of READ, a number, or nil.
What another version read, kept between Emacs sessions, is read again:
the data READ returns is noted with it, as :reader.")
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

(defun drover-transcript-index-create (name roots read on-change &optional on-cycle key version)
  "Return an index NAME of the files ROOTS tells, read with READ.
ON-CHANGE and ON-CYCLE are told of changes and of each look's end, KEY
names what was read of a file, and VERSION is READ's.  See
`drover-transcript-index' for each."
  (drover-transcript-index--create :name name :roots roots :read read
                                   :on-change on-change :on-cycle on-cycle :key key
                                   :version version))

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
Entries read since are kept over those of the file, and those another
version of its reader read are left out, to be read again."
  (unless (drover-transcript-index-loaded index)
    (setf (drover-transcript-index-loaded index) t)
    (let ((file (drover-transcript-cache-file index))
          (entries (drover-transcript-index-entries index))
          (version (drover-transcript-index-version index)))
      (condition-case err
          (let ((kept (drover-read-data file drover-transcript--version :files)))
            (unless (seq-every-p #'drover-transcript--entry-p kept)
              (error "Malformed entries"))
            (pcase-dolist (`(,name ,size ,mtime ,inode ,data) kept)
              (unless (or (gethash name entries)
                          (and data (not (eql (plist-get data :reader) version))))
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
        (when-let* ((version (drover-transcript-index-version index))
                    ((consp data)))
          (setq data (plist-put data :reader version)))
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
    (when (and (drover-transcript-index-full index) (drover-transcript-index-complete index)
               (drover-transcript-index-key index))
      (let ((files (drover-transcript-index-files index)))
        (drover-transcript-seen-prune (drover-transcript-index-name index)
                                      (lambda (id) (gethash id files)))))
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

(defun drover-transcript--bytes (beg end)
  "Return how many bytes of its file the text from BEG to END took.
It was decoded as UTF-8: the bytes of a character cut, left undecoded,
count as they were."
  (length (encode-coding-string (buffer-substring-no-properties beg end) 'utf-8-unix)))

(defun drover-transcript-look-back (file end floor function)
  "Return what FUNCTION finds in the lines of FILE before byte END, or nil.
FUNCTION is called in a buffer holding lines of FILE with their bounds,
BEG and END, and returns non-nil once it found what it looks for.  The
lines are read in windows of `drover-transcript-window' bytes, the
newest first, back to byte FLOOR, `drover-transcript-look-back' bytes
at most: a line a window cuts is read whole with the lines before it,
unless longer than a window, which no window holds whole."
  (let ((left drover-transcript-look-back)
        (found nil))
    (with-temp-buffer
      (while (and (not found) (> end floor) (> left 0))
        (let ((beg (max floor (- end drover-transcript-window))))
          (erase-buffer)
          (drover-transcript-insert file beg end)
          (setq left (- left (- end beg)))
          (goto-char (point-min))
          (cond ((= beg floor)
                 (setq found (funcall function (point-min) (point-max))
                       end floor))
                ((search-forward "\n" nil t)
                 (setq end (+ beg (drover-transcript--bytes (point-min) (1- (point))))
                       found (funcall function (point) (point-max))))
                (t (setq end beg))))))
    found))

(defun drover-transcript-look-before-tail (file size tail old-size function)
  "Return what FUNCTION finds in the lines of FILE between its head and tail.
The current buffer holds what `drover-transcript-insert-ends' inserted
of FILE, of SIZE bytes, with OLD-SIZE, the tail from TAIL: the lines
before the tail are those after the head, or with OLD-SIZE those
appended after it that the tail does not hold, nil when there are
none.  FUNCTION is as for `drover-transcript-look-back', which reads
them, and the line the tail begins in, whole."
  (let* ((window drover-transcript-window)
         (start (if old-size (max old-size (- size window)) (- size window)))
         (floor (cond (old-size)
                      ((> size (* 2 window))
                       ;; Where the head's last line begins, cut by its window.
                       (save-excursion
                         (goto-char (1- tail))
                         (if (search-backward "\n" nil t)
                             (drover-transcript--bytes (point-min) (1+ (point)))
                           0))))))
    (when (and floor (> start floor))
      (let ((end (save-excursion
                   (goto-char tail)
                   (if (search-forward "\n" nil t)
                       (+ start (drover-transcript--bytes tail (1- (point))))
                     size))))
        (drover-transcript-look-back file end floor function)))))

(defun drover-transcript-look-before-tail-for (file size tail old-size functions)
  "Return what each of FUNCTIONS finds before the tail, the newest, appended.
FUNCTIONS are functions as for `drover-transcript-look-back', each
returning a plist or nil, and FILE, SIZE, TAIL and OLD-SIZE as for
`drover-transcript-look-before-tail': the lines are read once, newest
first, until each function found what it looks for, or the look back
ends, so that looking for two things costs what looking for the later
one does."
  (let ((left (copy-sequence functions))
        (found nil))
    (when left
      (drover-transcript-look-before-tail
       file size tail old-size
       (lambda (beg end)
         (dolist (function (copy-sequence left))
           (when-let* ((value (funcall function beg end)))
             (setq found (append found value)
                   left (delq function left))))
         (null left))))
    found))

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

;;;; Turns the user saw

;; A conversation whose last turn ended in a reply, or an error, is
;; done, or failed, until the user sees it, as OpenCode's sessions are.
;; Engines record no such thing, so drover notes when the user last saw
;; each conversation: as they mark it seen, open it, or a front end
;; following it reports it idle (see :recorded-seen in
;; `drover-register-backend').  A reply after that time is unseen.  So
;; that the first look at hundreds of conversations does not ask the
;; user to see them all, a reply before drover first read an engine's
;; conversations, the time noted for its index, counts as seen: a
;; conversation is done only for a reply that came since.  What the
;; user saw is kept in `drover-state-directory', bounded: the times of
;; conversations still recorded, `drover-transcript-seen-limit' at most.

(defconst drover-transcript--seen-version 1
  "Version of the format of the file of what the user saw.")

(defconst drover-transcript-seen-limit 20000
  "Conversations at most whose time seen drover keeps.
Past it, those seen longest ago are forgotten: their replies, older
still, show done again unless older than when drover first read their
engine's conversations.")

(defconst drover-transcript-seen-save-delay 30
  "Seconds after a conversation was seen that what the user saw is kept.
Emacs exiting keeps it at once.")

(defvar drover-transcript--seen nil
  "What the user saw of conversations, as (FILE . TABLE), or nil before read.
FILE is the file it is kept in, as `drover-transcript-seen-file' named
it; TABLE maps (NAME . ID) to when the user last saw the conversation ID
of the index NAME, and (NAME) to when drover first read the
conversations of NAME, float times.")

(defvar drover-transcript--seen-dirty nil
  "Non-nil when what the user saw changed since it was kept.")

(defvar drover-transcript--seen-timer nil
  "Timer keeping what the user saw, or nil.")

(defvar drover-transcript--seen-warned nil
  "Non-nil once a problem with the file of what the user saw was reported.")

(defun drover-transcript-seen-file ()
  "Return the file what the user saw of conversations is kept in."
  (expand-file-name "seen.eld" drover-state-directory))

(defun drover-transcript--seen-warn (format-string &rest args)
  "Report a problem with the file of what the user saw, once.
FORMAT-STRING and ARGS are as for `format-message'."
  (unless drover-transcript--seen-warned
    (setq drover-transcript--seen-warned t)
    (apply #'drover-warn format-string args)))

(defun drover-transcript--seen-entry-p (entry)
  "Return non-nil when ENTRY is one as kept, (NAME ID TIME), ID nil or a string."
  (pcase entry
    (`(,(pred symbolp) ,(or 'nil (pred stringp)) ,(pred numberp)) t)))

(defun drover-transcript--seen-read (file)
  "Return the table of what the user saw FILE keeps, empty without one.
One that does not read is reported, and ignored: every reply then counts
as seen until a reply comes after it was read."
  (let ((table (make-hash-table :test #'equal)))
    (condition-case err
        (let ((entries (drover-read-data file drover-transcript--seen-version :seen)))
          (unless (seq-every-p #'drover-transcript--seen-entry-p entries)
            (error "Malformed entries"))
          (pcase-dolist (`(,name ,id ,time) entries)
            (puthash (cons name id) (float time) table)))
      (error
       (drover-transcript--seen-warn "Ignoring what drover noted you saw in %s: %s"
                                     (abbreviate-file-name file) (drover-error-message err))))
    table))

(defvar drover-transcript--seen-directory nil
  "The value of `drover-state-directory' the table of what was seen was read for.")

(defun drover-transcript--seen-table ()
  "Return the table of what the user saw, read from its file once.
It is read again when `drover-state-directory' names another file."
  (unless (and drover-transcript--seen
               (equal drover-transcript--seen-directory drover-state-directory))
    (let ((file (drover-transcript-seen-file)))
      (setq drover-transcript--seen-directory drover-state-directory)
      (unless (equal (car drover-transcript--seen) file)
        (setq drover-transcript--seen (cons file (drover-transcript--seen-read file))
              drover-transcript--seen-dirty nil))))
  (cdr drover-transcript--seen))

(defun drover-transcript--seen-changed ()
  "Note that what the user saw changed, to keep it soon."
  (setq drover-transcript--seen-dirty t)
  (unless (timerp drover-transcript--seen-timer)
    (setq drover-transcript--seen-timer
          (run-at-time drover-transcript-seen-save-delay nil #'drover-transcript--seen-flush))))

(defun drover-transcript--seen-flush ()
  "Keep what the user saw, from a timer."
  (drover-with-reporting "keeping what you saw"
    (setq drover-transcript--seen-timer nil)
    (drover-transcript-seen-save)))

(defun drover-transcript-seen-since (name)
  "Return when drover first read the conversations of the index NAME.
A reply before counts as seen.  Noted now, the first time asked."
  (let ((table (drover-transcript--seen-table)))
    (or (gethash (list name) table)
        (prog1 (puthash (list name) (float-time) table)
          (drover-transcript--seen-changed)))))

(defun drover-transcript-seen-time (name id)
  "Return when the user last saw conversation ID of the index NAME, or before.
That is when they saw it, or when drover first read the conversations
of NAME, whichever is later."
  (let ((since (drover-transcript-seen-since name))
        (seen (gethash (cons name id) (drover-transcript--seen-table))))
    (if seen (max seen since) since)))

(defun drover-transcript-unseen-p (name id time)
  "Return non-nil when a reply at TIME of conversation ID of index NAME is unseen."
  (and (numberp time) (> time (drover-transcript-seen-time name id))))

(defun drover-transcript-mark-seen (name id &optional time)
  "Note that the user saw conversation ID of the index NAME at TIME.
TIME defaults to now.  Return non-nil when that is later than noted."
  (let* ((table (drover-transcript--seen-table))
         (key (cons name id))
         (time (float (or time (float-time))))
         (before (gethash key table)))
    (when (or (null before) (> time before))
      (puthash key time table)
      (drover-transcript--seen-changed)
      t)))

(defun drover-transcript-seen-prune (name keep)
  "Forget what the user saw of the conversations of index NAME KEEP does not keep.
KEEP is a function of an identifier: those of conversations no longer
recorded go, after a look found every file.  So do times before drover
first read NAME's conversations, which tell nothing more."
  (let ((table (drover-transcript--seen-table))
        (gone nil))
    (let ((since (gethash (list name) table)))
      (maphash (lambda (key time)
                 (when (and (eq (car key) name) (cdr key)
                            (or (not (funcall keep (cdr key)))
                                (and since (<= time since))))
                   (push key gone)))
               table))
    (when gone
      (dolist (key gone)
        (remhash key table))
      (drover-transcript--seen-changed))))

(defun drover-transcript-seen-save ()
  "Keep what the user saw in its file, when it changed since kept.
What another Emacs kept there meanwhile is merged in, the later time
seen of each conversation and the earlier first look at each index;
the times seen longest ago are dropped past
`drover-transcript-seen-limit'.  A failure is reported once."
  (when (and drover-transcript--seen-dirty
             (equal (car drover-transcript--seen) (drover-transcript-seen-file)))
    (let* ((file (car drover-transcript--seen))
           (table (cdr drover-transcript--seen))
           (seen nil)
           (entries nil))
      (setq drover-transcript--seen-dirty nil)
      (when (file-exists-p file)
        (let ((drover-transcript--seen-warned t))
          (maphash (lambda (key time)
                     (let ((own (gethash key table)))
                       (when (or (null own) (if (cdr key) (> time own) (< time own)))
                         (puthash key time table))))
                   (drover-transcript--seen-read file))))
      (maphash (lambda (key time)
                 (if (cdr key)
                     (push (cons key time) seen)
                   (push (list (car key) nil time) entries)))
               table)
      (when (> (length seen) drover-transcript-seen-limit)
        (setq seen (sort seen :key #'cdr :reverse t :in-place t))
        (dolist (forgotten (nthcdr drover-transcript-seen-limit seen))
          (remhash (car forgotten) table))
        (setq seen (seq-take seen drover-transcript-seen-limit)))
      (dolist (entry seen)
        (push (list (caar entry) (cdar entry) (cdr entry)) entries))
      (condition-case err
          (drover-write-data file "What you saw of the conversations engines record"
                             drover-transcript--seen-version :seen
                             (sort entries :key (lambda (entry)
                                                  (concat (symbol-name (car entry)) " " (or (nth 1 entry) "")))
                                   :in-place t))
        (error (drover-transcript--seen-warn "Could not keep what you saw in %s: %s"
                                             (abbreviate-file-name file) (drover-error-message err)))))))

;;;; Providers

(defvar drover-transcript--providers (cons nil (make-hash-table :test #'equal))
  "The providers made, by (ID . NAME), and the icons they were made with.")

(defun drover-transcript-provider (id name)
  "Return the provider ID, named NAME, as keyword arguments of a record.
That is :provider, its icon as `opencode-provider-icons' gives it, else
a generic icon and NAME, as OpenCode's sessions show it, and
:provider-name, NAME, which lists group sessions by."
  (unless (eq (car drover-transcript--providers) opencode-provider-icons)
    (setq drover-transcript--providers (cons opencode-provider-icons (make-hash-table :test #'equal))))
  (with-memoization (gethash (cons id name) (cdr drover-transcript--providers))
    (list :provider (or (cdr (seq-find (lambda (entry) (string-match-p (car entry) id))
                                       opencode-provider-icons))
                        (concat opencode-provider-default-icon " " name))
          :provider-name name)))

;;;; Searching what was said

;; A search of what was said in conversations runs ripgrep over their
;; files, in a process of its own, so that Emacs never reads a line of
;; megabytes: the regular expression finds a match within the text of a
;; prompt or a reply, after the members that begin it, and ripgrep
;; prints only the snippet around it, a capture group, which its
;; default engine finds in linear time.  Tool calls, their results, the
;; API requests recorded whole and the engines' own records begin
;; otherwise, and are not searched.  It runs as a process of Emacs's
;; own rather than through `drover-command-run', whose commands give
;; their output once ended and in files: matches stream as found, and
;; a search typed on is stopped at once.

(defcustom drover-transcript-ripgrep "rg"
  "The ripgrep program searching what was said in conversations, or nil.
Without it, a search of sessions matches what they tell and their
prompts alone (see `drover-search')."
  :type '(choice (string :tag "Program") (const :tag "None" nil))
  :group 'drover)

(defconst drover-transcript-search-context 60
  "Characters of a snippet around a match, before it and after it.")

(defconst drover-transcript-search-per-file 1
  "Lines at most a search takes of one file.
A session shows one snippet: more would spend the matches a search
takes on the conversations found first.")

(defconst drover-transcript-search-query-limit 100
  "Characters at most of a text searched for.")

(defconst drover-transcript-search-arguments-limit
  (if (eq system-type 'windows-nt) 24000 200000)
  "Characters at most of the names of the files one search is given.
A command line of Windows holds 32767 characters.")

(defvar drover-transcript--ripgrep-told nil
  "Non-nil once a search told that ripgrep is missing.")

(defun drover-transcript--ripgrep ()
  "Return ripgrep's executable, or nil, telling once that it is missing."
  (or (and drover-transcript-ripgrep (executable-find drover-transcript-ripgrep))
      (progn
        (when (and drover-transcript-ripgrep (not drover-transcript--ripgrep-told))
          (setq drover-transcript--ripgrep-told t)
          (drover-message "Drover: %s is not found: searches match what sessions tell and their prompts"
                          drover-transcript-ripgrep))
        nil)))

(defun drover-transcript--rust-quote (string)
  "Return STRING quoted for the default engine of ripgrep, matching itself.
Each character of the syntax of its regular expressions is escaped; a
quote and a backslash as a JSON string has them, escaped too."
  (mapconcat (lambda (char)
               (cond ((eq char ?\") "\\\\\"")
                     ((eq char ?\\) "\\\\\\\\")
                     ((memq char '(?. ?+ ?* ?? ?\( ?\) ?| ?\[ ?\] ?{ ?} ?^ ?$ ?# ?& ?- ?~))
                      (string ?\\ char))
                     (t (string char))))
             string ""))

(defun drover-transcript-search-regexp (starts query)
  "Return the regular expression finding QUERY in strings STARTS begin.
STARTS are expressions of ripgrep's default engine matching what comes
before the text of a prompt or reply, its opening quote included.  The
group `s' is the snippet: the first match of QUERY in that text, case
ignored, with `drover-transcript-search-context' characters around it.
Words of QUERY match across any blanks, as escaped in JSON."
  (let* ((unit "(?:[^\"\\\\]|\\\\.)")
         (words (split-string (substring query 0 (min (length query) drover-transcript-search-query-limit))
                              "[ \t\n\r]+" t))
         (text (mapconcat #'drover-transcript--rust-quote words "(?:[ \\t]|\\\\[nrt])+"))
         (context (format "%s{0,%d}" unit drover-transcript-search-context)))
    (format "(?:%s)%s*?(?P<s>%s(?i:%s)%s)"
            (string-join starts "|") unit context text context)))

(defun drover-transcript-decode-json (string)
  "Return STRING, a piece of a JSON string, with its escapes decoded.
Line breaks and tabs become spaces; an escape cut at an end is dropped."
  (let ((case-fold-search nil))
    (replace-regexp-in-string
     "\\\\\\(?:u\\([[:xdigit:]]\\{4\\}\\)\\(?:\\\\u\\([[:xdigit:]]\\{4\\}\\)\\)?\\|u[[:xdigit:]]\\{0,3\\}\\'\\|\\(.\\)\\|\\'\\)"
     (lambda (match)
       (cond
        ((match-beginning 1)
         (let ((high (string-to-number (match-string 1 match) 16))
               (low (and (match-beginning 2) (string-to-number (match-string 2 match) 16))))
           (cond ((and low (<= #xd800 high #xdbff) (<= #xdc00 low #xdfff))
                  (string (+ #x10000 (ash (- high #xd800) 10) (- low #xdc00))))
                 ((<= #xd800 high #xdfff) (if low (string (if (<= #xd800 low #xdfff) ?\s low)) ""))
                 (t (concat (string high) (if low (string (if (<= #xd800 low #xdfff) ?\s low)) ""))))))
        ((match-beginning 3)
         (pcase (aref (match-string 3 match) 0)
           ((or ?n ?r ?t) " ")
           (?b "") (?f "")
           (char (string char))))
        (t "")))
     string t t)))

(defun drover-transcript--search-files (files)
  "Return FILES to search, a list of (FILE ID UPDATED), the newest first.
The value is (TABLE . NAMES): TABLE a hash table of the identifiers by
file, NAMES the files, as many as fit in
`drover-transcript-search-arguments-limit'."
  (let ((table (make-hash-table :test #'equal))
        (names nil)
        (room drover-transcript-search-arguments-limit))
    (dolist (file (sort (copy-sequence files) :key (lambda (file) (or (nth 2 file) 0)) :reverse t))
      (let ((name (expand-file-name (car file))))
        (when (> (setq room (- room (length name) 1)) 0)
          (puthash name (nth 1 file) table)
          (push name names))))
    (cons table (nreverse names))))

(defun drover-transcript-search (files starts query callback &optional limit)
  "Search FILES for QUERY said in prompts and replies, for CALLBACK.
FILES is a list of (FILE ID UPDATED), the conversation ID each records
and when it was last active, the newest searched first when they are
too many for one command.  STARTS are what comes before the text of a
prompt or reply in a record (see `drover-transcript-search-regexp').
CALLBACK gets lists of matches, plists of :id, :snippet, the text
around the match, bounded and decoded, and :position, its line in the
file, as ripgrep finds them, at most `drover-transcript-search-per-file'
lines of a file and LIMIT matches in all; then `done', or (:failure
MESSAGE).  Return a function stopping the search, after which CALLBACK
is not called; nil when there is nothing to search or no ripgrep, and
CALLBACK is never called."
  (let ((program (and files (not (string-blank-p query)) (drover-transcript--ripgrep))))
    (when program
      (pcase-let* ((`(,table . ,names) (drover-transcript--search-files files))
                   (default-directory temporary-file-directory)
                   (process
                    (make-process
                     :name "drover-search" :buffer nil :noquery t :connection-type 'pipe
                     :coding '(utf-8 . utf-8)
                     :command (append (list program "--no-config" "--no-messages" "--color" "never"
                                            "--no-heading" "--with-filename" "--null" "--line-number"
                                            "--only-matching" "--replace" "${s}"
                                            "--max-count" (number-to-string drover-transcript-search-per-file)
                                            "--max-columns" "2000" "--max-columns-preview"
                                            "-e" (drover-transcript-search-regexp starts query) "--")
                                      names)
                     :filter #'drover-transcript--search-filter
                     :sentinel #'drover-transcript--search-sentinel)))
        (process-put process 'drover-ids table)
        (process-put process 'drover-callback callback)
        (process-put process 'drover-partial "")
        (process-put process 'drover-left (or limit most-positive-fixnum))
        (lambda ()
          (process-put process 'drover-callback nil)
          (when (process-live-p process)
            (delete-process process)))))))

(defun drover-transcript--search-line (process line)
  "Return the match LINE of PROCESS tells, a plist, or nil when it tells none.
A line not of a match, an error of ripgrep, is kept for the end."
  (let ((null (string-search "\0" line)))
    (if (not (and null
                  (eq (string-match "[0-9]+:" line (1+ null)) (1+ null))))
        (progn
          (when (< (length (process-get process 'drover-errors)) 3)
            (process-put process 'drover-errors
                         (cons (drover-bound-line line drover-error-length)
                               (process-get process 'drover-errors))))
          nil)
      (when-let* ((id (gethash (substring line 0 null) (process-get process 'drover-ids))))
        (list :id id
              :position (string-to-number (substring line (1+ null) (1- (match-end 0))))
              :snippet (drover-bound-line
                        (drover-transcript-decode-json
                         (substring line (match-end 0)
                                    (min (length line) (+ (match-end 0) (* 8 drover-transcript-search-context)))))
                        (* 3 drover-transcript-search-context)))))))

(defun drover-transcript--search-deliver (process matches)
  "Give MATCHES, in order, to the callback of PROCESS, within its limit.
Past the limit, the search ends, its callback told it is done."
  (when-let* ((callback (process-get process 'drover-callback)))
    (let* ((left (process-get process 'drover-left))
           (taken (seq-take matches left)))
      (process-put process 'drover-left (- left (length taken)))
      (when taken
        (funcall callback taken))
      (when (<= (process-get process 'drover-left) 0)
        (process-put process 'drover-callback nil)
        (delete-process process)
        (funcall callback 'done)))))

(defun drover-transcript--search-filter (process output)
  "Take OUTPUT of the search PROCESS, telling its callback the matches it ends."
  (drover-with-reporting "search of transcripts"
    (when (process-get process 'drover-callback)
      (let* ((text (concat (process-get process 'drover-partial) output))
             (end (string-search "\n" text))
             (start 0)
             (matches nil))
        (while end
          (when-let* ((match (drover-transcript--search-line process (substring text start end))))
            (push match matches))
          (setq start (1+ end)
                end (string-search "\n" text start)))
        ;; A line is bounded by --max-columns; anything longer is noise.
        (process-put process 'drover-partial
                     (let ((rest (substring text start)))
                       (if (> (length rest) 65536) "" rest)))
        (drover-transcript--search-deliver process (nreverse matches))))))

(defun drover-transcript--search-sentinel (process _event)
  "Tell the callback of the search PROCESS that it ended, and how."
  (drover-with-reporting "search of transcripts"
    (unless (process-live-p process)
      (when (process-get process 'drover-callback)
        (let ((rest (process-get process 'drover-partial)))
          (when (and rest (not (string-empty-p rest)))
            (process-put process 'drover-partial "")
            (drover-transcript--search-filter process "\n")))
        (when-let* ((callback (process-get process 'drover-callback)))
          (process-put process 'drover-callback nil)
          (let ((status (process-exit-status process))
                (errors (process-get process 'drover-errors)))
            (funcall callback
                     (if (or (memq status '(0 1)) (and (eq status 2) (null errors)))
                         'done
                       (list :failure (or (car (last errors))
                                          (format "%s ended with status %s"
                                                  drover-transcript-ripgrep status)))))))))))

(provide 'drover-transcript)
;;; drover-transcript.el ends here
