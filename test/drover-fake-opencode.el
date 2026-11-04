;;; drover-fake-opencode.el --- A fake OpenCode service for the remote layer's tests  -*- lexical-binding: t; -*-

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

;; The OpenCode service of the agent (test/drover-remote-agent.el): an
;; HTTP server on 127.0.0.1, registered in a service.json of the agent's
;; directory as OpenCode's service registers itself, so that opencode.el
;; finds it as it finds the user's.  It serves sessions made on command,
;; their transcripts, an event stream, and runs a turn for each prompt
;; it is sent, echoing it, as the service would stream a reply: the
;; events first, the messages they name in the transcript before them.
;; It notes every request, so that the tests tell what reached it.  It
;; loads nothing of opencode.el, so that the bench copies it to another
;; host with the agent.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url-util)

(defvar drover-fake-opencode--server nil
  "The listening process.")

(defvar drover-fake-opencode--password nil
  "The password requests must carry.")

(defvar drover-fake-opencode--streams nil
  "The connections of the event streams open.")

(defvar drover-fake-opencode--sessions (make-hash-table :test #'equal)
  "Session infos, by identifier.")

(defvar drover-fake-opencode--order nil
  "The identifiers of the sessions, in order made.")

(defvar drover-fake-opencode--messages (make-hash-table :test #'equal)
  "The messages of each session, oldest first, by its identifier.")

(defvar drover-fake-opencode--requests nil
  "The requests received, newest first, as (METHOD PATH BODY).")

(defvar drover-fake-opencode--serial 0
  "The number of the last identifier made.")

(defvar drover-fake-opencode-directory "C:/Users/agent/project"
  "The directory of the sessions, as another host spells it.")

(defun drover-fake-opencode--id (prefix)
  "Return a new identifier of PREFIX, increasing as the service's do."
  (format "%s_%014d%012d" prefix (truncate (* 1000 (float-time))) (cl-incf drover-fake-opencode--serial)))

(defun drover-fake-opencode--now ()
  "Return the time now, in milliseconds."
  (truncate (* 1000 (float-time))))

;;;; HTTP

(defconst drover-fake-opencode--piece 16384
  "Bytes written at most at once on a connection, the rest from timers.
Emacs on Windows does not read while it writes to a socket: this Emacs
reading what it writes itself, as the agent's own opencode.el does,
would wait for itself forever on a large answer.")

(defun drover-fake-opencode--write (connection bytes)
  "Write BYTES on CONNECTION, a piece at a time, after what waits to be written."
  (let ((queue (process-get connection 'fake-queue)))
    (process-put connection 'fake-queue (append queue (list bytes)))
    (unless queue
      (drover-fake-opencode--write-next connection))))

(defun drover-fake-opencode--write-next (connection)
  "Write the next piece queued for CONNECTION."
  (when-let* ((queue (process-get connection 'fake-queue)))
    (if (not (process-live-p connection))
        (process-put connection 'fake-queue nil)
      (let ((bytes (car queue)))
        (process-send-string connection (substring bytes 0 (min drover-fake-opencode--piece (length bytes))))
        (if (> (length bytes) drover-fake-opencode--piece)
            (setcar queue (substring bytes drover-fake-opencode--piece))
          (process-put connection 'fake-queue (cdr queue)))
        (when (process-get connection 'fake-queue)
          (run-at-time 0 nil #'drover-fake-opencode--write-next connection))))))

(defun drover-fake-opencode--respond (connection status value)
  "Answer CONNECTION with STATUS and VALUE as JSON, keeping it open."
  (when (process-live-p connection)
    (let ((payload (encode-coding-string (if (stringp value) value (json-serialize value)) 'utf-8)))
      (drover-fake-opencode--write
       connection
       (concat (format "HTTP/1.1 %d %s\r\n" status (if (< status 400) "OK" "Error"))
               "Content-Type: application/json\r\n"
               (format "Content-Length: %d\r\n" (length payload))
               "Connection: keep-alive\r\nKeep-Alive: timeout=5\r\n\r\n"
               payload)))))

(defun drover-fake-opencode--send (event)
  "Send EVENT, an alist, on every event stream as one frame."
  (let ((frame (encode-coding-string (concat "data: " (json-serialize event) "\n\n") 'utf-8)))
    (setq drover-fake-opencode--streams (seq-filter #'process-live-p drover-fake-opencode--streams))
    (dolist (stream drover-fake-opencode--streams)
      (drover-fake-opencode--write stream frame))))

(defun drover-fake-opencode--event (type data)
  "Send an event of TYPE with DATA; return its identifier."
  (let ((id (drover-fake-opencode--id "evt")))
    (drover-fake-opencode--send `((id . ,id) (created . ,(drover-fake-opencode--now)) (type . ,type)
                                  (location (directory . ,drover-fake-opencode-directory))
                                  (data . ,data)))
    id))

(defun drover-fake-opencode--query (path)
  "Return the query of PATH as an alist of strings."
  (when-let* ((start (string-search "?" path)))
    (mapcar (lambda (pair)
              (let ((parts (split-string pair "=")))
                (cons (url-unhex-string (car parts)) (url-unhex-string (or (cadr parts) "")))))
            (split-string (substring path (1+ start)) "&" t))))

(defun drover-fake-opencode--messages-page (id query)
  "Return the page of session ID's messages QUERY asks for."
  (let* ((messages (gethash id drover-fake-opencode--messages))
         (messages (if (equal (cdr (assoc "order" query)) "desc") (reverse messages) messages))
         (limit (and (assoc "limit" query) (string-to-number (cdr (assoc "limit" query)))))
         (type (cdr (assoc "type" query)))
         (messages (if type (seq-filter (lambda (m) (equal (alist-get 'type m) type)) messages) messages)))
    `((data . ,(vconcat (if limit (seq-take messages limit) messages)))
      (cursor . ,(make-hash-table)))))

(defun drover-fake-opencode--handle (connection method path headers body)
  "Answer METHOD on PATH with HEADERS and BODY, from CONNECTION."
  (push (list method path body) drover-fake-opencode--requests)
  (let* ((bare (car (split-string path "?")))
         (query (drover-fake-opencode--query path))
         (auth (cdr (assoc "authorization" headers))))
    (cond
     ((not (equal auth (concat "Basic " (base64-encode-string
                                         (concat "opencode:" drover-fake-opencode--password) t))))
      (drover-fake-opencode--respond connection 401 '((message . "Unauthorized"))))
     ((equal bare "/api/info")
      (drover-fake-opencode--respond connection 200 `((pid . ,(emacs-pid)) (version . "2.0.0-fake"))))
     ((equal bare "/api/event")
      (drover-fake-opencode--write connection (concat "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n"
                                                      "Cache-Control: no-cache\r\n\r\n"))
      (push connection drover-fake-opencode--streams)
      (drover-fake-opencode--write connection (concat "data: " (json-serialize '((type . "server.connected")
                                                                                 (data . :null)))
                                                      "\n\n")))
     ((equal bare "/api/session/active")
      (drover-fake-opencode--respond connection 200 `((data . ,(make-hash-table)))))
     ((member bare '("/api/agent"))
      (drover-fake-opencode--respond connection 200 '((data . [((id . "build") (mode . "primary"))]))))
     ((member bare '("/api/model" "/api/provider" "/api/command"))
      (drover-fake-opencode--respond connection 200 '((data . []))))
     ((string-match "\\`/api/session/\\([^/]+\\)\\(/.*\\)?\\'" bare)
      (let* ((id (match-string 1 bare))
             (rest (or (match-string 2 bare) ""))
             (info (gethash id drover-fake-opencode--sessions)))
        (cond
         ((not info)
          (drover-fake-opencode--respond connection 404 `((_tag . "NotFoundError")
                                                          (message . ,(format "Session not found: %s" id)))))
         ((and (equal rest "") (equal method "GET"))
          (drover-fake-opencode--respond connection 200 `((data . ,info))))
         ((and (equal rest "") (equal method "DELETE"))
          (remhash id drover-fake-opencode--sessions)
          (drover-fake-opencode--respond connection 200 '((data . t))))
         ((equal rest "/message")
          (drover-fake-opencode--respond connection 200 (drover-fake-opencode--messages-page id query)))
         ((string-prefix-p "/message/" rest)
          (let ((wanted (substring rest 9)))
            (if (seq-find (lambda (m) (equal (alist-get 'id m) wanted)) (gethash id drover-fake-opencode--messages))
                (drover-fake-opencode--respond connection 200 '((data . t)))
              (drover-fake-opencode--respond connection 404 '((message . "Message not found"))))))
         ((member rest '("/inbox" "/permission" "/form"))
          (drover-fake-opencode--respond connection 200 '((data . []))))
         ((equal rest "/view")
          (drover-fake-opencode--respond connection 200 '((data . t))))
         ((and (equal rest "/prompt") (equal method "POST"))
          (let* ((sent (condition-case nil (json-parse-string body :object-type 'alist) (error nil)))
                 (item `((id . ,(or (alist-get 'id sent) (drover-fake-opencode--id "msg")))
                         (sessionID . ,id) (type . "user")
                         (payload (text . ,(or (alist-get 'text sent) "")))
                         (delivery . "steer"))))
            (drover-fake-opencode--respond connection 200 `((data . ,item)))
            (run-at-time 0.02 nil #'drover-fake-opencode-turn id (alist-get 'text sent) (alist-get 'id item))))
         (t (drover-fake-opencode--respond connection 404 '((message . "Not found")))))))
     (t (drover-fake-opencode--respond connection 404 '((message . "Not found")))))))

(defun drover-fake-opencode--filter (connection output)
  "Collect OUTPUT of CONNECTION and answer each request once complete."
  (let ((input (concat (or (process-get connection 'input) "") output))
        (done nil))
    (while (not done)
      (setq done t)
      (when-let* ((end (string-search "\r\n\r\n" input)))
        (let* ((lines (split-string (substring input 0 end) "\r\n"))
               (start (split-string (car lines) " "))
               (headers (delq nil (mapcar (lambda (line)
                                            (when (string-match "\\`\\([^:]+\\): *\\(.*\\)\\'" line)
                                              (cons (downcase (match-string 1 line)) (match-string 2 line))))
                                          (cdr lines))))
               (length (string-to-number (or (cdr (assoc "content-length" headers)) "0")))
               (body-start (+ end 4)))
          (when (>= (- (length input) body-start) length)
            (let ((body (decode-coding-string (substring input body-start (+ body-start length)) 'utf-8)))
              (setq input (substring input (+ body-start length))
                    done nil)
              (process-put connection 'input input)
              (drover-fake-opencode--handle connection (car start) (cadr start) headers body))))))
    (process-put connection 'input input)))

;;;; Sessions and turns

(defun drover-fake-opencode-add (id title)
  "Make the session ID, of TITLE, idle, with a turn already."
  (let ((now (drover-fake-opencode--now)))
    (unless (gethash id drover-fake-opencode--sessions)
      (setq drover-fake-opencode--order (append drover-fake-opencode--order (list id))))
    (puthash id `((id . ,id) (title . ,title) (agent . "build")
                  (location (directory . ,drover-fake-opencode-directory))
                  (time (created . ,now) (updated . ,now)))
             drover-fake-opencode--sessions)
    (puthash id (list `((id . ,(drover-fake-opencode--id "msg")) (time (created . ,now))
                        (type . "user") (text . ,(concat "First prompt of " title)))
                      `((id . ,(drover-fake-opencode--id "msg")) (time (created . ,now) (completed . ,now))
                        (type . "assistant") (agent . "build")
                        (content . [((type . "text") (text . ,(concat "First reply of " title)))])))
             drover-fake-opencode--messages)))

(defun drover-fake-opencode-turn (id text &optional inbox)
  "Run a turn of session ID for the prompt TEXT, its inbox item INBOX if sent.
The reply echoes TEXT; each message goes in the transcript before the
event naming it, as the service appends them."
  (when (gethash id drover-fake-opencode--sessions)
    (let ((inbox (or inbox (drover-fake-opencode--id "msg")))
          (reply (drover-fake-opencode--id "msg"))
          (answer (concat "Echo: " text))
          (now (drover-fake-opencode--now))
          (session `((sessionID . ,id))))
      (cl-flet ((add (message)
                  (puthash id (append (gethash id drover-fake-opencode--messages) (list message))
                           drover-fake-opencode--messages)))
        (drover-fake-opencode--event "session.inbox.enqueued"
                                     `((sessionID . ,id) (inboxID . ,inbox)
                                       (item (type . "user") (payload (text . ,text)) (delivery . "steer"))))
        (drover-fake-opencode--event "session.execution.started" session)
        (add `((id . ,inbox) (time (created . ,now)) (type . "user") (text . ,text)))
        (drover-fake-opencode--event "session.inbox.delivered" `((sessionID . ,id) (inboxID . ,inbox)))
        (add `((id . ,reply) (time (created . ,now) (completed . ,now)) (type . "assistant") (agent . "build")
               (content . [((type . "text") (text . ,answer))])))
        (drover-fake-opencode--event "session.step.started"
                                     `((sessionID . ,id) (agent . "build") (assistantMessageID . ,reply)
                                       (started . ,now)))
        (drover-fake-opencode--event "session.text.started"
                                     `((sessionID . ,id) (assistantMessageID . ,reply) (ordinal . 0)))
        (drover-fake-opencode--event "session.text.delta"
                                     `((sessionID . ,id) (assistantMessageID . ,reply) (ordinal . 0)
                                       (delta . ,answer)))
        (drover-fake-opencode--event "session.text.ended"
                                     `((sessionID . ,id) (assistantMessageID . ,reply) (ordinal . 0)
                                       (text . ,answer)))
        (drover-fake-opencode--event "session.step.ended"
                                     `((sessionID . ,id) (assistantMessageID . ,reply) (finish . "stop")
                                       (tokens (input . 10) (output . 5) (reasoning . 0)
                                               (cache (read . 0) (write . 0)))))
        (let ((idle (drover-fake-opencode--id "evt")))
          (add `((id . ,(concat "msg_" (substring idle 4))) (time (created . ,now)) (type . "idle")
                 (outcome . "succeeded")))
          (drover-fake-opencode--send `((id . ,idle) (created . ,now) (type . "session.execution.succeeded")
                                        (data . ,session))))))))

(defun drover-fake-opencode-archive (id)
  "Archive the session ID, as a client would, telling its event."
  (when-let* ((info (gethash id drover-fake-opencode--sessions)))
    (let ((now (drover-fake-opencode--now)))
      (setf (alist-get 'archived (alist-get 'time info)) now)
      (drover-fake-opencode--event "session.archived" `((sessionID . ,id))))))

(defun drover-fake-opencode-requests (&optional method)
  "Return the requests received, oldest first, of METHOD if given, as (METHOD PATH BODY)."
  (seq-filter (lambda (request) (or (null method) (equal (car request) method)))
              (reverse drover-fake-opencode--requests)))

(defun drover-fake-opencode-start (directory)
  "Start the service, registered in DIRECTORY's service.json; return that file."
  (setq drover-fake-opencode--password (format "fake-%d" (random 1000000))
        drover-fake-opencode--server
        (make-network-process :name "fake-opencode" :server t :host "127.0.0.1" :service t
                              :family 'ipv4 :noquery t :coding 'binary
                              :filter #'drover-fake-opencode--filter
                              :sentinel #'ignore))
  (let ((file (expand-file-name "service.json" directory)))
    (with-temp-file file
      (insert (json-serialize `((id . "svc_fake") (version . "2.0.0-fake")
                                (url . ,(format "http://127.0.0.1:%d"
                                                (process-contact drover-fake-opencode--server :service)))
                                (pid . ,(emacs-pid))
                                (password . ,drover-fake-opencode--password)))))
    file))

(defun drover-fake-opencode-port ()
  "Return the port the service listens on."
  (process-contact drover-fake-opencode--server :service))

(provide 'drover-fake-opencode)
;;; drover-fake-opencode.el ends here
