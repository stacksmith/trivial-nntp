;;;; trivial-nntp.lisp
;;;;
;;;; A minimal library for connecting to nntp servers
;;;;

(in-package #:trivial-nntp)


(define-condition nntp-error (error)
  ((message
    :initarg :message
    :accessor nntp-error-message
    :initform nil
    :documentation "text message indicating nntp error")
   (id
    :initarg :id
    :accessor nntp-id
    :initform nil)))

(defun nntp-error (id message)
  (error 'nntp-error
	 :message message
	 :id id))

;;; A terminator line with a single period followed by return and lf (stripped)
(defvar *blank* (with-output-to-string (out) (format out "~C~C"  #\period #\return )))

(defstruct server name port user password ssl)

(defparameter *server*
  (make-server :name "news.mixmin.net"
	       :port 119
	       :user nil
	       :password nil
	       :ssl nil))
;;;
;;; A conn is a socket for connecting with a server
(defstruct conn server group (stream 0) (bytes-read 0))

(defparameter *conn* (make-conn :server *server*))

;;; Restore server conn and state
(defun reconnect (conn)
  (with-slots (server group stream) conn
    (with-slots (name port user password ssl) server
      ;; For an unconnected socket, connect
      (when (numberp stream)
	(setf stream (usocket:socket-stream (usocket:socket-connect name port :timeout 5)))
	(when ssl
	  (setf stream (cl+ssl:make-ssl-client-stream
			stream :external-format '(:iso-8859-1 :eol-style :lf))))
	(read-response conn :expecting 2) ;eat the server initial signature
	;; if account information present, authenticate
	(when user
	  (send-command conn "AUTHINFO USER" :also user :expecting 3)
	  (send-command conn "AUTHINFO PASS" :also password :expecting 2))
	;;(format t "1.RECONNECTED ~A~%" sindex)
	;; if old conn was in a group, re-enter group
	(when group
	  (multiple-value-bind (digit response)
	      (send-command conn "GROUP" :also group :expecting 2)
	    (declare (ignore digit response))))))))


(defun read-unit (conn)
  "return a unit (line) of text and nil if a good line"
  ;; Non-UTF8 characters are skipped
  ;; This appears to be non-portable?
  (let* ((line (read-line (conn-stream conn)))
	 (end (string= line *blank*)))
    (incf (conn-bytes-read conn) (length line))
    (values line end))  )

;; read-list now has accepts a function :proc that converts strings to
;; anything and puts them in a list
(defun read-list (conn &key(proc #'(lambda (str) str)))
  "collect a list containing lines of data"
  (let ((retlist nil))
    (loop
       (multiple-value-bind (line done) (read-unit conn)
	 (if done (return retlist))
	 (let ((item (funcall proc line)))
	   (and item (push (funcall proc line) retlist)))))
    retlist))



(defun read-response (conn &key (expecting nil))
  "return first digit of the code and the entire line"
  (let* ((line (read-unit conn))
	 (code (parse-integer (subseq line 0 1) :junk-allowed t)))
    (if expecting
	(unless (eq code expecting) ;if checking for error
	  (nntp-error code line)))
    (values line code) ;return if ok
    ))
;;;

(defun send-command (conn string &key (expecting nil) (also nil) (depth 0))
  "send an NNTP command and read response. Return first digit of response and entire response string"
  (unless (zerop depth)
    (format t "DEPTH: ~A~%" depth)
    (if (> depth 3)
	(quit)))
  (let ((command (if also ;; secondary command string
		     (concatenate 'string string " " also)
		     string)))
    (format t "SENDING [~A]~%" command)
    (handler-case
	(with-slots (stream) conn
	  (format stream "~A~C~C" command #\return #\linefeed)
	  (force-output stream)
	  (read-response conn :expecting expecting))
      ;;nil socket results in simple-error
      (type-error ()
	(format t "TYPE-ERROR~%")
	(reconnect conn)
	(send-command conn command  :expecting expecting :depth (1+ depth))
	)
      (simple-error ()
	(format t "SIMPLE-ERROR~%")
	(reconnect conn)
	(send-command conn command  :expecting expecting) :depth (1+ depth))
      (usocket:socket-condition() (format t "SOCKET COND IN SEND-COMMAND~%"))
      (usocket:ns-try-again-condition ()
	(format t "TRY-AGAIN-CONDITION~%")
	(reconnect conn)
	(send-command conn command :expecting expecting :depth (1+ depth)))
      (sb-int:simple-stream-error ()
	(format t "SIMPLE STREAM_ERROR ~%")
	(reconnect conn )
	(send-command conn command :expecting expecting :depth (1+ depth)))
      (stream-error ()
	(format t "STREAM_ERROR ~%")
	(reconnect conn )
	(send-command conn command :expecting expecting :depth (1+ depth)))
      )))

(defun disconnect (conn)
  "cleanly disconnect from the server"
  
  (multiple-value-prog1
      (send-command conn "QUIT")
    (with-slots (stream) conn
      (close stream)
      (setf stream 0))
  ))

