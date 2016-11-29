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

(defun line-or-nil (line)
  "return nil for NNTP endline"
  (when line
    (unless (and (= 2 (length line))
		 (char= #\period (elt line 0))
		 (char= #\return (elt line 1)))
      line)))

;; declare a printer for server
(defstruct (server
	     (:print-function (lambda (p s k)
				(declare (ignore k))
				(format s "<* SERVER ~A:~A *>"
					(server-name p) (server-port p)))))
  name port user password ssl groups)

(defparameter *server*
  (make-server :name "news.mixmin.net"
	       :port 119
	       :user nil
	       :password nil
	       :ssl nil
	       ))
;;;
;;; A connection keeps track of its server, a stream  (which may be an
;;; SSL stream), some statistics, and some representation of a group.
;;;
;;; A connection may time out and become disconnected; the system will transparently
;;; reconnect as needed.  In order to do that, we keep track of the string representation
;;; of the group associated with this connection.
(defstruct conn server group (stream 0) (bytes-read 0))

(defparameter *conn* (make-conn :server *server*))

;;; Restore server conn and state
(defun reconnect (&key (conn *conn*))
  "Reconnect to the server, if disconnected, restoring state (entering a group).
  This needs more work..."
  (with-slots (server group stream) conn
    (with-slots (name port user password ssl) server
      ;; For an unconnected socket, connect
      (when (numberp stream)
	(setf stream (usocket:socket-stream (usocket:socket-connect name port :timeout 5)))
	(when ssl
	  (setf stream (cl+ssl:make-ssl-client-stream
			stream :external-format '(:iso-8859-1 :eol-style :lf))))
	(response :conn conn :expecting 2) ;eat the server initial signature
	;; if account information present, authenticate
	(when user
	  (command "AUTHINFO USER" :conn conn :also user :expecting 3)
	  (command "AUTHINFO PASS" :conn conn :also password :expecting 2))
	;;(format t "1.RECONNECTED ~A~%" sindex)
	;; if old conn was in a group, re-enter group.  This may be not sensible
	;; as it requires knowlege of group name...
	(when group
	  (multiple-value-bind (digit response)
	      (command "GROUP" :conn conn :also group :expecting 2)
	    (declare (ignore digit response))))))))


(defun rline (&key (conn *conn*))
  "return a unit (line) of text or nil for NNTP endline"
  (let* ((line (read-line (conn-stream conn)))
	 (bytes (length line)))
    (incf (conn-bytes-read conn) bytes)
    (line-or-nil line)))

;; rlist now has accepts a function :proc that converts strings to
;; anything and puts them in a list
(defun rlist (&key (conn *conn*) (proc #'(lambda (str) str)))
  "collect a list containing lines of data, processing if requested"
  (loop for line = (rline :conn conn)
     while line collect (funcall proc line)))


(defun response (&key (conn *conn*)(expecting nil))
  "return first digit of the code and the entire line"
  (let* ((line (rline :conn conn))
	 (code (parse-integer (subseq line 0 1) :junk-allowed t)))
    (if expecting
	(unless (eq code expecting) ;if checking for error
	  (nntp-error code line)))
    (values line code) ;return if ok
    ))
;;;

(defun range (low high &key (stream nil))
  "output an NNTP article range to stream."
  (if (and low high) (format stream "~A-~A" low high)
      (if low (format stream "~A-" low)
	  (if high (format stream "-~A" high)
	      (format stream "")))))


(defun command (string &key (conn *conn*)
			 (expecting nil)
			 (also "")
			 (also2 "")
			 (depth 0))
  "send an NNTP command and read response. Return first digit of response and entire response string.
- (commnad \"XOVER\" :also (range 1 10))
- (command \"GROUP\" :also \"alt.blah.blah.blah\""
  (unless (zerop depth)
    (format t "DEPTH: ~A~%" depth)
    (if (> depth 3)
	(blarghbarf)));;TODO: catch the problem cases and handle them...
  (handler-case
      
      (with-slots (stream) conn
	(format t "~A ~A ~A~C~C" string also also2 #\return #\linefeed)

	(format stream "~A ~A ~A~C~C" string also also2 #\return #\linefeed)
	(force-output stream)
	(response :conn conn :expecting expecting))
    ;;nil socket results in simple-error
    (type-error ()
      (format t "TYPE-ERROR~%")
      (reconnect :conn conn)
      (command string :conn conn :also also :also2 also2
		    :expecting expecting :depth (1+ depth)))
    (simple-error ()
      (format t "SIMPLE-ERROR~%")
      (reconnect :conn conn)
      (command string :conn conn :also also :also2 also2
		    :expecting expecting :depth (1+ depth)))
    (usocket:socket-condition() (format t "SOCKET COND IN COMMAND~%"))
    (usocket:ns-try-again-condition ()
      (format t "TRY-AGAIN-CONDITION~%")
      (reconnect :conn conn)
      (command string :conn conn :also also :also2 also2
		    :expecting expecting :depth (1+ depth))
      )
    (sb-int:simple-stream-error ()
      (format t "SIMPLE STREAM_ERROR ~%")
      (reconnect :conn conn )
      (command string :conn conn :also also :also2 also2
		    :expecting expecting :depth (1+ depth))
      )
    (stream-error ()
      (format t "STREAM_ERROR ~%")
      (reconnect :conn conn )
      (command string :conn conn  :also also :also2 also2
	       :expecting expecting :depth (1+ depth)))
    (cl+ssl::ssl-error ()
      (format t "CL+SSL::SSL-ERROR ~%") ;; on timed-out socket?
      (setf (conn-stream conn) 0)
      (reconnect :conn conn )
      (command string :conn conn  :also also :also2 also2
	       :expecting expecting :depth (1+ depth)))))

(defun disconnect (&key (conn *conn*))
  "cleanly disconnect from the server"
  
  (multiple-value-prog1
      (command "QUIT" :conn conn )
    (with-slots (stream) conn
      (close stream)
      (setf stream 0))
  ))

