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
;;; A connection is a socket for connecting with a server
(defstruct connection server group fd stream)

(defparameter *conn* (make-connection :server *server* :fd 0 :stream 0))
;;; Restore server connection and state
(defun reconnect (connection)
  (with-slots (server group fd stream) connection
    (with-slots (name port user password ssl) server
      (setf fd (usocket:socket-connect name port :timeout 5)
	    stream (usocket:socket-stream fd))
      (when ssl
;	(print "ssl connection")
 ;;       (print stream)
	(setf stream (cl+ssl:make-ssl-client-stream stream :external-format '(:iso-8859-1 :eol-style :lf)))
;;	(print stream)
)
      (read-response connection :expecting 2) ;eat the server initial signature
      ;; if account information present, authenticate
      (when user
	(send-command connection "AUTHINFO USER" :also user :expecting 3)
	(send-command connection "AUTHINFO PASS" :also password :expecting 2))
      ;;(format t "1.RECONNECTED ~A~%" sindex)
      ;; if old connection was in a group, re-enter group
      (when group
	(multiple-value-bind (digit response)
	    (send-command connection "GROUP" :also group :expecting 2)
	  (declare (ignore digit response)))))))



(defun read-unit (connection)
  "return a unit (line) of text and nil if a good line"
  ;; Non-UTF8 characters are skipped
  ;; This appears to be non-portable?
  (handler-bind ((sb-int:stream-decoding-error
		  #'(lambda (ex) (declare (ignore ex))
		      (invoke-restart 'sb-int:attempt-resync))))
    (let* ((line (read-line (connection-stream connection)))
	   (end (string= line *blank*)))
      (values line end)))  )

;; read-list now has accepts a function :proc that converts strings to
;; anything and puts them in a list
(defun read-list (connection &key(proc #'(lambda (str) str)))
  "collect a list containing lines of data"
  (let ((retlist nil))
    (loop
       (multiple-value-bind (line done) (read-unit connection)
	 (if done (return retlist))
	 (let ((item (funcall proc line)))
	   (and item (push (funcall proc line) retlist)))))
    retlist))



(defun read-response (connection &key (expecting nil))
  "return first digit of the code and the entire line"
  (let* ((line (read-unit connection))
	 (code (parse-integer (subseq line 0 1) :junk-allowed t)))
    (if expecting
	(unless (eq code expecting) ;if checking for error
	  (nntp-error code line)))
    (values line code) ;return if ok
    ))
;;;

(defun send-command (connection string &key (expecting nil) (also nil))
  "send an NNTP command and read response. Return first digit of response and entire response string"
  (let ((command (if also ;; secondary command string
		     (concatenate 'string string " " also)
		     string)))
    (format t "SENDING [~A]~%" command)
    (handler-case
	(with-slots (stream) connection
	  (format stream "~A~C~C" command #\return #\linefeed)
	  (force-output stream)
	  (read-response connection :expecting expecting))
      ;;nil socket results in simple-error
      (type-error ()
	(reconnect connection)
	(send-command connection command  :expecting expecting)
	)
      (simple-error ()
	(reconnect connection)
	(send-command connection command  :expecting expecting))
      (usocket:socket-condition() (format t "SOCKET COND IN SEND-COMMAND"))
      (usocket:ns-try-again-condition ()
					;      (format t "TRY-AGAIN-CONDITION~%")
	(reconnect connection)
	(send-command connection command :expecting expecting))
      (sb-int:simple-stream-error ()
	(reconnect connection )
	(send-command connection command :expecting expecting))
      )))

(defun disconnect (connection)
  "cleanly disconnect from the server"
  
  (multiple-value-prog1
      (send-command connection "QUIT")
    (with-slots (fd stream) connection
      (close stream)
      (usocket:socket-close fd)
      (setf stream 0
	    fd 0))
  ))

