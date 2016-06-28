;;;; trivial-nntp.lisp
;;;;
;;;; A minimal library for connecting to nntp servers
;;;;

(in-package #:trivial-nntp)
;; server struct encapsulates a multi-socket connection to a server,
;; the account information to log in, and state (group connection is in)

;;; A terminator line with a single period followed by return and lf (stripped)
(defvar *blank* (with-output-to-string (out) (format out "~C~C"  #\period #\return )))

(defstruct server name port user password sock-num sock grp)

(defparameter *server*
  (make-server :name "news.mixmin.net"
	       :port 119
	       :user ""
	       :password ""
	       :sock-num 1
	       :sock (make-array 1 )
	       :grp  (make-array 1 :element-type 'string :initial-element "alt.autos.camaro")))

(defun socket (&optional (server *server*) (sindex 0))
  (elt (server-sock server) sindex))

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

(defun read-unit (&key (server *server*) (sindex 0))
  "return a unit (line) of text and nil if a good line"
  ;; Non-UTF8 characters are skipped
  (handler-bind ((sb-int:stream-decoding-error
		  #'(lambda (ex) (declare (ignore ex))
		      (invoke-restart 'sb-int:attempt-resync))))
    
    (let* ((socket (socket server sindex))
	   (line (read-line (usocket:socket-stream socket)))
	   (end (string= line *blank*)))
      (values line ;(string-right-trim '(#\return) line) trim ^M from end
	      end)))  )

(defun read-list (&key (server *server*) (sindex 0))
  "collect a list containing lines of data"
  (let ((retlist nil))
    (loop
       (multiple-value-bind (line done) (read-unit :server server :sindex sindex)
	 (if done (return retlist))
	 (push line retlist)))
    retlist))



(defun read-response (&key (expecting nil) (server *server*) (sindex 0) )
  "return first digit of the code and the entire line"
  (let* ((line (read-unit :server server :sindex sindex))
	 (code (parse-integer (subseq line 0 1) :junk-allowed t)))
    (if expecting
	(unless (eq code expecting) ;if checking for error
	    (nntp-error code line)))
    (values code line) ;return if ok
))
;;;
;;; Restore server connection and state
(defun reconnect (&key (server *server*) (sindex 0))
  (setf (elt (server-sock server) sindex)
	(usocket:socket-connect (server-name server) (server-port server)
				:timeout 5))
  ;(format t "1.RECONNECTED ~A~%" sindex)
  (when (elt (server-grp server) sindex)
      
    (multiple-value-bind (a b)
	(send-command (with-output-to-string (out) (format out "GROUP ~A" (elt (server-grp server) sindex)))))
    ;(read-response :expecting 2)
      )
    
  (format t "3.RECONNECTED ~A~%" sindex)
 
  )

(defun send-command (string &key (expecting nil) (server *server*) (sindex 0))
 ; (format t "SEND-COMMAND ~A~%" string)
  "send an NNTP command and read response"
  (handler-case
      (let ((socket (elt (server-sock server) sindex)))
	(format (usocket:socket-stream socket) "~A~C~C" string #\return #\linefeed)
	(force-output (usocket:socket-stream socket))
	(read-response :expecting expecting :server server :sindex sindex))
    ;;nil socket
    (simple-error ()
;      (format t "1.NIL socket~%")
      (reconnect :server server :sindex sindex)
  ;    (format t "1.AFTER reconnect~%")
       (send-command string :expecting expecting :server server :sindex sindex))
    (usocket:socket-condition() (format t "SOCKET COND IN SEND-COMMAND"))
    (usocket:ns-try-again-condition ()
;      (format t "TRY-AGAIN-CONDITION~%")
      (reconnect)
      (send-command string :expecting expecting :server server :sindex sindex))
    (sb-int:simple-stream-error ()
      (reconnect )
      (send-command string :expecting expecting :server server :sindex sindex))
    ))

(defun disconnect (&key (server *server*) (sindex 0))
  "cleanly disconnect from the server"
  (when (socket server sindex)
    (multiple-value-prog1
	(send-command "QUIT"  :server server :sindex sindex)
      (usocket:socket-close (socket server sindex))
      (setf (elt (server-sock server) sindex) nil))))

