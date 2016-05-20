;;;; trivial-nntp.lisp
;;;;
;;;; A minimal library for connecting to nntp servers
;;;;

(in-package #:trivial-nntp)
;;; For interactive use many functions default to using *socket*
(defparameter *socket* nil)
;;; A terminator line with a single period followed by return and lf (stripped)
(defvar *blank* (with-output-to-string (out) (format out "~C~C"  #\period #\return )))

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

;;TODO: to avoid consing, try replacing read-line/string-right-trim
;; with something like stream-read-line, filtering off
;; 
(defun read-unit (&key (socket *socket*))
  "return a unit (line) of text and nil if a good line"
  (let* ((line (read-line (usocket:socket-stream socket)))
	 (end (string= line *blank*)))
    (values line ;(string-right-trim '(#\return) line) trim ^M from end
	    end))
  
  )
(defun read-response (&key (expecting nil) (socket *socket*) )
  "return first digit of the code and the entire line"
  (let* ((line (read-unit :socket socket))
	 (code (parse-integer (subseq line 0 1) :junk-allowed t)))
    (if expecting
	(unless (eq code expecting) ;if checking for error
	    (nntp-error code line)))
    (values code line) ;return if ok
))

(defun read-list (&key (socket *socket*))
  "collect a list containing lines of data"
  (let ((retlist nil))
    (loop
       (multiple-value-bind (line done) (read-unit :socket socket)
	 (if done (return retlist))
	 (push line retlist)))
    retlist))


(defun send-command (string &key (expecting nil) (socket *socket*))
  "send an NNTP command and read response"
  (format (usocket:socket-stream socket) "~A~C~C" string #\return #\linefeed)
  (force-output (usocket:socket-stream socket))
  (read-response :expecting expecting))

(defun disconnect (&key (socket *socket*))
  "cleanly disconnect from the server"
  (multiple-value-prog1
      (send-command "QUIT" :expecting 2 :socket socket)
    (usocket:socket-close socket)
    (setf *socket* nil)))


;;;

(defstruct acct server port user password)



(defparameter *acct* (make-acct :server "news.mixmin.net" :port 119
				 :user "" :password ""))

(defun connect (&key (acct *acct*))
  (setf *socket* (usocket:socket-connect (acct-server acct) (acct-port acct)))
  (read-response)
  )

(defun login (&key (acct *acct*))
  (send-command (concatenate 'string "AUTHINFO USER "
			     (acct-user acct)) :expecting 3)
  (send-command (concatenate 'string "AUTHINFO PASS "
			     (acct-password acct)) :expecting 2)
)

(defun test ()
  (connect)
  (send-command "HELP")
  (prog1
      (read-list)
    (disconnect)))

;;(asdf:system-relative-pathname 'tnntp "test.txt" )



