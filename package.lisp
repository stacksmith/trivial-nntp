;;;; package.lisp

(defpackage #:trivial-nntp
  (:nicknames :tnntp)
  (:use #:cl)
  (:export :connect
	   :disconnect
	   :command
	   :range
	   :rlist
	   :rline
	   :reconnect
	   :nntp-error
	   :server
	   :make-server
	   :server-name
	   :server-port
	   :server-user
	   :server-password
	   :server-ssl
	   :server-groups
	   :name
	   :port
	   :user
	   :password
	   :ssl
	   :groups
	   :conn
	   :make-conn
	   :conn-server
	   :conn-group
	   :conn-stream
	   :conn-bytes-read
	   :group
	   :bytes-read))

