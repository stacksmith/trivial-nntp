;;;; package.lisp

(defpackage #:trivial-nntp
  (:nicknames :tnntp)
  (:use #:cl)
  (:export :connect
	   :disconnect
	   :send-command
	   :read-list
	   :read-unit))

