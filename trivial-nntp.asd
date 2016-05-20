;;;; trivial-nntp.asd

(asdf:defsystem #:trivial-nntp
  :description "Describe trivial-nntp here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:usocket)
  :serial t
  :components ((:file "package")
               (:file "trivial-nntp")))

