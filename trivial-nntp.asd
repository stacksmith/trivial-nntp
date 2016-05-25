;;;; trivial-nntp.asd

(asdf:defsystem #:trivial-nntp
  :description "Simple tools for interfacing to NNTP servers"
  :author "Stacksmith <fpgasm@apple2.x10.mx>"
  :license "MIT"
  :depends-on (#:usocket)
  :serial t
  :components ((:file "package")
               (:file "trivial-nntp")))

