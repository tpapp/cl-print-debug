(asdf:defsystem #:print-debug
  :description "Tools for debugging by printing."
  :version "0.1"
  :author "Tamas K Papp <tkpapp@gmail.com>"
  :license "BSD without advertising clause."
  :depends-on (:alexandria)
  :components ((:file "print-debug")))
