(defsystem #:debug-tools
  :description "Tools for debugging."
  :version "alpha"
  :author "Tamas K Papp <tkpapp@gmail.com>"
  :license "BSD without advertising clause"
  :serial t
  :components
  ((:file "package")
   (:file "utilities"))
  :depends-on
  (:cl-utilities :iterate :metabang-bind :anaphora :alexandria
                 :named-readtables))
