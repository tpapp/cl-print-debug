(defpackage :debug-tools
  (:use :common-lisp :cl-utilities :iterate :bind :anaphora
        :named-readtables)
  (:nicknames :d)
  (:shadowing-import-from :iterate :collecting :collect)
  (:export 
   ;; utilities
   d v d-syntax p s))
