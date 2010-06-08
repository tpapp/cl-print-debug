;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:debug-tools)

(defun d (control-string &rest arguments)
  "Print formatted arguments, preceded by a timestamp.  A closing
newline is appended."
  (bind ((output *error-output*)
         ((:values second minute hour) (get-decoded-time)))
    (format output "~2,'0d:~2,'0d:~2,'0d " hour minute second)
    (apply #'format *error-output* control-string arguments)
    (format *error-output* "~&")))

(defmacro v (&rest forms)
  "Print formatted form=value pairs (usind D)."
  (let ((control-string (format nil "~{~a=~~A~^  ~}" forms)))
    `(d ,control-string ,@forms)))

(defmacro p (form)
  "Print form, then print and return its value.  Only handles a single
value."
  (with-unique-names (value)
    `(progn 
       (d "~A =>" ',form)
       (let ((,value ,form))
         (d "    => ~A" ,value)
         ,value))))

(defmacro s (variable form)
  "Save the result of FROM in VARIABLE.  (:VALUES VAR...) is also recognized,
for multiple values."
  (if (atom variable)
      (with-unique-names (value)
        (check-type variable symbol)
        `(let ((,value ,form))
           (defparameter ,variable ,value)
           ,value))
      (let* ((variables (cdr variable))
             (values (mapcar (lambda (symbol) (gensym (symbol-name symbol)))
                             variables)))
        (assert (eq (car variable) :values))
        `(multiple-value-bind ,values ,form
           ,@(mapcar (lambda (variable value)
                       `(defparameter ,variable ,value))
                     variables values)
           (values ,@values)))))
