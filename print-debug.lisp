;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-
(cl:defpackage #:print-debug
  (:use #:common-lisp #:alexandria)
  (:nicknames #:pide)
  (:export
   #:d
   #:v
   #:p
   #:s))

(in-package #:print-debug)

(defun print-time-stamp ()
  "Print a time stamp, return its length."
  (multiple-value-bind (second minute hour) (get-decoded-time)
    (let ((string (format nil "~2,'0d:~2,'0d:~2,'0d" hour minute second)))
      (format *error-output* string)
      (length string))))

(defun print-spacer (length)
  "Print LENGH spaces."
  (loop repeat length do (princ #\space *error-output*)))

(defun d (control-string &rest arguments)
  "Print formatted arguments, preceded by a timestamp.  A closing newline is appended."
  (print-time-stamp)
  (princ #\space *error-output*)
  (apply #'format *error-output* control-string arguments)
  (format *error-output* "~&"))

(defmacro v (&rest forms)
  "Print formatted form=value pairs (usind D).  :/ prints a fresh line (aligned)."
  (with-unique-names (n)
    `(let ((,n (print-time-stamp)))
       (declare (ignorable ,n))
       ,@(mapcar (lambda (form)
                   (cond
                     ((eq form :/)
                      `(progn
                         (fresh-line *error-output*)
                         (print-spacer ,n)))
                     ((stringp form) `(format *error-output* " ~a" ,form))
                     (t `(format *error-output* " ~a=~a" ',form ,form))))
                 forms)
       (fresh-line *error-output*))))

(defmacro p (form &optional (label form))
  "Print form, then print and return its value.  Only handles a single value.  An optional label may be given."
  (with-unique-names (value)
    `(progn
       (d ,(format nil "~A =>" label))
       (let ((,value ,form))
         (d "    => ~A" ,value)
         ,value))))

(defmacro s (variable form)
  "Save the result of FROM in VARIABLE, then return them.  (:VALUES VAR...) is also recognized,for multiple values."
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
