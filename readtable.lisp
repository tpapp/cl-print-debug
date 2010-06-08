;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:debug-tools)

(defun debug-declaration (stream c n)
  (declare (ignore stream c))
  (if n
      (assert (<= 0 n 3))
      (setf n 3))
  `(declare (optimize (debug ,n))))

(defreadtable d-syntax
  (:merge :standard)
  (:dispatch-macro-char #\# #\d #'debug-declaration))
