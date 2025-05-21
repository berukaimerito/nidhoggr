;; In temp-package.lisp
(defpackage :temp-package
  (:use :common-lisp)
  (:export :temp-func))

(in-package :temp-package)

(defvar *egg* 9)

(defun temp-func ()
  (print "Hello from temp package"))
