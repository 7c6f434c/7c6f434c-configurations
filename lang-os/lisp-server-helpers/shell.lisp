(defpackage :shell
  (:use :common-lisp)
  (:export
    #: escape-for-shell
    #: run-program-return-code
    #: run-program-return-success
    ))

(in-package :shell)

(defun escape-for-shell (s)
  (concatenate
    'string "'"
    (cl-ppcre:regex-replace-all "'" s "'\\\\''")
    "'"))

(defmacro run-program-return-code (&body body)
  `(multiple-value-bind
     (stdout stderr exit-code)
     (handler-bind
       ((uiop/run-program:subprocess-error (lambda (e) e (continue))))
       (progn
         ,@ body))
     stdout
     stderr
     exit-code))

(defmacro run-program-return-success (&body body)
  `(= 0 (run-program-return-code ,@body)))
