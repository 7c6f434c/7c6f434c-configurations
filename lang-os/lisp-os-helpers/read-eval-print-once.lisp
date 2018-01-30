(defpackage :lisp-os-helpers/read-eval-print-once
  (:use :common-lisp)
  (:export
    #:read-eval-print
    #:package-rep)
  (:nicknames :rep1))
(in-package :lisp-os-helpers/read-eval-print-once)

(defun read-eval-print ()
  (handler-case (format t "~s~%" (eval (read)))
    (t (e)
       (format *error-output* "~%~%~a~%" e)
       (trivial-backtrace:print-backtrace-to-stream *error-output*)
       (format *error-output* "~a~%" e))))

(defun package-rep (package)
  (use-package package)
  (read-eval-print))
