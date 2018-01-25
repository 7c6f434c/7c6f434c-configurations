(defpackage :shell
  (:use :common-lisp)
  (:export
    #:escape-for-shell
    #:run-program-return-code
    #:run-program-return-success
    #:which
    #:program-output-lines
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

(defun which (cmd)
  (uiop:run-program
    (list "which" cmd)
    :output '(:string :stripped t)))

(defun program-output-lines (command &key
                                     ignore-error-status
                                     input-string input-file input-stdin)
  (multiple-value-bind
    (stdout stderr result)
    (with-input-from-string (s (or input-string ""))
      (uiop:run-program
        command
        :ignore-error-status ignore-error-status
        :output '(:string :stripped t)
        :input (cond
                 (input-file input-file)
                 (input-string s)
                 (input-stdin t)
                 (t nil))))
    stderr
    (values
      (cl-ppcre:split
        (format nil "(~a|~a|~a~a|~a~a)" #\Return #\Newline #\Return #\Newline #\Newline #\Return)
        stdout)
      result)))
