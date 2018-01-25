(defpackage :daemon
  (:use :common-lisp :shell :timestamp)
  (:export
    #:file-used
    #:console-used
    #:periodically
    ))
(in-package :daemon)

(defun file-used (filename)
  (run-program-return-success
    (uiop:run-program (list "fuser" filename))))

(defun console-used (n)
  (file-used (format nil "/dev/tty~d" n)))

(defmacro periodically ((period &key silently) &body body)
  (let*
    ((res (gensym)))
    `(loop
       for ,res := (list ,@body)
       ,@(unless silently
           `(
             do (format t "Results:~%~{~s~%~}" ,res)
             do (format t "~s~%~%" (local-time:now))))
       do (sleep ,period))))

(defmacro with-logging ((name) &body body)
  (let
    ((timestamp (gensym)))
    `(progn
       (ensure-directories-exist
         (format nil "/var/log/system-lisp-logs/~a/" ,name))
       (let
         ((,timestamp (timestamp)))
         (with-open-file
           (*standard-output*
             (format
               nil
               "/var/log/system-lisp-logs/~a/stdout-~a.log"
               ,name ,timestamp)
             :direction :output)
           (with-open-file
             (*error-output*
               (format
                 nil
                 "/var/log/system-lisp-logs/~a/stderr-~a.log"
                 ,name ,timestamp)
               :direction :output)
             ,@body))))))

(defun run-with-logging (name command &rest options)
  (with-logging
    ((format 
       nil "~a/~a"
       name (cl-ppcre:regex-replace-all "^.*/" command "")))
    (apply 
      'uiop:run-program command
      :output *standard-output* :error-output *error-output*
      :input "/dev/null" options)))

(defun start-with-logging (name command &rest options)
  (with-logging
    ((format 
       nil "~a/~a"
       name (cl-ppcre:regex-replace-all "^.*/" command "")))
    (apply 
      'uiop:launch-program command
      :output *standard-output* :error-output *error-output*
      :input "/dev/null" options)))

(defun daemon-with-logging (name command &rest options)
  (with-logging
    ((format 
       nil "~a/~a"
       name (cl-ppcre:regex-replace-all "^.*/" command "")))
    (apply 
      'uiop:run-program "setsid" command
      :output *standard-output* :error-output *error-output*
      :input "/dev/null" options)))
