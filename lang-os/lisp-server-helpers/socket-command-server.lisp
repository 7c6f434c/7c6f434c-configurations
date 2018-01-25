(defpackage :socket-command-server
  (:use :common-lisp :references :safe-read)
  (:export
    ))
(in-package :socket-command-server)

(defpackage :socket-command-server-commands
  (:use)
  (:export
    ))

(defun eval-command-form
  (form &optional
        (context (with-reference (context equal) (function context))))
  (handler-case
    (progn
      (assert (consp form))
      (assert (stringp (first form)))
      (check-safety form nil)
      (let*
        ((op-sym (find-symbol (string-upcase (first form))
                              :socket-command-server-commands)))
        (assert op-sym)
        (assert (symbol-function op-sym))
        (list "value" (apply op-sym context (rest form)))))
    (error (e) (list "error" (format nil "~a" e) e))))

(defun socket-command-server-commands::ping
  (context &optional (reply "alive")) context reply)
(defun socket-command-server-commands::error
  (context &optional (reply "failed")) context (error reply))
(defun socket-command-server-commands::finish
  (context) (funcall context :continue nil) nil)

(defun eval-socket-connection-handler (stream peer)
  (with-reference 
    (context equal)
    (context :continue t)
    (context :peer peer)
    (context :connection stream)
    (loop
      with context := (function context)
      while (context :continue)
      for input := (multiple-value-list (ignore-errors (safe-read stream nil)))
      for value :=
      (if (second input)
        (list "error" (format nil "~a" (second input)) (second input))
        (eval-command-form context (first input)))
      do (format stream "~s~%" value)
      do (finish-output stream)
      when (equal (first value) "error")
      do (format t "Error in connection handler for peer ~a:~%~a~%"
                 peer (second value)))))

(defun eval-socket-runner (name)
  (ignore-errors (delete-file name))
  (let*
    ((socket
       (iolib:make-socket
         :connect :passive :address-family :local
         :type :stream :local-filename name)))
    (unwind-protect
      (progn
        (iolib:listen-on socket)
        (iolib/syscalls:chmod name #8r0777)
        (loop
          for connection :=
          (multiple-value-list
            (iolib:accept-connection socket :wait 1 :external-format :utf-8))
          for accepted-socket := (car connection)
          for peer := (cdr connection)
          when accepted-socket
          do 
          (let* ((accepted-socket accepted-socket)
                 (peer peer))
            (bordeaux-threads:make-thread
              (lambda ()
                (unwind-protect
                  (eval-socket-connection-handler accepted-socket peer)
                  (close accepted-socket)))
              :name (format nil "Connection handler for ~a" peer)))))
      (close socket))))
