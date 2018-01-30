(defpackage :lisp-os-helpers/socket-command-client
  (:use :common-lisp :lisp-os-helpers/safe-read :lisp-os-helpers/socket-command-server)
  (:export
    #:ask-server
    #:coerce-to-socket-stream
    #:get-current-user-name
    #:with-uid-auth
    #:with-presence-auth
    #:with-strong-presence-auth
    #:with-password-auth
    #:with-system-socket
    ))
(in-package :lisp-os-helpers/socket-command-client)

(defvar *ambient-system-socket* nil)

(defun symbols-to-string (form)
  (etypecase form
    ((or string number null) form)
    (symbol
      (if (equal (symbol-package form) (find-package :keyword))
        (string-downcase (symbol-name form))
        (symbol-name form)))
    (cons
      (cons
        (symbols-to-string (car form))
        (symbols-to-string (cdr form))))))

(defun send-to-server (stream form)
  (format stream "~s~%" (symbols-to-string form))
  (finish-output stream))

(defun read-from-server (stream)
  (safe-read stream nil))

(defun skip-server-messages (stream)
  (let*
    ((marker (format nil "~36r" (random (expt 36 40)))))
    (send-to-server stream `(ping ,marker))
    (loop
      for reply := (read-from-server stream)
      while (not (equal (second reply) marker)))))

(defun ask-server (form &key (stream *ambient-system-socket*))
  (let
    ((stream (coerce-to-socket-stream stream)))
    (skip-server-messages stream)
    (send-to-server stream form)
    (read-from-server stream)))

(defun coerce-to-socket-stream (socket)
  (etypecase socket
    ((or string pathname)
     (iolib:make-socket
       :connect :active :address-family :local :type :stream
       :remote-filename socket))
    (stream socket)
    (null (coerce-to-socket-stream *system-lisp-socket*))))

(defmacro with-system-socket ((&optional socket) &body body)
  `(let*
     ((*ambient-system-socket* (coerce-to-socket-stream ,socket)))
     ,@body))

(defun get-current-user-name ()
  (iolib/syscalls:getpwuid (iolib/syscalls:getuid)))

(defun take-reply-value (reply)
  (assert (equal "value" (first reply)))
  (second reply))

(defun with-uid-auth (form &key (socket *ambient-system-socket*) user)
  (let*
    ((user (or user (get-current-user-name)))
     (socket (coerce-to-socket-stream socket))
     (challenge (take-reply-value (ask-server `(request-uid-auth ,user) :stream socket)))
     (answer (with-open-file (f challenge) (read-line f nil nil))))
    `(with-uid-auth ,answer ,form)))

(defun with-presence-auth (prompt form &key (timeout 15))
  `(with-presence-auth ,prompt ,form ,timeout))
(defun with-strong-presence-auth (prompt form &key (timeout 15))
  `(with-strong-presence-auth ,prompt ,form ,timeout))
(defun with-password-auth (prompt form &key user (timeout 15))
  `(with-password-auth ,prompt ,form ,(or user (get-current-user-name)) ,timeout))
