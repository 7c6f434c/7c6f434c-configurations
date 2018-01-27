(defpackage :socket-command-client
  (:use :common-lisp)
  (:export
    #:ask-server
    #:coerce-to-socket-stream
    #:get-current-user-name
    #:with-file-auth
    #:with-presence-auth
    #:with-strong-presence-auth
    #:with-password-auth
    ))
(in-package :socket-command-client)

(defun symbols-to-string (form)
  (etypecase (form)
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
    ((marker (format "~36r" (expt 36 40))))
    (send-to-server stream `(ping ,marker))
    (loop
      for reply := (read-from-server stream)
      while (not (equal reply marker)))))

(defun ask-server (stream form)
  (skip-server-messages stream)
  (send-to-server stream form)
  (read-from-server form))

(defun coerce-to-socket-stream (socket)
  (etypecase socket
    ((or string pathname)
     (iolib:make-socket
       :connect :active :address-family :local :type :stream
       :remote-filename socket))
    (stream socket)))

(defun get-current-user-name ()
  (iolib/syscalls:getpwuid (iolib/syscalls:getuid)))

(defun with-file-auth (socket form &optional user)
  (let*
    ((user (or user (get-current-user-name)))
     (socket (coerce-to-socket-stream socket))
     (challenge (ask-server socket `(request-auid-auth ,user)))
     (answer (with-open-file (f challenge) (read-line f nil nil))))
    `(with-uid-auth ,answer ,form)))

(defun with-presence-auth (socket prompt form &key (timeout 15))
  `(with-presence-auth ,prompt ,form ,timeout))
(defun with-strong-presence-auth (socket prompt form &key (timeout 15))
  `(with-strong-presence-auth ,prompt ,form ,timeout))
(defun with-password-auth (socket prompt form &key user (timeout 15))
  `(with-password-auth ,prompt ,form ,(or user (get-current-user-name)) ,timeout))
