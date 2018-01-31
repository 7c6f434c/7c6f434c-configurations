(defpackage :lisp-os-helpers/socket-command-server
  (:use :common-lisp
	:lisp-os-helpers/references :lisp-os-helpers/safe-read
	:lisp-os-helpers/auth-data :lisp-os-helpers/fbterm-requests)
  (:export
    #:eval-socket-runner
    #:require-presence
    #:require-uid
    #:require-root
    #:require-password
    #:take-reply-value
    #:*system-lisp-socket*
    ))
(in-package :lisp-os-helpers/socket-command-server)

(defpackage :socket-command-server-commands
  (:use)
  (:export
    ))

(defvar *system-lisp-socket* "/run/system-lisp-socket")

(defvar *socket-command-package* :socket-command-server-commands)

(defun eval-command-form
  (form &optional
        (context (with-reference (context equal) (function context))))
  (handler-case
    (progn
      (unless (consp form) (error "No point in evaluating atom ~s" form))
      (unless (stringp (first form))
	(error 
	  "Fist element of the form should be a command name as a string, found ~s"
	  (first form)))
      (check-safety form nil)
      (let*
        ((op-sym (find-symbol (string-upcase (first form))
                              *socket-command-package*)))
        (unless
	  (and op-sym (symbol-function op-sym))
	  (error "Operation ~a (using package: ~a) not known"
		 (first form) *socket-command-package*))
        (list "value" (apply op-sym context (rest form)))))
    (error (e) (values (list "error" (format nil "~a" e)) e))))

(defun take-reply-value (reply)
  (if
    (equal "value" (first reply))
    (second reply)
    (error (second reply))))

(defun socket-command-server-commands::ping
  (context &optional (reply "alive")) context reply)
(defun socket-command-server-commands::error
  (context &optional (reply "failed")) context (error reply))
(defun socket-command-server-commands::finish (context)
  (funcall context :continue nil) "")

(defun socket-command-server-commands::request-uid-auth (context user)
  (flet ((context (&rest args) (apply context args)))
    (context :uid-auth-claimed-user user)
    (context :uid-auth-challenge (create-file-challenge :users (list user)))
    (context :uid-auth-challenge)))
(defun socket-command-server-commands::with-uid-auth
  (context answer form)
  (flet
    ((context (&rest args) (apply context args)))
    (unless
      (verify-file-challenge (context :uid-auth-challenge) answer)
      (error "File challenge for UID authentication failed"))
    (context :uid-auth-verified-user (context :uid-auth-claimed-user))
    (take-reply-value (eval-command-form form context))))
(defun socket-command-server-commands::with-presence-auth
  (context prompt form timeout)
  (flet
    ((context (&rest args) (apply context args)))
    (unless
      (fbterm-request
	(format
	  nil "Peer ~s (UID ~s) wants to use a privileged invocation:~%~s~%It says:~%~s~%"
	  (context :peer) (context :uid-auth-verified-user) form prompt)
	:timeout timeout)
      (error "User confirmation has not been received"))
    (context :presence-auth-confirmed :weak)
    (unwind-protect
      (take-reply-value (eval-command-form form context))
      (context :presence-auth-confirmed nil))))
(defun socket-command-server-commands::with-strong-presence-auth
  (context prompt form timeout)
  (flet
    ((context (&rest args) (apply context args)))
    (unless
      (fbterm-request
	(format
	  nil "Peer ~s (UID ~s) wants to use a privileged invocation:~%~s~%It says:~%~s~%"
	  (context :peer) (context :uid-auth-verified-user) form prompt)
	:pre-prompt
	(format
	  nil "Peer ~s (UID ~s) wants to use a privileged invocation:~%~s~%It says:~%~s~%Press Enter to allow"
	  (context :peer)  (context :uid-auth-verified-user) form prompt)
	:timeout timeout)
      (error "User confirmation has not been received"))
    (context :presence-auth-confirmed :strong)
    (unwind-protect
      (take-reply-value (eval-command-form form context))
      (context :presence-auth-confirmed nil))))
(defun socket-command-server-commands::with-password-auth
  (context prompt form user timeout)
  (flet
    ((context (&rest args) (apply context args)))
    (let*
      ((password 
	 (fbterm-request
	   (format
	     nil "Peer ~s (UID ~s) asks the ~s password for a privileged invocation:~%~s~%It says:~%~s~%"
	     (context :peer) (context :uid-auth-verified-user) user form prompt)
	   :pre-prompt
	   (format
	     nil "Peer ~s (UID ~s) asks the ~s password for a privileged invocation:~%~s~%It says:~%~s~%"
	     (context :peer) (context :uid-auth-verified-user) user form prompt)
	   :timeout timeout :hide-entry t)
	 ))
      (format t "Received password: ~s~%" password)
      (unless
	(check-password user password)
	(error "Correct password for user ~a not received" user)))
    (context :presence-auth-confirmed :strong)
    (context :password-auth-user user)
    (unwind-protect
      (take-reply-value (eval-command-form form context))
      (context :password-auth-user nil)
      (context :presence-auth-confirmed nil))
    ))

(defun socket-command-server-commands::request-string
  (context prompt timeout)
  (let
    ((res 
       (fbterm-request
         (format
           nil "Peer ~s (UID ~s) asks to answer a question.~%It says:~%~s~%"
           (funcall context :peer) (funcall context :uid-auth-verified-user) prompt)
         :pre-prompt
         (format
           nil "Peer ~s (UID ~s) asks to answer a question.~%It says:~%~s~%"
           (funcall context :peer) (funcall context :uid-auth-verified-user) prompt)
         :timeout timeout :hide-entry nil)))
    (unless res (error "Requesting a string failed"))
    res))
(defun socket-command-server-commands::request-secret
  (context prompt timeout)
  (let
    ((res 
       (fbterm-request
         (format
           nil "Peer ~s (UID ~s) asks to enter a secret.~%It says:~%~s~%"
           (funcall context :peer) (funcall context :uid-auth-verified-user) prompt)
         :pre-prompt
         (format
           nil "Peer ~s (UID ~s) asks to enter a secret.~%It says:~%~s~%"
           (funcall context :peer) (funcall context :uid-auth-verified-user) prompt)
         :timeout timeout :hide-entry t)))
    (unless res (error "Requesting a string failed"))
    res))

(defun eval-socket-connection-handler (stream peer)
  (ignore-errors
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
	  (list "error" (format nil "~a" (second input)))
	  (eval-command-form (first input) context))
	do (format stream "~s~%" value)
	do (finish-output stream)
	when (equal (first value) "error")
	do (format *error-output* "Error in connection handler for peer ~a:~%~a~%"
		   peer (second value))
	finally (close-received-fds context)))))

(defun eval-socket-runner (name)
  (ignore-errors (delete-file name))
  (let*
    ((socket
       (iolib:make-socket
	 :connect :passive :address-family :local
	 :type :stream :local-filename name)))
    (format t "Created system socket ~s at ~s~%" socket name)
    (unwind-protect
      (progn
	(iolib/syscalls:chmod name #8r0777)
	(iolib:listen-on socket)
	(format t "Starting to listen on the system socket ~s~%" socket)
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
	    (format t "Accepted system socket connection ~s~%"
		    accepted-socket)
	    (bordeaux-threads:make-thread
	      (lambda ()
		(unwind-protect
		  (eval-socket-connection-handler accepted-socket peer)
		  (ignore-errors (close accepted-socket))))
	      :name (format nil "Connection handler for ~a" peer)))))
      (close socket))))

(defun require-presence (context)
  (unless
    (or (funcall context :presence-auth-confirmed)
        (funcall context :password-auth-user))
    (error "User presence has not been confirmed")))

(defun require-uid (context user)
  (unless
    (or (equal user (funcall context :uid-auth-verified-user))
        (equal user (funcall context :password-auth-user)))
    (error "Expected UID or password authentication for user ~a" user)))

(defun require-root (context)
  (require-uid context "root"))

(defun require-password (context user)
  (unless
    (equal user (funcall context :password-auth-user))
    (error "Expected password authentication for user ~a" user)))

(defun close-received-fds (context)
  (loop
    for fd in
    (funcall context :fd-socket-received-fs)
    do (ignore-errors (iolib/syscalls:close fd)))
  (funcall context :fd-socket-fd-plist)
  (funcall context :fd-socket-received-fs nil))

(defun socket-command-server-commands::close-received-fds (context)
  (close-received-fds context)
  "OK")
(defun socket-command-server-commands::fd-socket (context)
  (or
    (funcall context :fd-socket)
    (let*
      ((key (format nil "~36r" (random (expt 36 20))))
       (socket-address (concatenate 'string (string #\Null) key)))
      (funcall
        context :fd-socket
        (iolib:make-socket
          :connect :passive :address-family :local :type :datagram
          :local-filename socket-address))
      (socket-command-server-commands::close-received-fds context)
      key)))
(defun socket-command-server-commands::receive-fd (context tag)
  (let*
    ((fd (iolib/sockets:receive-file-descriptor
           (funcall context :fd-socket))))
    (funcall context :fd-socket-received-fs
             (cons fd (funcall context :fd-socket-received-fs)))
    (funcall context :fd-socket-fd-plist
             (append
               (list (intern (string-upcase tag) :keyword) fd)
               (funcall context :fd-socket-fd-plist)))
    fd))

(defun socket-command-server-commands::set-brightness (context n)
  (unless (ignore-errors (require-root context) t)
    (require-presence context))
  (let*
    ((f
       (loop
         for name in
         (list "intel_backlight" "acpi_backlight" "acpi_backlight0" "nv_backlight" "radeon_backlight")
         for file := (format nil "/sys/class/backlight/~a/brightness" name)
         when (probe-file file) return file
         finally (return (first (directory "/sys/class/backlight/*/brightness"))))))
    (alexandria:write-string-into-file (format nil "~a" n) f)))
(defun socket-command-server-commands::system-shutdown (context)
  (unless (ignore-errors (require-root context) t)
    (require-presence context))
  (uiop:run-program (list "kill" "-USR1" "1")))
(defun socket-command-server-commands::system-poweroff (context)
  (unless (ignore-errors (require-root context) t)
    (require-presence context))
  (uiop:run-program (list "/run/current-system/bin/poweroff")))
(defun socket-command-server-commands::system-reboot (context)
  (unless (ignore-errors (require-root context) t)
    (require-presence context))
  (uiop:run-program (list "/run/current-system/bin/reboot")))