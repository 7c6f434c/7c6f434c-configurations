(defpackage :lisp-os-helpers/socket-command-definitions
  (:use :common-lisp
	:lisp-os-helpers/shell
	:lisp-os-helpers/subuser
	:lisp-os-helpers/daemon
	:lisp-os-helpers/socket-command-server
	:lisp-os-helpers/vt
	:lisp-os-helpers/util
	:lisp-os-helpers/auth-data
	:lisp-os-helpers/fbterm-requests
	:lisp-os-helpers/kernel
	:lisp-os-helpers/unix-users
	)
  (:export
    #:start-x-allowed-p
    #:grab-device-allowed-p
    ))
(in-package :lisp-os-helpers/socket-command-definitions)

(defun socket-command-server-commands::load (context path)
  (require-root context)
  (load path) "OK")
(defun socket-command-server-commands::eval (context code)
  (require-root context)
  (eval (read-from-string code)))

(defun socket-command-server-commands::chvt (context vtn)
  (require-presence context)
  (chvt vtn))

(defun-weak start-x-allowed-p (context display) (declare (ignorable context display)) nil)

(defun socket-command-server-commands::start-x (context &optional (display 0))
  (unless
    (ignore-errors (start-x-allowed-p context display))
    (error "Starting X on :~a is not allowed" display))
  (let*
    ((test-command
       (list
	 "env"
	 (format nil "DISPLAY=:~d" display)
	 "xprop" "-root")
       ))
    (when
      (run-program-return-success (uiop:run-program test-command))
      (error "There is already a display at ~a" display))
    (system-service display "from-nixos/xorg" (format nil "~a" display))
    (loop
      for ping from 1 to 100
      while (not (run-program-return-success
		   (uiop:run-program test-command)))
      do (sleep 0.3))
    (when
      (context-uid context)
    (uiop:run-program
      (list
	"chown"
	(format nil "~a:~a" (context-uid context) 0)
	(format nil "/tmp/.X11-unix/X~a" display)))
    (iolib/syscalls:chmod (format nil "/tmp/.X11-unix/X~a" display) #o700))
    (format nil "~a" display)))

(defun socket-command-server-commands::run-as-subuser
  (context name command environment options)
  (let
    ((value
       (apply 
         'run-as-subuser (context-uid context) command
         :name (when (> (length name) 0) name)
         :environment environment
         (loop
           for o in options
           if (equalp o "pty") append (list :pty t)
           if (equalp o "wait") append (list :wait t)
           if (equalp o "slay") append (list :slay t)
           if (equalp o "slurp") append (list :slurp-stdout t)
           if (equalp o "slurp-output") append (list :slurp-stdout t)
           if (equalp o "slurp-stdout") append (list :slurp-stdout t)
           if (equalp o "slurp-stderr") append (list :slurp-stderr t)
           if (and (listp o) (equalp (first o) "feed-stdin"))
           append (list :feed-stdin (second o))
           if (and (listp o) (equalp (first o) "directory"))
           append (list :directory (second o))
           if (and (listp o) (equalp (first o) "chroot"))
           append (list :chroot (second o))
           if (and (listp o) (equalp (first o) "masking-mounts"))
           append (list :masking-mounts
                        (loop 
                          for (source target . options) in (second o)
                          collect 
                          (append (list source target)
                                  (loop
                                    for opt in options
                                    collect 
                                    (intern (string-upcase opt) :keyword)))))
           if (and (listp o) (equalp (first o) "fake-groups"))
           append (list :fake-groups (second o))
           if (equalp o "fake-passwd") append (list :fake-passwd t)
           if (and (listp o) (equalp (first o) "fake-usernames"))
           append (list :fake-usernames (second o))
           if (equalp o "clear-env") append (list :clear-env t)
           if (and (listp o) (equalp (first o) "nsjail"))
           append (list
                    :nsjail t :nsjail-settings
                    (loop
                      for oo in (rest o)
                      if (equalp oo "network") append (list :network t)
                      if (equalp oo "no-network") append (list :network nil)
                      if (equalp oo "skip-default-mounts") append (list :skip-default-mounts t)
                      if (equalp oo "no-proc") append (list :proc nil :proc-rw nil)
                      if (equalp oo "proc-rw") append (list :proc-rw t)
                      if (equalp oo "proc-ro") append (list :proc-rw nil)
                      if (equalp oo "full-dev") append (list :full-dev t)
                      if (equalp oo "fhs-current-system") append (list :fhs-current-system t)
                      if (and (listp oo) (equalp (first oo) "fhs-from"))
                      append (list :fhs-from (second oo))
                      if (equalp oo "fake-passwd") append (list :fake-passwd t)
                      if (equalp oo "fake-groups") append (list :fake-groups t)
                      if (equalp oo "newprivs") append (list :enable-newprivs t)
                      if (and (listp oo) (equalp (first oo) "resolv-conf"))
                      append (list :resolv-conf (second oo))
                      if (and (listp o) (equalp (first o) "machine-id"))
                      append (list :machine-id (second o))
                      if (equalp oo "verbose") append (list :verbose t)
                      if (and (listp oo) (equalp (first oo) "dev-log-socket"))
                      append (list :dev-log-socket (second oo))
                      if (and (listp oo) (equalp (first oo) "fake-groups"))
                      append (list :fake-groups (second oo))
                      if (and (listp oo) (equalp (first oo) "fake-usernames"))
                      append (list :fake-usernames (second oo))
                      if (and (listp oo) (equalp (first oo) "mounts"))
                      append (list :mounts (second oo))
                      if (and (listp oo) (equalp (first oo) "hostname"))
                      append (list :hostname (second oo))
                      if (and (listp oo) (equalp (first oo) "keep-namespaces"))
                      append (list :keep-namespaces (second oo))
                      if (and (listp oo) (equalp (first oo) "home"))
                      append (list :home (second oo))
                      ))
           if (and (listp o) (equalp (first o) "netns"))
           append (list :netns t
                        :netns-ports-out (second o)
                        :netns-ports-in (third o)
                        :netns-verbose (find "verbose" (fourth o) :test 'equal)
                        :netns-proc (not (find "no-proc" (fourth o) :test 'equal))
                        :netns-proc-rw (not (or (find "proc-ro" (fourth o) :test 'equal)
                                                (find "no-proc" (fourth o) :test 'equal)))
                        :netns-tuntap-devices
                        (second (find "tuntap-devices" (fourth o) :test 'equalp
                                      :key (lambda (x) (and x (listp x) (first x))))))
           if (and (listp o) (equalp (first o) "stdin-fd"))
           append (list :stdin-fd
                        (getf
                          (funcall context :fd-socket-fd-plist )
                          (intern (string-upcase (second o)) :keyword)))
           if (and (listp o) (equalp (first o) "stdout-fd"))
           append (list :stdout-fd
                        (getf
                          (funcall context :fd-socket-fd-plist )
                          (intern (string-upcase (second o)) :keyword)))
           if (and (listp o) (equalp (first o) "stderr-fd"))
           append (list :stderr-fd
                        (getf
                          (funcall context :fd-socket-fd-plist )
                          (intern (string-upcase (second o)) :keyword)))
           ))))
    (if (typep value 'iolib/os:process)
      (iolib/os:process-pid value) value)))

(defun socket-command-server-commands::add-persistent-subuser (context name home)
  (subuser-uid
    (context-uid context) 
    :name name :passwd-entry t
    :home (when (> (length home) 0) home)))

(defun socket-command-server-commands::drop-persistent-subuser (context name)
  (drop-subuser (context-uid context) :name name))

(defun socket-command-server-commands::select-subuser (context name)
  (select-subuser (context-uid context) :name name))
(defun socket-command-server-commands::select-subuser-by-uid (context uid)
  (second (multiple-value-list (select-subuser (context-uid context) :uid uid))))
(defun socket-command-server-commands::subuser-uid (context name)
  (subuser-uid (context-uid context) :name name :passwd-entry nil))

(defun socket-command-server-commands::chown-subuser (context path name
                                                              &optional recursive)
  (chown-subuser (context-uid context) path
                 :name (when (> (length name) 0) name)
                 :recursive (> (length recursive) 0)))

(defun-weak grab-device-allowed-p (user subuser device) (declare (ignorable user subuser device)) nil)

(defun do-grab-device (user subuser device)
  (unless
    (grab-device-allowed-p user subuser device)
    (error "User ~a is not allowed to grab device ~s for subuser ~s" user device subuser))
  (uiop:run-program
    (list
      "setfacl" "-m"
      (format nil "u:~a:rw"
	      (if subuser (subuser-uid user :name subuser) user ))
      device)))

(defun do-ungrab-device (user subuser device)
  (unless
    (grab-device-allowed-p user subuser device)
    (error "User ~a is not allowed to grab device ~s for subuser ~s" user device subuser))
  (uiop:run-program
    (list
      "setfacl" "-x"
      (format nil "u:~a"
	      (if subuser (subuser-uid user :name subuser) user ))
      device)
    :ignore-error-status t))

(defun socket-command-server-commands::grab-devices (context devices &optional subuser)
  (loop
    with user := (context-uid context)
    for d in devices
    do (do-grab-device user subuser d))
  "OK")

(defun socket-command-server-commands::ungrab-devices (context devices &optional subuser)
  (loop
    with user := (context-uid context)
    for d in devices
    do (do-ungrab-device user subuser d))
  "OK")

(defun socket-command-server-commands::set-password (context &optional password)
  (require-password context (context-uid context))
  (unless
    (set-password
      (context-uid context)
      (or password
	  (let*
	    ((first
	       (fbterm-request
		 (format 
		   nil
		   "Setting password for user ~a. Please enter your new password:~%"
		   (context-uid context))
		 :pre-prompt
		 (format 
		   nil
		   "Setting password for user ~a.~%"
		   (context-uid context))
		 :timeout 30 :hide-entry t))
	     (second
	       (when 
		 first
		 (fbterm-request
		   (format 
		     nil
		     "Setting password for user ~a. Please repeat your new password:~%"
		     (context-uid context))
		   :pre-prompt
		   (format 
		     nil
		     "Setting password for user ~a.~%"
		     (context-uid context))
		   :timeout 30 :hide-entry t))))
	    (assert first)
	    (assert (equal first second))
	    first)))
    (error "Password change failed for ~a" (context-uid context))))

(defun socket-command-server-commands::set-brightness (context n)
  (unless (ignore-errors (require-root context) t)
    (require-presence context))
  (set-brightness n))

(defun socket-command-server-commands::fuser (context filename)
  (let* ((user (context-uid context))
         (owner (file-owner-uid filename))
         (owner-name (iolib/syscalls:getpwuid owner)))
    (assert (or (equal (format nil "~a" user)
                       (format nil "~a" owner))
                (equal user owner-name))
            nil
            "File owned by ~a / ~a instead of ~a"
            owner owner-name user)
    (remove
      ""
      (cl-ppcre:split
        " "
        (uiop:run-program
          (list "fuser" filename)
          :output (list :string :stripped t)
          :ignore-error-status t))
      :test 'equal)))

(defun socket-command-server-commands::sleep (context n)
  (declare (ignorable context))
  (sleep n))
