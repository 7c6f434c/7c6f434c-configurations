(defpackage :lisp-os-helpers/subuser-x
  (:use
    :common-lisp
    :lisp-os-helpers/shell
    :lisp-os-helpers/timestamp
    :lisp-os-helpers/socket-command-client)
  (:export
    #: subuser-command-with-x
    #: set-firefox-path
    #: set-marionette-python-path
    ))
(in-package :lisp-os-helpers/subuser-x)

(defvar *firefox-path* nil)
(defvar *marionette-python-path* nil)
(defvar *marionette-search-path* nil)

(defun calculate-firefox-path (&rest arguments &key
				     (package "firefox") (filename "firefox"))
  (if
    (probe-file (format nil "/~a/bin/~a" package filename))
    (format nil "/~a/bin/~a" package filename)
    (format
      nil "/~a/bin/~a"
      (apply
	'nix-build package
	:allow-other-keys t arguments) filename)))

(defun set-firefox-path (&rest arguments)
  (setf *firefox-path* (apply 'calculate-firefox-path arguments)))

(defun ensure-firefox-path (&rest arguments)
  (unless *firefox-path* (apply 'set-firefox-path arguments)))

(defun calculate-marionette-python-path
  (&rest arguments &key (package "python2Packages.marionette-harness"))
  (let*
    ((marionette-package
       (if (probe-file (format nil "/~a" package))
	 (format nil "/~a" package)
	 (apply 'nix-build package :allow-other-keys t arguments))))
    (uiop:with-temprorary-file
      (:path p)
      (nix-shell-for-packages 
	(list "sh" "-c"
	      (format nil "echo \"$PYTHONPATH\" > ~a" (escape-for-shell p)))
	marionette-package)
      (alexandria:read-file-into-string p))))

(defun set-marionette-python-path (&rest arguments)
  (setf *marionette-python-path*
	(apply 'calculate-marionette-python-path arguments)))

(defun ensure-marionette-python-path (&rest arguments)
  (unless *marionette-python-path*
    (apply 'set-marionette-python-path arguments)))

(defun calculate-marionette-search-path
  (&rest arguments &key (package "search2Packages.marionette-harness"))
  (let*
    ((marionette-package
       (if (probe-file (format nil "/~a" package))
	 (format nil "/~a" package)
	 (apply 'nix-build package :allow-other-keys t arguments))))
    (uiop:with-temprorary-file
      (:path p)
      (nix-shell-for-packages 
	(list "sh" "-c"
	      (format nil "echo \"$PATH\" > ~a" (escape-for-shell p)))
	"--pure"
	marionette-package)
      (alexandria:read-file-into-string p))))

(defun set-marionette-search-path (&rest arguments)
  (setf *marionette-search-path*
	(apply 'calculate-marionette-search-path arguments)))

(defun ensure-marionette-search-path (&rest arguments)
  (unless *marionette-search-path*
    (apply 'set-marionette-search-path arguments)))

(defun marionette-socket-comand (port socket &key python-path search-path)
  (ensure-marionette-python-path)
  (ensure-marionette-search-path)
  (let*
    ((launch-marionette
       `("python" "-i" "-c"
	 ,(format
	    nil
	    "from marionette_driver.marionette import Marionette; session = Marionette(); session.port = ~a; session.start_session();"
	    port)))
     (launch-marionette-with-env
       (add-command-env 
	 launch-marionette
	 `(
	   ("PYTHONPATH" ,(or pythonpath *marionette-python-path*))
	   ("PATH" ,(or pythonpath *marionette-search-path*))
	   )))
     )))

(defun firefox-command (&key firefox-path
			     (display 0)
			     home profile profile-template
			     (marionette t) (marionette-port 2828)
			     (marionette-socket nil))
  (ensure-firefox-path)
  (ensure-marionette-python-path)
  (ensure-marionette-search-path)
  (let*
    ((main-command
       (add-command-env
	 `(("DISPLAY" ,(format nil ":~a" display))
	   ("PATH" ,(uiop:getenv "PATH")))

(defun
  subuser-command-with-x
  (command 
    &key
    (display
      (or
	(ignore-errors
	  (parse-integer
	    (subseq (uiop:getenv "DISPLAY") 1)))
	0))
    name environment options)
  (let*
    ((name (or name (timestamp-usec-recent-base36)))
     (uid (with-system-socket
	    ()
	    (ask-server (with-uid-auth `(subuser-uid ,name)))))
     (x-socket (format nil "/tmp/.X11-unix/X~a" display))
     )
    (uiop:run-program
      (list "setfacl" "-m" (format nil "u:~a:rw" uid)
	    xsocket))
    (with-system-socket
      ()
      (ask-server
	(with-uid-auth
	  `(run-as-subuser
	     ,name
	     ,command
	     (,@environment
	       ("DISPLAY" ,(format nil ":~a" display)))
	     ,(loop
		for o in option
		collect
		(cond
		  ((and (listp o) (equalp (string (first o)) "nsjail"))
		   (cons
		     "nsjail"
		     (loop
		       for oo in (rest o)
		       collect
		       (cond
			 ((and (listp oo)
			       (equalp (string (first oo)) "mounts"))
			  (list
			    "mounts"
			    (append
			      (second oo)
			      (list (list "-B" x-socket x-socket)))))
			 (t oo)))))
		  (t o)))))))))
