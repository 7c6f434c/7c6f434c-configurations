(defpackage :lisp-os-helpers/subuser-x
  (:use
    :common-lisp
    :lisp-os-helpers/shell
    :lisp-os-helpers/nix
    :lisp-os-helpers/timestamp
    :lisp-os-helpers/socket-command-client)
  (:export
    #:reset-firefox-launcher
    #:subuser-command-with-x
    #:subuser-firefox
    #:firefox-pref-value-js
    ))
(in-package :lisp-os-helpers/subuser-x)

(defvar *firefox-profile-contents* nil)
(defvar *firefox-launcher* nil)

(defun
  subuser-command-with-x
  (command 
    &key
    display
    name environment options system-socket)
  (let*
    ((display
       (or
	 display
	 (ignore-errors
	   (parse-integer
	     (subseq (uiop:getenv "DISPLAY") 1)))
	 0))
     (name (or name (timestamp-usec-recent-base36)))
     (uid 
       (take-reply-value 
	 (with-system-socket
	   ()
	   (ask-server (with-uid-auth `(subuser-uid ,name))))))
     (x-socket (format nil "/tmp/.X11-unix/X~a" display))
     )
    (uiop:run-program
      (list "setfacl" "-m" (format nil "u:~a:rw" uid)
	    x-socket))
    (with-system-socket
      (system-socket)
      (take-reply-value
	(ask-server
	  (with-uid-auth
	    `(run-as-subuser
	       ,name
	       ,command
	       (,@environment
		 ("DISPLAY" ,(format nil ":~a" display)))
	       ,(loop
		  for o in options
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
		    (t o))))))))))

(defun reset-firefox-launcher (&key profile-contents nix-path nix-wrapper-file)
  (setf *firefox-profile-contents* profile-contents)
  (setf *firefox-launcher*
	(nix-build "firefoxLauncher" :nix-file nix-wrapper-file :nix-path nix-path)))

(defun firefox-pref-value-js (value)
  (cond
    ((null value) "false")
    ((equal t value) "true")
    ((numberp value) value)
    ((stringp value) (format nil "~s" value))
    (t (error "Unrecognised preference type for value ~s" value))))

(defun subuser-firefox
  (arguments &key display prefs
	     environment marionette-socket home profile-storage name
	     (firefox-launcher *firefox-launcher*) (slay t) (wait t)
	     (netns t) network-ports pass-stderr mounts system-socket)
  (with-system-socket
    (system-socket)
    (when pass-stderr
      (send-fd-over-unix-socket
	(concatenate
	  'string (string #\Null)
	  (take-reply-value (ask-server `(fd-socket))))
	2)
      (ask-server `(receive-fd stderr)))
    (prog1
      (subuser-command-with-x
	`(,firefox-launcher ,@arguments)
	:display display :name name
	:environment
	`(
	  ,@ environment
	  ("MARIONETTE_SOCKET" ,(or marionette-socket ""))
	  ("HOME" ,(or home ""))
	  ("FIREFOX_PROFILE" ,(or profile-storage ""))
	  ("FIREFOX_EXTRA_PREFS"
	   ,(format
	      nil "~{user_pref(\"~a\",~a);~%~}"
	      (loop 
		for pair in prefs
		for name := (first pair)
		for value := (second value)
		for representation :=
		(firefox-pref-value-js value))))
	  )
	:options
	`(
	  ,@(when slay `("slay"))
	  ,@(when wait `("wait"))
	  ("nsjail" "network" ("mounts" ,mounts))
	  ,@(when netns `(("netns" ,network-ports)))
	  ,@(when pass-stderr `(("stderr-fd" "stderr"))))
	:system-socket *ambient-system-socket*)
      (ask-server `(close-received-fds)))))
