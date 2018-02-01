(defpackage :lisp-os-helpers/subuser
  (:use :common-lisp
	:lisp-os-helpers/shell :lisp-os-helpers/global-sqlite :lisp-os-helpers/timestamp)
  (:export
    #:subuser-uid
    #:select-subuser
    #:drop-subuser
    #:slay-subuser
    #:run-as-subuser
    ))
(in-package :lisp-os-helpers/subuser)

(defvar *subuser-uid-shift* 100000)
(defvar *nogroup* 65534)
(defvar *numeric-su-helper* "/run/current-system/sw/bin/numeric-su")

(defun select-subuser (user &key uid name)
  (unless (or uid name) (error "Subuser selection requires either name or uid"))
  (unless user (error "Subuser selection requires owner"))
  (let*
    ((name (or (and name (format nil "~a.~a" user name))
	       (and uid
		    (first
		      (with-global-sqlite
			()
			(complex-global-value-by-id
			  :subusers (- uid *subuser-uid-shift*) '(:key)))))
	       (iolib/syscalls:getpwuid uid)))
     (name-uid
       (with-global-sqlite
	 ()
	 (first
	   (complex-global-value :subusers name '(:uid) nil))))
     (uid (or
	    uid
	    (third (multiple-value-list (iolib/syscalls:getpwnam name)))
	    name-uid))
     (name-suffix (and name (subseq name (1+ (length user)))))
     (owner (with-global-sqlite 
	      ()
	      (first
		(complex-global-value
		  :subusers name '(:owner) nil))))
     (passwd-line (and uid (multiple-value-list (iolib/syscalls:getpwuid uid)))))
    (when (and uid name)
      (unless
	(equal user owner)
	(error "Subuser owner is ~a instead of ~a" owner user))
      (unless
	(equal name (format nil "~a.~a" user name-suffix))
	(error "Subuser name mismatch: ~a is not ~a.~a"
	       name user name-suffix))
      (when
	(and name-uid (not (equal uid name-uid)))
	(error "Subuser name mismatch for ~a: ~a is not ~a"
	       name uid name-uid))
      (assert
	(equal
	  passwd-line
	  (multiple-value-list (iolib/syscalls:getpwnam name))))
      (values
	uid name passwd-line name-suffix owner))))

(defun subuser-uid (user &key name home gid group passwd-entry)
  (with-global-sqlite
    ()
    (let*
      ((name-suffix (or name (timestamp-usec-recent-base36)))
       (name (format nil "~a.~a" user name-suffix))
       (home (or home "/var/empty"))
       (gid (or
	      gid 
	      (and
		group
		(third (multiple-value-list (iolib/syscalls:getgrnam group))))
	      *nogroup*))
       (uid
	 (or
	   (select-subuser user :name name)
	   (if passwd-entry
	     (progn
	       (uiop:run-program
		 `("/run/current-system/bin/system-useradd"
		   "-d" ,home
		   "-g" ,(format nil "~a" gid) ,name)))
	     (progn
	       (complex-global-value 
		 :subusers name
		 `(:owner :home :uid :gid) `(:varchar :varchar :integer :integer)
		 (list user home nil gid) t)
	       (+ *subuser-uid-shift*
		  (first (complex-global-value :subusers name `(:id) nil)))
	       ))))
       )
      (complex-global-value 
	:subusers name
	`(:owner :home :uid :gid) `(:varchar :varchar :integer :integer)
	(list user home uid gid))
      (values uid name-suffix))))

(defun slay-subuser (user &key uid name)
  (multiple-value-bind
    (uid)
    (select-subuser user :uid uid :name name)
    (when uid
      (run-program-return-success
	(uiop:run-program
	  `("pkill" "-u" ,(format nil "~a" uid) "-KILL"))))))

(defun drop-subuser (user &key uid name slay)
  (multiple-value-bind
    (uid name passwd-line)
    (select-subuser user :uid uid :name name)
    (and slay uid (slay-subuser user :uid uid))
    (when passwd-line
      (uiop:run-program
	`("/run/current-system/bin/system-userdel" ,name)))))

(defun run-as-subuser (user command &key uid name environment
			    stdin-fd stdout-fd stderr-fd
			    pty wait slurp-stdout feed-stdin slay)
  (let*
    ((uid
       (cond
	 (uid (select-subuser user :uid uid :name name))
	 (name (or (select-subuser user :name name)
		   (subuser-uid user :name name)))
	 (t (subuser-uid user :name name))))
     (process
       (iolib/os:create-process
	 `(,*numeric-su-helper*
	    ,(format nil "~a" uid) "0"
	    ,@(add-command-env command environment))
	 :stdin (or stdin-fd (when feed-stdin :pipe) :null)
	 :stdout (or stdout-fd (when slurp-stdout :pipe) :null)
	 :stderr (or stderr-fd :null)
	 :pty pty :current-directory "/" :new-session pty))
     (stdin-feeder
       (bordeaux-threads:make-thread
	 (lambda ()
	   (when feed-stdin
	     (let*
	       ((stream (iolib/os:process-stdin process)))
	       (ignore-errors
		 (unwind-protect
		   (progn
		     (format stream "~a" feed-stdin)
		     (finish-output stream))
		   (close stream))))))
	 :name "Process stdin feeder"))
     (return-value
       (cond
	 (slurp-stdout
	   (iolib/os:process-status process :wait t)
	   (iolib/os::slurp-char-stream
	     (iolib/os:process-stdout process)))
	 (wait (iolib/os:process-status process :wait t))
	 (t process))))
    (if (or wait slurp-stdout)
      (progn
	(bordeaux-threads:join-thread stdin-feeder)
	(when slay (slay-subuser user :uid uid)))
      (bordeaux-threads:make-thread
	(lambda ()
	  (iolib/os:process-status process :wait t)
	  (bordeaux-threads:join-thread stdin-feeder)
	  (when slay (slay-subuser user :uid uid)))
	:name "Process reaper"))
    return-value))
