(defpackage :subuser
  (:use :common-lisp :shell :global-sqlite :timestamp)
  (:export
    #:subuser-uid
    #:drop-subuser
    #:slay-subuser
    #:run-as-subuser
    ))
(in-package :subuser)

(defvar *subuser-uid-shift* 100000)
(defvar *nogroup* 65534)
(defvar *numeric-su-helper* "/run/current-system/sw/bin/numeric-su")

(defun subuser-uid (user &key name home gid group passwd-entry)
  (with-global-sqlite
    ()
    (let*
      ((name (or name (format nil "~36r" (expt 36 20))))
       (home (or home "/var/empty"))
       (gid (or
              gid 
              (and
                group
                (third (multiple-value-list (iolib/syscalls:getgrnam group))))
              *nogroup*))
       (uid
         (progn
           (complex-global-value 
             :subusers name
             `(:owner :home :gid) `(:varchar :varchar :integer)
             (list home user gid) t)
           (+ *subuser-uid-shift*
              (first (complex-global-value :subuser name `(:id) nil)))
           ))
       )
      (when passwd-entry
        (unless
          (iolib/syscalls:getpwuid uid)
          (uiop:run-program
            `("/run/current-system/bin/system-useradd"
              "-d" ,home "-u" ,(format nil "~a" uid)
              "-g" ,(format nil "~a" gid) ,name))))
      (values uid name))))

(defun drop-subuser (user &key uid name)
  (assert (or uid name))
  (let*
    ((name (or name (iolib/syscalls:getpwuid uid)))
     (exists (iolib/syscalls:getpwnam name))
     (owner (first (with-global-sqlite 
                     ()
                     (complex-global-value :subusers name `(:owner) nil)))))
    (when exists
      (assert (equal owner user))
      (uiop:run-program
        `("/run/current-system/bin/system-userdel" ,name)))))

(defun slay-subuser (user &key uid name)
  (with-global-sqlite
    ()
    (assert (or uid name))
    (let*
      ((name (or name (first (complex-global-value-by-id 
                               :subusers (- uid *subuser-uid-shift*) :key))))
       (uid (or uid (+ *subuser-uid-shift*
                       (first (complex-global-value
                                :subusers name `(:id) nil)))))
       (owner (first (complex-global-value :subusers name `(:owner) nil))))
      (assert (equal user owner))
      (uiop:run-program `("pkill" "-u" ,(format nil "~a" uid) "-KILL")))))

(defun run-as-subuser (user uid command &key environment
                            stdin-fd stdout-fd stderr-fd
                            pty wait slurp-stdout)
  (assert
    (equal
      user 
      (first
        (with-global-sqlite
          ()
          (complex-global-value-by-id
            :subusers (- uid *subuser-uid-shift*) `(:owner))))))
  (let*
    ((process
       (iolib/os:create-process
         `(,*numeric-su-helper*
            ,(format nil "~a" uid) "0"
            ,@(add-command-env command environment))
         :stdin (or stdin-fd :null)
         :stdout (or stdout-fd (when slurp-stdout :pipe) :null)
         :stderr (or stderr-fd :null)
         :pty pty :current-directory "/" :new-session pty)))
  (if (or wait slurp-stdout) (iolib/os:process-status process)
    (bordeaux-threads:make-thread
      (lambda () (iolib/os:process-status process :wait t))
      :name "Process reaper"))
  (when slurp-stdout
    (iolib/os::slurp-char-stream (iolib/os:process-stdout process)))))
