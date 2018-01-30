(defpackage :lisp-os-helpers/auth-data
  (:use :common-lisp)
  (:export
    #:check-password
    #:*pty-helper*
    #:*auth-challenge-directory*
    #:create-file-challenge
    #:verify-file-challenge
    ))
(in-package :lisp-os-helpers/auth-data)

(defvar *pty-helper* "/run/current-system/sw/bin/in-pty")

(defun check-password (user password &key (pty-helper *pty-helper*))
  (let*
    ((uid (iolib/syscalls:getuid))
     (command 
       (if (= uid 0)
         (list
           pty-helper "su" "nobody" "-s" "/bin/sh" "-c"
           (format nil "su ~a -s /bin/sh -c true" user))
         (list pty-helper "su" user "-s" "/bin/sh" "-c" "true")))
     (su-process (uiop:launch-program command :input :stream :output :stream))
     (su-in (uiop:process-info-input su-process))
     (su-out (uiop:process-info-output su-process)))
    (read-char su-out)
    (format su-in "~a~%" password)
    (close su-in)
    (prog1
      (= 0 (uiop:wait-process su-process))
      (close su-out))))

(defvar *auth-challenge-directory* "/run/auth-challenges")

(defun create-file-challenge (&key users groups (auth-challenge-directory *auth-challenge-directory*))
  (uiop:with-temporary-file
    (:pathname path :stream f :keep t :directory auth-challenge-directory)
    (uiop:run-program (list "chmod" "0" (namestring path)))
    (format f "~36r~%" (random (expt 36 20)))
    (force-output f)
    (loop
      for user in users
      do (uiop:run-program
           (list "setfacl" "-m"
                 (format nil "u:~a:r" user)
                 (namestring path))))
    (loop
      for group in groups
      do (uiop:run-program
           (list "setfacl" "-m"
                 (format nil "g:~a:r" group)
                 (namestring path))))
    (namestring path)))

(defun verify-file-challenge (path answer)
  (uiop:run-program (list "chmod" "u=r" path))
  (with-open-file (f path)
    (prog1
      (equal (read-line f nil nil) answer)
      (uiop:run-program (list "rm" path)))))
