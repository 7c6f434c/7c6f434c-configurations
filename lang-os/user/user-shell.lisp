(defun load-helpers ()
  (with-open-file (*error-output* "/dev/null" :direction :output :if-exists :overwrite)
    (with-open-file (*trace-output* "/dev/null" :direction :output :if-exists :overwrite)
      (load (format nil "~a/lib/common-lisp/lisp-os-helpers/lisp-os-helpers--all-systems.fasl" *lisp-os-helpers-package*)))))

(unless
  (find-package :lisp-os-helpers/shell)
  (load-helpers))

(defvar *rc-path*
 (or
  (uiop:getenv "USER_LISP_SHELL_RC")
  *compile-file-pathname*
  *load-pathname*))

(use-package :lisp-os-helpers/user-abbreviations)
(use-package :lisp-os-helpers/socket-command-client)
(use-package :lisp-os-helpers/shell)
(use-package :lisp-os-helpers/nix)

(defpackage :sudo (:use))

(defun loadrc () (load *rc-path*))
(defun edrc () (editor *rc-path*))

(defmacro defun-export (name args &rest code)
  `(progn
     (defun ,name ,args ,@code)
     (export ',name (find-package ,(package-name (symbol-package name))))))

(defmacro defmacro-export (name args &rest code)
  `(progn
     (defmacro ,name ,args ,@code)
     (export ',name (find-package ,(package-name (symbol-package name))))))

(defun-export
  sudo::start-x (&optional (display 0) command)
  (with-system-socket 
    ()
    (ask-server
      (with-uid-auth 
	(with-presence-auth
	  "X11 session"
	  `(start-x ,display))))
    (when command
      (uiop:run-program
	(add-command-env
	  command `((:display ,(format nil ":~a" display))))))))

(defun-export
  sudo::run-as-subuser (name command environment &rest options)
  (with-system-socket
    ()
    (ask-server
      (with-uid-auth
	`(run-as-subuser
	   ,name ,command ,environment ,options)))))

(defun-export
  sudo::restart-system-lisp ()
  (with-system-socket
    ()
    (ignore-errors
      (ask-server
	(with-uid-auth
	  (with-presence-auth
	    "Load fresh code into system lisp"
	    `(quit)))))))

(defun-export
 sudo::system-rebuild (path &optional (nix-path (uiop:getenv "NIX_PATH")))
 (format
  t "System: ~s~%"
  (nix-build "systemInstance"
   :nix-file path
   :nix-path (if (stringp nix-path)
	   (cl-ppcre:split ":" nix-path)
	   nix-path)))
 (with-system-socket
  ()
  (ask-server
   (with-password-auth
    "Rebuild the system"
    `(rebuild-from-path ,path ,nix-path)
    :user "root"))))

(defun-export
  sudo::run (&rest command)
  (with-system-socket
    ()
    (ask-server
      (with-password-auth
	"Arbitrary command execution is requested"
	`(run ,@ command)
	:user "root"))))

(defmacro
  !su (&rest data)
  `(apply 'sudo:run
     ,(multiple-value-bind
	(command arguments environment)
	(lisp-os-helpers/user-abbreviations::split-command data)
	(assert (null arguments))
	`(add-command-env
	   (list ,@command) (list ,@environment)))))

(defun-export
  sudo::chvt (vtn)
  (with-system-socket
    ()
    (ask-server
      (with-presence-auth
	"Change VT"
	`(chvt ,vtn)))))

#+sbcl(push 'editor sb-ext:*ed-functions*)