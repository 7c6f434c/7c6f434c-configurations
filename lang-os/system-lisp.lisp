(with-open-file (*error-output* "/dev/null" :direction :output :if-exists :overwrite)
  (with-open-file (*trace-output* "/dev/null" :direction :output :if-exists :overwrite)
    (load (format nil "~a/lib/common-lisp/lisp-os-helpers/lisp-os-helpers--all-systems.fasl" *lisp-os-helpers-package*))))

(use-package :lisp-os-helpers/shell)
(use-package :lisp-os-helpers/network)
(use-package :lisp-os-helpers/socket-command-server)
(use-package :lisp-os-helpers/subuser)
(use-package :lisp-os-helpers/daemon)
(use-package :lisp-os-helpers/unix-users)

(defvar *socket-main-thread* nil)

(defparameter *socket-main-thread-preexisting* *socket-main-thread*)

(unless *socket-main-thread-preexisting*
  (format t "Starting the Common Lisp system daemon at ~a~%" (local-time:now)))

(defvar *vt-spawners* (make-array 12 :initial-element nil))

(loop
  for vtn from 2 to 6 do
  (let ((vtn vtn))
    (unless (elt *vt-spawners* vtn)
      (setf 
	(elt *vt-spawners* vtn)
	(bordeaux-threads:make-thread
	  (lambda ()
	    (loop
	      do (loop while (console-used vtn) do (sleep 10))
	      do (ignore-errors
		   (uiop:run-program
		     (list
		       "/run/current-system/bin/spawn-getty"
		       (format nil "~a" vtn))))
	      ))
	  :name (format nil "Console spawner ~a" vtn))))))

(defparameter
  *socket-main-thread*
  (or *socket-main-thread*
      (bordeaux-threads:make-thread
	(lambda () (eval-socket-runner *system-lisp-socket*))
	:name "System lisp daemon socket command evaluator")))

(unless
  (run-program-return-success
    (uiop:run-program
      (list "env" "NIX_REMOTE=daemon"
            "nix-store" "--check-validity" "/run/current-system/")))
  (system-service "daemon/nix-daemon" "nix-daemon"))

(ensure-daemon-user "postgres")
(ensure-daemon-user "named")
(grant-to-user "named" "/var/lib/bind/")
(grant-to-group "named" "/var/lib/bind/")
(ensure-daemon-user "cups")
(ensure-daemon-group "lp")
(grant-to-user "cups" "/var/lib/cups/")
(grant-to-group "lp" "/var/lib/cups/")

(unless
  (port-open-p 22)
  (system-service "" "from-nixos/openssh"))

(unless
  (run-program-return-success
    (uiop:run-program
      (list "pgrep" "-x" "udevd")))
  (system-service "" "udevd"))

(unless
  (port-open-p 631)
  (system-service "" "from-nixos/cups"))

(unless
  (port-open-p 53)
  (system-service "" "from-nixos/bind"))

(unless
  (port-open-p 5432)
  (daemon-with-logging 
    "daemon/postgresql"
    (list "su" "postgres" "-s" "/bin/sh" "-c"
	  "env -i /run/current-system/services/from-nixos/postgresql")))

(defun socket-command-server-commands::load (context path)
  (require-root context)
  (load path) "OK")
(defun socket-command-server-commands::eval (context code)
  (require-root context)
  (eval (read-from-string code)))
(defun socket-command-server-commands::run (context &rest command)
  (require-root context)
  (uiop:run-program command))
(defun socket-command-server-commands::quit (context)
  (require-presence context)
  (quit))

(defun socket-command-server-commands::restart-udevd (context)
  (require-presence context)
  (if
    (ignore-errors (uiop:run-program (list "udevadm" "control" "--exit")))
    (progn
      (system-service "" "udevd")
      "OK")
    (error "Stopping udevd failed")
    ))

(defun socket-command-server-commands::restart-openssh (context)
  (require-presence context)
  (if
    (ignore-errors (kill-by-executable "sshd"))
    (progn
      (system-service "" "from-nixos/openssh")
      "OK")
    (error "Stopping openssh failed")))

(defun socket-command-server-commands::restart-cups (context)
  (require-presence context)
  (if
    (ignore-errors (kill-by-log "daemon/cups"))
    (progn
      (system-service "" "from-nixos/cups")
      "OK")
    (error "Stopping cupsd failed")))

(defun socket-command-server-commands::restart-bind (context)
  (require-presence context)
  (if (ignore-errors (kill-by-log "daemon/bind"))
    (progn
      (system-service "" "from-nixos/bind")
      "OK")
    (error "Stopping bind failed")))

(defun socket-command-server-commands::restart-postgresql (context)
  (require-presence context)
  (if (ignore-errors (kill-by-log "daemon/postgresql"))
    (progn
      (daemon-with-logging 
	"daemon/postgresql" 
	(list
	  "su" "postgres" "-s" "/bin/sh" "-c"
	  "env -i /run/current-system/services/from-nixos/postgresql"))
      "OK")
    (error "Stopping postgresql failed")))

(defun socket-command-server-commands::start-x (context &optional (display 0))
  (require-presence context)
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
    (format nil "~a" display)))

(unless
  *socket-main-thread-preexisting*
  (sleep 5)
  (format
    t "Finished Common Lisp daemon initialisation at ~a.~%Socket thread: ~s~%"
    (local-time:now) *socket-main-thread*)
  (bordeaux-threads:join-thread *socket-main-thread*))
