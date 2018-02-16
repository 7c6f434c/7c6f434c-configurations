(defvar *socket-main-thread* nil)

(defparameter *socket-main-thread-preexisting* *socket-main-thread*)

(format t "~s~%Preexisting socket: ~s~%Helpers: ~s~%"
        "Initialising Common Lisp system daemon"
        *socket-main-thread-preexisting*
        *lisp-os-helpers-package*)

(unless
  (ignore-errors
    (with-open-file (*error-output* "/dev/null" :direction :output :if-exists :overwrite)
      (with-open-file (*trace-output* "/dev/null" :direction :output :if-exists :overwrite)
        (load (format nil "~a/lib/common-lisp/lisp-os-helpers/lisp-os-helpers--all-systems.fasl" *lisp-os-helpers-package*))))
    t)
  (load (format nil "~a/lib/common-lisp/lisp-os-helpers/lisp-os-helpers--all-systems.fasl" *lisp-os-helpers-package*)))

(use-package :lisp-os-helpers/shell)
(use-package :lisp-os-helpers/network)
(use-package :lisp-os-helpers/socket-command-server)
(use-package :lisp-os-helpers/subuser)
(use-package :lisp-os-helpers/daemon)
(use-package :lisp-os-helpers/unix-users)
(use-package :lisp-os-helpers/vt)
(use-package :lisp-os-helpers/socket-command-definitions)

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
  (system-service "" "nix-daemon"))

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

(defun socket-command-server-commands::run (context &rest command)
  (require-root context)
  (uiop:run-program command :output "/dev/tty62" :error-output "/dev/tty62"))
(defun socket-command-server-commands::run-pipe (context input &rest command)
  (require-root context)
  (with-input-from-string (s input)
    (uiop:run-program command :output :string :input s)))
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

(defun socket-command-server-commands::restart-nix-daemon (context)
  (require-presence context)
  (if (ignore-errors (kill-by-log "daemon/nix-daemon"))
    (progn
      (system-service "" "nix-daemon")
      "OK")
    (error "Stopping nix-daemon failed")))

(defun socket-command-server-commands::dhclient (context interface &optional copy-resolv)
  (assert
    (or
      (ignore-errors (require-root context) t)
      (gethash (list (context-uid context) :owner) *user-info*)))
  (require-presence context)
  (run-link-dhclient interface)
  (when copy-resolv
    (alexandria:write-string-into-file
      (alexandria:read-file-into-string
	"/etc/resolv.conf.dhclient")
      "/etc/resolv.conf")))

(defun socket-command-server-commands::add-ip-address (context interface address &optional (netmask-length 24))
  (assert
    (or
      (ignore-errors (require-root context) t)
      (gethash (list (context-uid context) :owner) *user-info*)))
  (require-presence context)
  (add-ip-address interface address netmask-length))

(defun socket-command-server-commands::rebuild-from-path
  (context path &optional nix-path)
  (require-presence context)
  (require-root context)
  (assert
    (or
      (ignore-errors (require-root context) t)
      (gethash (list (context-uid context) :owner) *user-info*)))
  (uiop:run-program
    `("/run/current-system/bin/update-self-from-expression"
      ,path
      ,@(when nix-path `("-I" ,nix-path)))))

(defun start-x-allowed-p (context display)
  (require-presence context) t)

(defun grab-device-allowed-p (user subuser device)
  (and
    (gethash (list user :owner) *user-info*)
    (or
      (alexandria:starts-with-subseq "/dev/snd/" device)
      (alexandria:starts-with-subseq "/dev/dri/" device)
      (alexandria:starts-with-subseq "/dev/fb" device)
      (alexandria:starts-with-subseq "/dev/video" device)
      (equal "/dev/fuse" device)
      (equal "/dev/kvm" device)
      )))

(defun nsjail-mount-allowed-p (from to type)
  (or
    (equal type "T")
    (equal from to)
    (and
      (or
        (alexandria:starts-with-subseq "/home/" target)
        (alexandria:starts-with-subseq "/tmp/" target)
        )
      (or
        (alexandria:starts-with-subseq "/home/" internal-target)
        (alexandria:starts-with-subseq "/tmp/" internal-target)
        ))
    ))

(unless
  *socket-main-thread-preexisting*
  (format
    t "Finished Common Lisp daemon initialisation at ~a.~%Socket thread: ~s~%"
    (local-time:now) *socket-main-thread*)
  (bordeaux-threads:join-thread *socket-main-thread*)
  (sleep 5))
