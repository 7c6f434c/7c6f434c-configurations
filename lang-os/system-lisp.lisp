(with-open-file (*error-output* "/dev/null" :direction :output :if-exists :overwrite)
  (with-open-file (*trace-output* "/dev/null" :direction :output :if-exists :overwrite)
    (load (format nil "~a/lib/common-lisp/server-helpers/server-helpers--all-systems.fasl" *server-helpers-package*))))

(use-package :shell)
(use-package :network)
(use-package :socket-command-server)
(use-package :subuser)
(use-package :daemon)
(use-package :unix-users)

(format t "Starting the Common Lisp system daemon at ~a~%" (local-time:now))

(loop
  for vtn from 2 to 6 do
  (let ((vtn vtn))
    (bordeaux-threads:make-thread
      (lambda ()
        (loop
          do (loop while (console-used vtn) do (sleep 10))
          do (ignore-errors
               (uiop:run-program
                 (list "/run/current-system/bin/spawn-getty"
                       (format nil "~a" vtn))))
          ))
      :name (format nil "Console spawner ~a" vtn))))

(defparameter
  *socket-main-thread*
  (bordeaux-threads:make-thread
    (lambda () (eval-socket-runner "/run/system-lisp-socket"))
    :name "System lisp daemon socket command evaluator"))

(unless
  (run-program-return-success
    (uiop:run-program
      (list "env" "NIX_REMOTE=daemon"
            "nix-store" "--check-validity" "/run/current-system/")))
  (system-service "daemon/nix-daemon" "nix-daemon"))

(ensure-daemon-user "postgresql")
(ensure-daemon-user "named")
(grant-to-user "named" "/var/lib/bind/")
(grant-to-group "named" "/var/lib/bind/")
(ensure-daemon-user "cups")
(ensure-daemon-group "lp")
(grant-to-user "cups" "/var/lib/cups/")
(grant-to-group "lp" "/var/lib/cups/")

(unless
  (port-open-p 22)
  (system-service "daemon/ssh" "from-nixos/openssh"))

(unless
  (run-program-return-success
    (uiop:run-program
      (list "pgrep" "-x" "udevd")))
  (system-service "daemon/udevd" "udevd"))

(unless
  (port-open-p 631)
  (system-service "daemon/cups" "from-nixos/cups"))

(unless
  (port-open-p 53)
  (system-service "daemon/bind" "from-nixos/bind"))

(unless
  (port-open-p 5432)
  (daemon-with-logging 
    "daemon/postgresql"
    (list "su" "postgres" "-s" "/bin/sh" "-c" "env -i /run/current-system/services/from-nixos/postgresql")))

(sleep 5)

(bordeaux-threads:join-thread *socket-main-thread*)
