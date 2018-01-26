(with-open-file (*error-output* "/dev/null" :direction :output :if-exists :overwrite)
  (with-open-file (*trace-output* "/dev/null" :direction :output :if-exists :overwrite)
    (load (format nil "~a/lib/common-lisp/server-helpers/server-helpers--all-systems.fasl" *server-helpers-package*))))

(use-package :shell)
(use-package :socket-command-server)
(use-package :subuser)
(use-package :daemon)

(format t "Starting the Common Lisp system daemon at ~a~%" (local-time:now))

(bordeaux-threads:make-thread
  (lambda () (eval-socket-runner "/run/system-lisp-socket"))
  :name "System lisp daemon socket command evaluator")

(loop
  for vtn from 2 to 6 do
  (let ((vtn vtn))
    (bordeaux-threads:make-thread
      (lambda ()
        (loop
          do (loop while (console-used vtn) (sleep 10))
          do (ignore-errors (uiop:run-program (list "/run/current-system/bin/spawn-getty" (format nil "~a"))))
          ))
      :name (format nil "Console spawner ~a" vtn))))

(sleep 5)
