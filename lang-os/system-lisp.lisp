(load (format nil "~a/lib/common-lisp/server-helpers/server-helpers--all-systems.fasl" *server-helpers-package*))

(require :server-helpers)

(format t "Starting the Common Lisp system daemon at ~a~%" (local-time:now))
