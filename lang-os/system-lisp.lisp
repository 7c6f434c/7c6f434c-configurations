(with-open-file (*error-output* "/dev/null" :direction :output :if-exists :overwrite)
  (with-open-file (*trace-output* "/dev/null" :direction :output :if-exists :overwrite)
    (load (format nil "~a/lib/common-lisp/server-helpers/server-helpers--all-systems.fasl" *server-helpers-package*))))

(format t "Starting the Common Lisp system daemon at ~a~%" (local-time:now))

(sleep 5)
