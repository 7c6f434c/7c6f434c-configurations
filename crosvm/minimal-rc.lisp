(progn (require :sb-posix) (require :sb-bsd-sockets))

(with-open-file (*error-output* "/dev/null" :direction :output :if-exists :overwrite)
        (with-open-file (*trace-output* "/dev/null" :direction :output :if-exists :overwrite)
         (load (format nil "~a/lib/common-lisp/lisp-os-helpers/lisp-os-helpers--all-systems.fasl"
         *lisp-os-helpers-package*))))

(use-package :lisp-os-helpers/user-abbreviations)
(use-package :lisp-os-helpers/socket-command-client)
(use-package :lisp-os-helpers/subuser-x)
(use-package :lisp-os-helpers/shell)
(use-package :lisp-os-helpers/timestamp)

(setf *random-state* (make-random-state t))
