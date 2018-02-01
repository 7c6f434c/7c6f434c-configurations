(with-open-file (*error-output* "/dev/null" :direction :output :if-exists :overwrite)
  (with-open-file (*trace-output* "/dev/null" :direction :output :if-exists :overwrite)
    (load (format nil "~a/lib/common-lisp/lisp-os-helpers/lisp-os-helpers--all-systems.fasl" *lisp-os-helpers-package*))))

(use-package :lisp-os-helpers/user-abbreviations)
(use-package :lisp-os-helpers/socket-command-client)
