#! /bin/sh

"$(dirname "$0")"/run-user-shell.sh --eval "
(progn
               (setf cffi:*foreign-library-directories*
                        (cffi::explode-path-environment-variable \"NIX_LISP_LD_LIBRARY_PATH\"))
               (loop
                       with libpath := (uiop:split-string (uiop:getenv \"NIX_LISP_LD_LIBRARY_PATH\")
                                :separator \":\")
                       for l in sb-alien::*shared-objects*
                       for ns := (sb-alien::shared-object-namestring l)
                       do (and (> (length ns) 0) (not (equal (elt ns 0) \"/\"))
                               (let*
                                 ((prefix (find-if (lambda (s) (probe-file (format nil \"~a/~a\" s ns))) libpath))
                                  (fullpath (and prefix (format nil \"~a/~a\" prefix ns))))
                                  (when fullpath
                                     (setf
                                       (sb-alien::shared-object-namestring l) fullpath
                                       (sb-alien::shared-object-pathname l) (probe-file fullpath)))))
                   )
		   )
" --eval '(sb-ext:save-lisp-and-die "'"$1"'" :executable t)'
