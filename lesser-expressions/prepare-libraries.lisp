               #+cffi(setf cffi:*foreign-library-directories*
                           (remove
                             ""
                             (uiop:split-string (uiop:getenv "LD_LIBRARY_PATH") 
                                                :separator ":")
                             :test 'equalp))
               #+sbcl(loop
                       with libpath := (uiop:split-string (uiop:getenv "LD_LIBRARY_PATH")
                                :separator ":")
                       for l in sb-alien::*shared-objects*
                       for ns := (sb-alien::shared-object-namestring l)
                       do (format *error-output* "Searching alien object ~s in ~s~%"
                               ns libpath)
                       do (and (> (length ns) 0) (not (equal (elt ns 0) "/"))
                               (let*
                                 ((prefix (find-if (lambda (s) (probe-file (format nil "~a/~a" s ns))) libpath))
                                  (fullpath (and prefix (format nil "~a/~a" prefix ns))))
                                  (when fullpath
                                     (format *error-output* "Found: ~s~%" fullpath)
                                     (setf
                                       (sb-alien::shared-object-namestring l) fullpath
                                       (sb-alien::shared-object-pathname l) (probe-file fullpath)))))
                   )

