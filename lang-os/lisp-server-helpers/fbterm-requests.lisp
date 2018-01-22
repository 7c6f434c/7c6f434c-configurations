(defpackage :fbterm-requests
  (:use :common-lisp :vt :file-locks :shell)
  (:export
    #:*fb-device*
    #:*fbterm-settings*
    #:fbterm-request
    ))

(in-package :fbterm-requests)

(defvar *fb-device* "/dev/fb0")

(defvar *fbterm-settings*
  `((:font-name ,"DejaVu Sans Mono")
    (:font-size ,33)))

(defun fbterm-request
  (prompt &key (vtn 63)
          (file-lock-helper *file-lock-helper*)
          (vt-lock-helper *vt-lock-helper*)
          (vt-lock-directory *vt-lock-directory*)
          (fb-device *fb-device*)
          (hide-entry nil)
          (fbterm-settings *fbterm-settings*)
          (timeout 15)
          )
  (uiop:with-temporary-file
    (:pathname value-path)
    (uiop:with-temporary-file
      (:pathname failure-path)
      (uiop:run-program
        (list "chmod" "u=rw,og="
              (namestring value-path) (namestring failure-path)))
      (at-locked-vt
        (vtn
          :vt-lock-helper vt-lock-helper
          :vt-lock-directory vt-lock-directory
          :file-lock-helper file-lock-helper)
        (let*
          ((vtdev (format nil "/dev/tty~d" vtn))
           (run-success
             (run-program-return-success
               (uiop:run-program
                 `("env" ,(format nil "FRAMEBUFFER=~a" fb-device)
                   "fbterm"
                   ,@(loop for s in fbterm-settings 
                           collect (format nil "--~a" (string-downcase (first s)))
                           collect (format nil "~a" (second s)))
                   "--"
                   "sh" "-c"
                   ,(format nil 
                            "sayintr() { echo interrupt > ~a ; } ; trap sayintr 2;
                            echo ~a; read -t ~a ~a x || echo fail > ~a;
                            echo \"$x\" > ~a"
                            (escape-for-shell (namestring failure-path))
                            (escape-for-shell prompt)
                            timeout
                            (if hide-entry "-s" "")
                            (escape-for-shell (namestring failure-path))
                            (escape-for-shell (namestring value-path))))
                   :input vtdev :output vtdev :ignore-error-status t))))
           (unless
             (or
               (not run-success)
               (> (length (alexandria:read-file-into-string failure-path)) 2))
             (string-right-trim
               `(#\Newline #\Return)
               (alexandria:read-file-into-string value-path))))))))
