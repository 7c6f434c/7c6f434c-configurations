(defpackage :lisp-os-helpers/marionette
  (:use
    :common-lisp
    :lisp-os-helpers/subuser-x
    )
  (:export
    #:subuser-firefox
    #:with-marionette
    #:ask-marionette
    #:ask-marionette-parenscript
    #:marionette-set-pref
    ))
(in-package :lisp-os-helpers/marionette)

(defvar *ambient-marionette-socket* nil)

(defmacro with-marionette ((socket-path 
			     &key window (context :content)
			     (socket-name '*ambient-marionette-socket*))
			   &rest code)
  `(let*
     ((,socket-name
	(iolib:make-socket
	  :connect :active :address-family :local :type :stream
	  :remote-filename socket-path)))
     ,@(when window
	 `((format ,socket-name "session.switch_to_window('~a');~%" ,window)))
     ,@(when context
	 `((format ,socket-name "session.set_context(CONTEXT_~a);~%"
		   (string-upcase context))))
     ,@ code))

(defun skip-marionette-messages (&key (socket *ambient-marionette-socket*))
  (let*
    ((key (format nil "~36r" (random (expt 36 20)))))
    (format socket "print('~a');" key)
    (loop
      for line := (read-line socket)
      until (equal line key)
      collect line)))

(defun ask-marionette (code &key (socket *ambient-marionette-socket*))
  (skip-marionette-messages :socket socket)
  (format socket "~a~%" code)
  (finish-output socket)
  (skip-marionette-messages :socket socket))

(defun escape-for-python (str)
  (let*
    ((str str)
     (str (cl-ppcre:regex-replace-all "\\\\" str "\\\\\\\\"))
     (str (cl-ppcre:regex-replace-all "[\"']" str "\\\\\\1"))
     (str (cl-ppcre:regex-replace-all (string #\Return) str "\\\\r"))
     (str (cl-ppcre:regex-replace-all (string #\Newline) str "\\\\n"))
     (str (cl-ppcre:regex-replace-all (string #\Null) str "\\\\0"))
     )
    str))

(defun ask-marionette-parenscript (ps-code &key (socket *ambient-marionette-socket*))
  (let*
    ((js-code (ps:ps* ps-code))
     (js-code-escaped (escape-for-python js-code))
     (py-code
       (format nil "print(session.execute_script(\"~a\"));"
	       js-code-escaped)))
    (ask-marionette py-code :socket socket)))

(defun marionette-set-pref (key value &key (socket *ambient-marionette-socket*))
  (ask-marionette
    (format nil "session.set_pref('~a',~a)"
            (escape-for-python key)
            (firefox-pref-value-js value))))
