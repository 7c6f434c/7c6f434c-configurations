(defpackage :lisp-os-helpers/user-abbreviations
  (:use
    :common-lisp
    :lisp-os-helpers/socket-command-client
    :lisp-os-helpers/shell
    )
  (:export
    ))
(in-package :lisp-os-helpers/user-abbreviations)

(eval-when
  (:compile-toplevel :load-toplevel :execute)
  (defun underscore-to-capitals (s &key invert)
    (apply
      'concatenate 'string
      (loop
	for c across s
	for pending-this := nil then pending-next
	for pending-next := nil then (and (not pending-this) (equal c #\_))
	collect (if (equal invert pending-this)
		  (string-downcase c) (string-upcase c)))))
  (defun to-string (e)
    (etypecase e
      (string e)
      (null "")
      (keyword (underscore-to-capitals (symbol-name e) :invert t))
      (symbol (underscore-to-capitals (symbol-name e) :invert nil))
      (pathname (namestring e))
      (number (format nil "~a" e))))
  (defun split-command (l)
    (let*
      ((command nil) (arguments nil))
      (loop
	for e in l
	for lt on l
	for parameter := nil then parameter-next
	for parameter-next := (and (not parameter) (keywordp e))
	do
	(if parameter
	  (cond
	    ((and (symbolp e) (equal #\/ (elt (symbol-name e) 0)))
	     (push (underscore-to-capitals (symbol-name e)) arguments))
	    (t (push e arguments)))
	  (etypecase e
	    (keyword (case e
		       ((:>) (push :output arguments))
		       ((:2>) (push :error-output arguments))
		       ((:<) (push :input arguments))
		       ((:&>) 
			(push :output arguments)
			(push (second lt) arguments)
			(push ::error-output arguments))
		       (t (push e arguments))))
	    ((or string symbol number pathname null)
	     (push (to-string e) command))
	    (list
	      (cond
		((eq :splice (first e))
		 (multiple-value-bind
		   (scommand sarguments)
		   (split-command (rest e))
		   (setf command (append (reverse scommand) command)
			 arguments (apply (reverse sarguments) arguments))))
		((keywordp (first e))
		 (multiple-value-bind
		   (scommand sarguments)
		   (split-command e)
		   (setf command (append (reverse scommand) command)
			 arguments (apply (reverse sarguments) arguments))))
		(t (push e arguments)))))))
      (values (reverse command) (reverse arguments)))))

(defmacro ! (&rest data)
  (multiple-value-bind
    (command arguments) (split-command data)
    `(uiop:run-program (list ,@command) :ignore-error-status t ,@arguments)))

(defmacro & (&rest data)
  (multiple-value-bind
    (command arguments) (split-command data)
    `(uiop:launch-program (list ,@command) ,@arguments)))

(defmacro !! (&rest data)
  `(! screen ,@data))

(defmacro
  >>-impl (bangs &key stream)
  (cond
    ((null bangs))
    ((not (listp (first bangs)))
     `(>>-impl ,(cdr bangs) :stream ,(first bangs)))
    ((= (length bangs) 1)
     `(! :< ,stream ,@(first bangs)))
    (t
      (let ((first-process (gensym)))
	`(let
	   ((,first-process
	      (& :< ,stream :> :stream
		 ,@(first bangs))))
	   (>>-impl
	     ,(cdr bangs) :stream
	     (uiop:process-info-output ,first-process)))))))

(defmacro >> (&rest bangs)
  `(>>-impl ,bangs :stream t))

(defmacro $ (&rest args)
  (cond
    ((= 1 (length args)) `(uiop:getenv ,(to-string (first args))))
    ((and (symbolp (second args)) 
	  (equal ">>" (symbol-name (second args))) 
	  (null (first args)))
     (let
       ((stream (gensym)))
       `(cl-ppcre:split 
	  *line-break-regexpr*
	  (with-output-to-string (,stream)
	    (>> ,(cddr args) :stream ,stream)))))
    ((and (second args) (null (first args)))
     (let
       ((stream (gensym)))
       `(cl-ppcre:split 
	  *line-break-regexpr*
	  (with-output-to-string (,stream)
	    (! :> ,stream ,@(cdr args))))))))

(defun $= (name value)
  (setf (uiop:getenv (to-string name)) (to-string value)))

(defun editor (&optional filename)
  (!! (or ($ :visual) ($ :editor)) filename))

(defun cd (&optional dir)
  (uiop:chdir (or dir ($ "HOME")))
  (setf *default-pathname-defaults* (pathname (uiop:getcwd))))

(defun ls (&optional name &rest args)
  (mapcar
    'to-string
    (apply
      'uiop:directory*
      (to-string (or name (uiop:getcwd))) args)))

(defun ls* (&optional name)
  (list
    (mapcar 'to-string
            (uiop:directory-files (to-string (or name (uiop:getcwd)))))
    (mapcar 'to-string
            (uiop:subdirectories (to-string (or name (uiop:getcwd)))))))

(defun send-fd-over-unix-socket (path fd)
  (let*
    ((socket (iolib:make-socket
               :connect :active :address-family :local :type :datagram
               :remote-filename path)))
    (unwind-protect
      (iolib/sockets:send-file-descriptor socket fd)
      (close socket))))

(defmacro && (&rest args)
  `(bordeaux-threads:make-thread
     (lambda () ,@args)
     :name "Background worker thread"))

(defun ps ()
  (let*
    ((raw ($ () "ps" "-A" "-ww" "-o" "pid:8,uid:8,etimes:12,user:40,cmd" "--no-headers"))
     (split (loop for s in raw collect (cl-ppcre:split " +" s :limit 6)))
     (marked (loop for l in split collect 
                   (loop 
                     for e in (rest l)
                     for k in `(:pid :uid :elapsed :user :command)
                     collect k collect e))))
    marked))

(defmacro periodically (period &body body)
  `(loop
    do (format t "Results:~%~{~s~%~}" (list ,@body))
    do (format t "~s~%~%" (local-time:now))
    do (sleep ,period)))
