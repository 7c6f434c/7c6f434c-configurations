(defpackage :network
  (:use :common-lisp :shell)
  (:export
    #:parsed-ip-address-show
    #:add-ip-address
    #:remove-ip-address
    #:enable-ip-link
    #:disable-ip-link
    #:run-link-dhclient
    #:port-open-p
    ))
(in-package :network)

(defun attribute-labels-to-keywords (l)
  (loop
    for p := l then (cddr p)
    for k := (first p)
    for v := (second p)
    for ks := (intern (string-upcase k) :keyword)
    while v
    collect ks collect v))

(defun parse-ip-address-line (line)
  (cond
    ((equal (subseq line 0 7) (make-string 7 :initial-element #\Space))
     (attribute-labels-to-keywords
       (cl-ppcre:split " " (string-trim " " line))))
    ((equal (subseq line 0 4) (make-string 4 :initial-element #\Space))
     (let*
       ((components (cl-ppcre:split " " (string-trim " " line)))
        (address-type (first components))
        (address-line (second components))
        (address-components (cl-ppcre:split "/" address-line))
        (address (first address-components))
        (mask-length (second address-components)))
       (append
         (list
           :address-type address-type
           :address address
           :netmask-length mask-length)
         (attribute-labels-to-keywords (cddr components)))))
    (t
      (let*
        ((components (cl-ppcre:split " " (string-trim " " line)))
         (interface-index (parse-integer (string-trim ":" (first components))))
         (interface-name (string-trim ":" (second components)))
         (interface-attributes (third components))
         (interface-attribute-components
           (remove "" (cl-ppcre:split "[<>,]" interface-attributes)
                   :test 'equal)))
        (append
          (list
            :interface-index interface-index
            :interface-name interface-name
            )
          (loop
            for a in interface-attribute-components
            collect (intern (string-upcase
                              (cl-ppcre:regex-replace-all "_" a "-"))
                            :keyword)
            collect t)
          (attribute-labels-to-keywords
            (cdddr components)))))))

(defun parsed-ip-address-show (&optional interface)
  (let*
    ((lines 
       (program-output-lines
         `("ip" "address" "show" ,@(when interface `(,interface)))))
     (parsed-lines
       (mapcar 'parse-ip-address-line lines))
     (interfaces
       (loop
         with interface := nil
         with addresses := nil
         with address := nil
         for l in (append parsed-lines (list nil))
         when (or (null l) (getf l :interface-name) (getf l :address))
         do (progn
              (when address (push address addresses))
              (setf address nil))
         when (or (null l) (getf l :interface-name))
         do (progn
              (setf 
                interface
                (append 
                  interface 
                  (list 
                    :addresses (reverse addresses) 
                    :addresses-global-brief
                    (loop
                      for a in addresses
                      when (equal (getf a :scope) "global")
                      collect (getf a :address)))))
              (setf addresses nil))
         if (and (or (null l) (getf l :interface-name))
                 (getf interface :interface-name))
         collect interface
         else if (or (null l) (getf l :interface-name))
         do (progn)
         else if (getf l :address)
         do (setf address l)
         else
         do (setf address (append address l))
         when (getf l :interface-name) do (setf interface l)
         when (getf l :address) do (setf address l)
         )))
    interfaces))

(defun ip-address-info (interface address)
  (find-if
    (lambda (x) (equal (getf x :address) address))
    (getf (first (parsed-ip-address-show interface)) :addresses)))

(defun add-ip-address (interface address &optional (netmask-length 24))
  (uiop:run-program
    (list "ip" "address"
          "add" (format nil "~a/~a" address netmask-length)
          "dev" interface)))

(defun remove-ip-address (interface address)
  (uiop:run-program
    (list "ip" "address"
          "delete"
          (format
            nil "~a/~a"
            address
            (getf (ip-address-info interface address) :netmask-length))
          "dev" interface)))

(defun enable-ip-link (interface)
  (uiop:run-program
    (list "ip" "link" "set" interface "up")))

(defun disable-ip-link (interface)
  (uiop:run-program
    (list "ip" "link" "set" interface "down")))

(defun run-link-dhclient (interface)
  (run-program-return-success
    (uiop:run-program
      (list "dhclient" "-1" interface))))

(defun port-open-p (port &key (host "127.0.0.1"))
  (ignore-errors
    (iolib/sockets:make-socket
      :remote-host host :remote-port port)
    t))
