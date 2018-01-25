(defpackage :timestamp
  (:use :common-lisp)
  (:export
    #:timestamp
    #:timestamp-usec
    #:timestamp-nsec
    #:timestamp-nsec-base36
    #:timestamp-base63
    ))
(in-package :timestamp)

(defun timestamp ()
  (local-time:format-timestring
    nil (local-time:now)
    :format `((:year 4) (:month 2) (:day 2) #\- (:hour 2) (:min 2) (:sec 2))))

(defun timestamp-usec ()
  (local-time:format-timestring
    nil (local-time:now)
    :format `((:year 4) (:month 2) (:day 2) #\- (:hour 2) (:min 2) (:sec 2) #\. (:usec 6))))

(defun timestamp-nsec ()
  (local-time:format-timestring
    nil (local-time:now)
    :format `((:year 4) (:month 2) (:day 2) #\- (:hour 2) (:min 2) (:sec 2) #\. (:nsec 9))))

(defun timestamp-nsec-base36 ()
  (let*
    (
     (tnow (local-time:now))
     (secs (local-time:timestamp-to-unix tnow))
     (nsecs (local-time:nsec-of tnow))
     (n (+ nsecs (* secs (expt 10 9))))
     )
    (format nil "~36,13,'0r" (truncate n))))

(defun format-number-with (n alphabet &optional (l 1))
  (map
    'string (lambda (x) (elt alphabet x))
    (reverse
      (loop
        with k := (length alphabet)
        for nn := n then (truncate nn k)
        for p upfrom 0
        for d := (mod nn k)
        while (or (> nn 0) (< p l))
        collect d))))

(defparameter *base63-alphabet*
  "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_")

(defun timestamp-base63 ()
  (let*
    (
     (tnow (local-time:now))
     (secs (local-time:timestamp-to-unix tnow))
     (nsecs (local-time:nsec-of tnow))
     (n (+ nsecs (* secs (expt 10 9))))
     )
    (format-number-with n *base63-alphabet* 11)))
