(defpackage :nix
  (:use :common-lisp :shell)
  (:export
    #:nix-expression
    #:nix-instantiate
    #:nix-eval
    #:nix-realise
    #:nix-build
    ))
(in-package :nix)

(defun nix-expression
  (package-name &key nix-file 
                nixpkgs-suffix source-expression)
  (let*
    ((expression
       (if (listp package-name)
         (format nil "[ ~{(~a) ~}]" package-name)
         package-name))
     (source-expression
       (or
         source-expression
         (and nixpkgs-suffix (format nil "with import <nixpkgs~a> {}; " nixpkgs-suffix))
         (and nix-file
              (let*
                ((marker (expt 36 20)))
                (format
                  nil
                  "let
                      imported_~a = import ~s; 
                      true_~a = if (builtins,isFunction imported_~a) then 
                                   imported_~a {} else imported_~a; 
                   in with true_~a; "
                   marker nix-file
                   marker     marker
                   marker marker
                   marker)))
         "with import <nixpkgs> {}; "))
     (full-expression (format nil "~a~a" expression source-expression)))
    full-expression))

(defun nix-instantiate-raw
  (name &key nix-file 
        nix-path nix-path-prefix nix-path-suffix
        nixpkgs-suffix source-expression
        nix-args extra-env
        )
  (let*
    ((nix-path-env (cl-ppcre:split ":" (uiop:getenv "NIX_PATH")))
     (nix-new-path
       (or nix-path 
           (and
             (or nix-path-prefix nix-path-suffix)
             (append nix-path-prefix nix-path-env nix-path-suffix))))
     (expression
       (nix-expression
         name
         :nix-file nix-file
         :nixpkgs-suffix nixpkgs-suffix
         :source-expression source-expression))
     (nix-command
       `("nix-instantiate" "-E" ,expression ,@nix-args))
     (command
       (if nix-new-path
         (add-command-env
           nix-command `(("NIX_PATH" ,nix-new-path)))
         nix-command))
     (command (if extra-env (add-command-env command extra-env)))
     )
    (uiop:run-program command :output '(:string :stripped t)
                      :error-output t)))

(defun nix-instantiate (&rest args)
  (cl-ppcre:split *line-break-regexpr* (apply 'nix-instantiate-raw args)))

(defun nix-eval (expr &rest args &key nix-args)
  (apply
    'nix-instantiate-raw
    expr
    :nix-args (append nix-args (list "--eval-only"))
    args))

(defun nix-realise (derivations &key nix-args extra-env out-link)
  (let*
    ((command `("nix-store" "-r"
                ,@(if out-link
                    `("--add-root" ,out-link "--indirect"))
                ,@(if (stringp derivations)
                    (list derivations) derivations) ,@nix-args))
     (command (add-command-env command extra-env)))
    (uiop:run-program command :output `(:string :stripped t))))

(defun nix-build (name &rest args &key out-link)
  out-link
  (let*
    ((derivations
       (apply 'nix-instantiate name :allow-other-keys t args)))
    (apply 'nix-realise derivations :allow-other-keys t args)))
