{ pkgs ? import <nixpkgs> {}, ...}: pkgs.lib.makeExtensible ( self: with self; {
  mccme-helpers-bis = pkgs.sbcl.buildASDFSystem rec {
    pname = "mccme-helpers";
    version = "0.0-unstable";
    description = "Helper code accumulated in MCCME Common Lisp projects";
    src = /home/raskin/src/lsp/mccme-helpers;
    lispLibs = with pkgs.sbcl.pkgs; [
      cl-utilities iterate clsql yason cl-ppcre parse-number cl-emb hunchentoot
      ironclad local-time fare-csv trivial-utf-8
    ];
  };
  cl-mailer-bis = pkgs.sbcl.buildASDFSystem {
    pname = "cl-mailer";
    version = "0.0-unstable";
    description = "My mail processing utilties in Common Lisp";
    src = /home/raskin/src/lsp/venv-cl-fuse/src/cl-mailer;
    lispLibs = with pkgs.sbcl.pkgs; [
      mccme-helpers-bis
      cl-smtp trivial-utf-8 local-time cl-ppcre iterate cl-qprint cl-base64
      esrap-peg unix-options net-telent-date
    ];
  };
  cl-mailer-bis-bin = pkgs.runCommandNoCC "cl-mailer" {} ''
      mkdir -p "$out/bin"
      ${
        pkgs.sbcl.withPackages (p: [cl-mailer-bis])
      }/bin/sbcl \
      --eval '(require :asdf)' \
      --eval '(require :cl-mailer)' \
      --load '${./prepare-libraries.lisp}' \
      --eval '(save-lisp-and-die 
                "'"$out/bin/cl-mailer-launcher"'" 
                :executable t)'
      "$out/bin/cl-mailer-launcher" \
      --eval '(save-lisp-and-die 
                "'"$out/bin/cl-mailer-send-email"'" 
                :executable t :toplevel 
                (lambda () 
           (destructuring-bind
            (f) (unix-options::cli-options)
            (format t "~a~%"
            (cl-mailer::sendmail f))))
            )'
      "$out/bin/cl-mailer-launcher" \
      --eval '(save-lisp-and-die 
                "'"$out/bin/cl-mailer-tabulate-mail"'" 
                :executable t :toplevel 
          (lambda ()
           (destructuring-bind
            (f) (unix-options:cli-options)
            (format t "~a~%"
             (cl-mailer::email-table-entry f))))
            )'
      "$out/bin/cl-mailer-launcher" \
      --eval '(save-lisp-and-die 
                "'"$out/bin/cl-mailer-make-reply-header"'" 
                :executable t :toplevel 
          (lambda ()
           (destructuring-bind 
            (from inp out) (unix-options:cli-options)
            (with-open-file (f out :direction :output :if-exists :append :if-does-not-exist :create)
             (cl-mailer::make-email-reply-header
              (cl-mailer::read-only-headers inp)
              f from))))
            )'
  '';
  squid-url-rewrite-bis = pkgs.sbcl.buildASDFSystem rec {
    pname = "squid-url-rewrite";
    version = "0.0-unstable";
    description = "URL rewriter for Squid";
    src = /home/raskin/src/lsp/http/src/squid-url-rewrite;
    lispLibs = with pkgs.sbcl.pkgs; [
      string-case iterate cl-ppcre
    ];
  };
  squid-url-rewrite-bis-bin = pkgs.runCommandNoCC "squid-url-rewrite" {} ''
      mkdir -p "$out/bin"
      ${
        pkgs.sbcl.withPackages (p: [squid-url-rewrite-bis])
      }/bin/sbcl \
      --eval '(require :asdf)' \
      --eval '(require :squid-url-rewrite)' \
      --eval '(save-lisp-and-die 
                "'"$out/bin/squid-url-rewrite"'" 
                :executable t :toplevel `process-rewrites)'
  '';
  rare-words = pkgs.runCommandNoCC "" {
    buildInputs = [ (pkgs.sbcl.withPackages (p: with p; [])) ];
  } ''
    mkdir -p "$out/bin"
    sbcl --load ${/home/raskin/src/lsp/topic-sorter/rare-words-1.lisp} --eval '(sb-ext:save-lisp-and-die "${placeholder "out"}/bin/rare-words-1.bin" :executable t :toplevel (function sort-input))'
    sbcl --load ${/home/raskin/src/lsp/topic-sorter/rare-words-1.lisp} --eval '(sb-ext:save-lisp-and-die "${placeholder "out"}/bin/rare-words-1f.bin" :executable t :toplevel (function sort-file-list))'
  '';
})
