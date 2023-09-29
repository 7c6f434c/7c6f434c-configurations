{ pkgs ? import <nixpkgs> {}, ...}: pkgs.lib.makeExtensible ( self: with self; {
  mccme-helpers = pkgs.lispPackages.buildLispPackage {
    baseName = "mccme-helpers";
    version = "unstable";
    description = "Helper code accumulated in MCCME Common Lisp projects";
    src = /home/repos/mccme-helpers-clnet;
    deps = with pkgs.lispPackages; [
      cl-utilities iterate clsql yason cl-ppcre parse-number cl-emb hunchentoot
      ironclad local-time fare-csv trivial-utf-8
    ];
  };
  mccme-helpers-bis = pkgs.sbcl.buildASDFSystem rec {
    pname = "mccme-helpers";
    version = "0.0-unstable";
    description = "Helper code accumulated in MCCME Common Lisp projects";
    src = /home/repos/mccme-helpers-clnet;
    lispLibs = with pkgs.sbcl.pkgs; [
      cl-utilities iterate clsql yason cl-ppcre parse-number cl-emb hunchentoot
      ironclad local-time fare-csv trivial-utf-8
    ];
  };
  cl-mailer = pkgs.lispPackages.buildLispPackage {
    baseName = "cl-mailer";
    version = "unstable";
    description = "My mail processing utilties in Common Lisp";
    src = /home/raskin/src/lsp/venv-cl-fuse/src/cl-mailer;
    deps = with pkgs.lispPackages; [
      mccme-helpers
      cl-smtp trivial-utf-8 local-time cl-ppcre iterate cl-qprint cl-base64
      esrap-peg unix-options net-telent-date
    ];
    overrides = x: {
      postInstall = ''
        NIX_LISP_PRELAUNCH_HOOK='
          nix_lisp_build_system cl-mailer "
          (lambda () 
           (destructuring-bind
            (f) (unix-options::cli-options)
            (format t \"~a~%\"
             (cl-mailer::sendmail f))))
          " "" ""
        ' "$out/bin"/*-lisp-launcher.sh
        mv "$out"/lib/common-lisp/cl-mailer/cl-mailer "$out"/bin/cl-mailer-send-email
        NIX_LISP_PRELAUNCH_HOOK='
          nix_lisp_build_system cl-mailer "
          (lambda ()
           (destructuring-bind
            (f) (unix-options:cli-options)
            (format t \"~a~%\"
             (cl-mailer::email-table-entry f))))
          " "" ""
        ' "$out/bin"/*-lisp-launcher.sh
        mv "$out"/lib/common-lisp/cl-mailer/cl-mailer "$out"/bin/cl-mailer-tabulate-mail
        NIX_LISP_PRELAUNCH_HOOK='
          nix_lisp_build_system cl-mailer "
          (lambda ()
           (destructuring-bind 
            (from inp out) (unix-options:cli-options)
            (with-open-file (f out :direction :output :if-exists :append :if-does-not-exist :create)
             (cl-mailer::make-email-reply-header
              (cl-mailer::read-only-headers inp)
              f from))))
          " "" ""
        ' "$out/bin"/*-lisp-launcher.sh
        mv "$out"/lib/common-lisp/cl-mailer/cl-mailer "$out"/bin/cl-mailer-make-reply-header
      '';
    };
  };
  squid-url-rewrite = pkgs.lispPackages.buildLispPackage rec {
    baseName = "squid-url-rewrite";
    version = "unstable";
    description = "An URL rewriter for Squid";
    src = /home/raskin/src/lsp/http/src/squid-url-rewrite;
    deps = with pkgs.lispPackages; [ string-case iterate cl-ppcre ];
    overrides = x: {
      postInstall = ''
        NIX_LISP_PRELAUNCH_HOOK='
          nix_lisp_build_system ${baseName} "
          (lambda () (process-rewrites))
          " "" ""
        ' "$out/bin"/*-lisp-launcher.sh
        mv "$out"/lib/common-lisp/${baseName}/${baseName} "$out"/bin/${baseName}
      '';
    };
  };
  rare-words = pkgs.runCommandNoCC "" {
    buildInputs = [ (pkgs.sbcl.withPackages (p: with p; [])) ];
  } ''
    mkdir -p "$out/bin"
    sbcl --load ${/home/raskin/src/lsp/topic-sorter/rare-words-1.lisp} --eval '(sb-ext:save-lisp-and-die "${placeholder "out"}/bin/rare-words-1.bin" :executable t :toplevel (function sort-input))'
    sbcl --load ${/home/raskin/src/lsp/topic-sorter/rare-words-1.lisp} --eval '(sb-ext:save-lisp-and-die "${placeholder "out"}/bin/rare-words-1f.bin" :executable t :toplevel (function sort-file-list))'
  '';
})
