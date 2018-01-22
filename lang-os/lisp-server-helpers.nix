{
  pkgs, src, deps
}:
pkgs.lispPackages.buildLispPackage {
    inherit (pkgs) stdenv;
    inherit (pkgs.lispPackages) clwrapper;

    inherit src deps;

    baseName = "server-helpers";
    buildSystems = ["server-helpers"];
    description = "Local library for Common Lisp system server";

    overrides = x: {
      postInstall = ''
        NIX_LISP_PRELAUNCH_HOOK='nix_lisp_run_single_form "(asdf:perform (quote asdf:monolithic-compile-bundle-op) :server-helpers)"' "$out"/bin/*-lisp-launcher.sh ""
      '';
    };
  }

