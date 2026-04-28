with import ./env-defs.nix;
with pkgs;
fullEnv "nodejs-package-set" ([ nodejs ])
