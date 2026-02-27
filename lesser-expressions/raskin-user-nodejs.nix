with import ./env-defs.nix;
with pkgs;
with nodePackages;
fullEnv "nodejs-package-set" ([ npm ])
