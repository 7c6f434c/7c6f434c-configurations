let
NIXPKGS_env = builtins.getEnv "NIXPKGS";
pkgsPath = if NIXPKGS_env == "" then <nixpkgs> else NIXPKGS_env;
pkgs = import pkgsPath {};
allOutputNames = packages: builtins.attrNames
      (pkgs.lib.fold
        (a: b: b //
          (builtins.listToAttrs (map (x: {name = x; value = x;}) a.outputs or ["out"])))
        {} packages);
fullEnv = name: packages:
  pkgs.buildEnv {
      name = name;
      paths = packages;
      ignoreCollisions = false;
      checkCollisionContents = true;
      pathsToLink = ["/"];
      extraOutputsToInstall = (allOutputNames packages);
    };
in with pkgs;

linkFarm "raskin-ultraheavy-packages" ([
  { name = "main-ultraheavy-package-set";
    path = (fullEnv "main-ultraheavy-package-set"
      [
        libreoffice chromium qutebrowser wineUnstable 
        /* sage */ midori scilab-bin /*clasp-common-lisp*/
        ffmpeg-full obs-studio audacity pitivi
      ]);}
])

