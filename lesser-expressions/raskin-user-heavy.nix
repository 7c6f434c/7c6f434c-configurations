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

linkFarm "raskin-heavy-packages" ([
  { name = "main-heavy-package-set";
    path = (fullEnv "main-heavy-package-set"
      [
        libreoffice gimp ghostscript asymptote qemu chromium love imagemagick7
        midori vue gqview geeqie subversion espeak fossil mercurial djview
        gnuplot mozlz4a lz4 maxima valgrind pdftk lilypond timidity OVMF
        gptfdisk dmidecode inkscape x11vnc tightvnc xdummy
      ]);}
])
