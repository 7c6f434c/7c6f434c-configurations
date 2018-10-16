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
        gimp ghostscript asymptote qemu love imagemagick7 openscad
        vue gqview geeqie subversion espeak fossil mercurial djview wxmaxima
        gnuplot mozlz4a lz4 maxima valgrind pdftk lilypond timidity OVMF atop
        gptfdisk dmidecode inkscape x11vnc tightvnc xdummy tcpdump wireshark
        testdisk fdupes ntfs3g lazarus icewm youtube-dl swfdec xorg.xwd
        vlc sshfs dmtx glxinfo xorg.xdpyinfo xorg.xdriinfo go-mtpfs nmap
        xorg.xinput usbutils wgetpaste gdb scowl xcalib fmbt eprover glucose
        nginx cfdg highlight cachix darcs
      ]);}
])
