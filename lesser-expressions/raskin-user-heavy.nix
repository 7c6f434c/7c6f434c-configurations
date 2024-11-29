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
wrapperEnv = package:
  pkgs.buildEnv {
    name = package.name;
    paths = [package];
    ignoreCollisions = false;
    checkCollisionContents = false;
    pathsToLink = ["/"];
    extraOutputsToInstall = [];
  };
in with pkgs;

linkFarm "raskin-heavy-packages" ([
  { name = "main-heavy-package-set";
    path = (fullEnv "main-heavy-package-set"
      [
        gimp ghostscript asymptote
        qemu
        love imagemagick openscad
        vue gwenview gthumb geeqie subversion espeak fossil mercurial djview wxmaxima
        gnuplot mozlz4a lz4 maxima valgrind pdftk lilypond timidity OVMF atop
        gptfdisk dmidecode inkscape x11vnc 
        tigervnc
        xdummy tcpdump wireshark
        testdisk fdupes ntfs3g lazarus icewm yt-dlp xorg.xwd
        vlc sshfs dmtx-utils
        glxinfo xorg.xdpyinfo xorg.xdriinfo go-mtpfs nmap sox
        xorg.xinput usbutils wgetpaste gdb scowl xcalib /*fmbt*/ eprover glucose
        nginx cfdg /* highlight */ transmission_4 spass iprover /*cvc4*/ z3 z3-tptp prover9
        /* signal-desktop */ mustache-spec mustache-go zxing lame
        rustc xournalpp bmap-tools webcamoid
        /* cachix */ darcs /* petrinizer */
        /* swfdec */
        coqPackages.coqide coqPackages.coq
        ripgrep-all
      ]);}
])
