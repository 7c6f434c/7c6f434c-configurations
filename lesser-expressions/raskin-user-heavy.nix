with import ./env-defs.nix;
with pkgs;

fullEnv "main-heavy-package-set"
      [
        gimp ghostscript asymptote
        qemu
        love imagemagick 
        openscad-unstable
        kdePackages.gwenview gthumb geeqie subversion espeak fossil mercurial djview wxmaxima
        gnuplot mozlz4a lz4 maxima valgrind pdftk lilypond timidity OVMF atop
        gptfdisk dmidecode inkscape x11vnc 
        tigervnc
        xdummy tcpdump wireshark
        testdisk fdupes ntfs3g lazarus icewm yt-dlp xwd
        vlc sshfs dmtx-utils
        mesa-demos xdpyinfo xdriinfo go-mtpfs nmap sox
        xinput usbutils wgetpaste gdb scowl xcalib /*fmbt*/ eprover 
        glucose kissat
        nginx cfdg /* highlight */ transmission_4 spass iprover z3 z3-tptp 
        cvc4 cvc5
        /*prover9*/
        /* signal-desktop */ mustache-spec mustache-go zxing lame
        rustc xournalpp bmaptool webcamoid
        /* cachix */ darcs /* petrinizer */
        /* swfdec */
        /* coqPackages.coqide coqPackages.coq */
        ripgrep-all
        qvge
        meshlab
        telegram-desktop
        /* (libsForQt5.callPackage ./qvge {}) */
        (let models = callPackage ./bergamot-model.nix {
          bergamot = callPackage ./bergamot.nix {};
        }; in runCommand "my-bergamot-set" {} ''
          mkdir -p "$out/bin"
          cd "$out/bin"
          ln -s "${models.fren.wrapperHTML}"/bin/* bergamot-fren
          ln -s "${models.enfr.wrapperHTML}"/bin/* bergamot-enfr
          ln -s "${models.enru.wrapperHTML}"/bin/* bergamot-enru
          ln -s "${models.ruen.wrapperHTML}"/bin/* bergamot-ruen
          ln -s "${models.ende.wrapperHTML}"/bin/* bergamot-ende
          ln -s "${models.deen.wrapperHTML}"/bin/* bergamot-deen
        '')
      ]
