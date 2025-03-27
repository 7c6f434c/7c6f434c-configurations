let
NIXPKGS_env = builtins.getEnv "NIXPKGS";
pkgsPath = if NIXPKGS_env == "" then <nixpkgs> else NIXPKGS_env;
pkgs = import pkgsPath { 
    config = {
        allowInsecurePredicate = x: (
            (
             (
              (pkgs.lib.hasPrefix "curl-impersonate-" (x.name or x.pname))
              ||
              ("curl-impersonate" == (x.name or x.pname))
             )
             &&
             (pkgs.lib.all (y: 
                            (pkgs.lib.findFirst (z: z == y) null [
                             "CVE-2023-38545"  # socks5h long hostname heap overflow; I don't use that combo for impersonate
                             "CVE-2023-32001"  # fopen TOCTOU race condition - https://curl.se/docs/CVE-2023-32001.html
                             "CVE-2022-43551"  # HSTS bypass - https://curl.se/docs/CVE-2022-43551.html
                             "CVE-2022-42916"  # HSTS bypass - https://curl.se/docs/CVE-2022-42916.html
                            ]) != null
                           ) x.meta.knownVulnerabilities)
            ) 
            ||
            (
             x.pname == "squid"
             &&
             (
               x.version == "7.0.1"
               )
            )
        );
    };
};
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
let konsole-profile = ./rc.private/konsole.profile; in
let konsole-colorscheme = ./rc.private/KonsoleMyLight.colorscheme; in
let tmux-profile = ./rc.private/tmux/tmux.conf; in
let tmux-profile-sh = ./rc.private/tmux/tmux.conf.sh; in
#let tmux-to-use = tmux.overrideAttrs (x: 
#{
#  patches = tmux.patches ++ [
#    ./tmux-sixel-fixes-issue-3839-at-2024-02-25.diff
#  ]; 
#});
###
let tmux-to-use = tmux;
in

fullEnv "main-light-package-set"
      [
        squid git monotone fbida fbterm 
        git-lfs
        (symlinkJoin {
           name = "postgresql-13"; 
           paths = [ postgresql_13.out ];})
        expect /*pmount*/ fdm
        fzf mcabber ii irssi links2 rsync ratpoison xdummy
        elinks
        
        /*matrix-commander*/
        pv lvm2 mariadb remind xterm zsh 
        (mlterm.override {enableFeatures = mlterm.enableFeatures // {ssh2 = false;};})
        kdePackages.konsole
        tmux-to-use
        (writeScriptBin "konsole-launcher" ''
        #!/bin/sh
        XDG_DATA_DIRS="${
          runCommandNoCC "konsole-colorscheme" {} ''
          mkdir -p "$out/share/konsole"
          cp "${konsole-colorscheme}" "$out/share/konsole/KonsoleMy.colorscheme"
          ''
        }/share" ${kdePackages.konsole}/bin/konsole --profile ${konsole-profile} --hide-tabbar --hide-menubar --separate -p ColorScheme=KonsoleMy "$@";
        '')
        (writeScriptBin "tmux-launcher" ''
        #!/bin/sh
        export PATH="${tmux-to-use}/bin/:$PATH"
        tmux -f ${
          runCommandNoCC "tmux.conf" {} ''
          cat ${tmux-profile} > "$out"
          echo run-shell '"${tmux-profile-sh}"' >> "$out"
          ''
        } "$@"
        '')
        ntp mc ncdu ltrace weechat
        htop iotop powertop mtr bind inotify-tools xorg.setxkbmap xorg.xev
        #(callPackage ./curl-impersonate-fork {})
        curl-impersonate
        xorg.xset
        xfig transfig 
        libsForQt5.kig 
        netpbm
        firefox vimHugeX evince mplayer alsa-utils xvfb-run
        xorg.xmodmap bc xdotool lftp wget wget2 unzip gnumake xcape xorg.xrandr
        xsel xclip pulseaudio ripmime xscreensaver xorg.xsetroot lsof rofi
        fpc graphviz diffutils fontconfig picom xorg.xprop xorg.xwininfo jq
        cflow
        xorg.xlsclients fortune fuse3 openssl axel arping whois hping badvpn dict
        rdap
        megatools
        xorg.appres
        xdaliclock openvpn iftop file patchutils zip gawk perl btrfs-progs
        man man-pages oathToolkit wavemon m4
        (proxychains.overrideAttrs (x: {
          postPatch = (x.postPatch or "") + ''
            sed -e '/while[(]dll_dirs\[i\])/ii=0;' -i src/main.c
          '';
        }))
        zathura
        /*monotoneViz*/ udftools units texinfoInteractive yap _3proxy
        python3Packages.pygments poppler_utils libarchive wdiff ydiff
        pass gnupg age easyrsa
        (import ./texlive-set.nix pkgs)
        p7zip mupdf librsvg sxiv
        /* xpdf vifm ffsend kitty */
        rosie latexrun moreutils gnupatch
        shadowsocks-rust
        (aspellWithDicts (p: with p; [en ru de fr da]))
        cmake
        libsixel
        nixpkgs-fmt
        (runCommandNoCC "gcc-gcov" {} ''
          mkdir -p "$out/bin"
          ln -s "${gcc.cc}/bin"/gcov* "$out/bin"
          '')
        colordiff
        /* gdmap */ qdirstat
        mucommander
        scrot
        nixfmt-rfc-style
      ]      
