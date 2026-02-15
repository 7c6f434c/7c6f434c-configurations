with import ../env-defs.nix;
with pkgs;

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
        (symlinkJoin {
           name = "postgresql-18"; 
           paths = [ postgresql_18.out ];})
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
          runCommand "konsole-colorscheme" {} ''
          mkdir -p "$out/share/konsole"
          cp "${konsole-colorscheme}" "$out/share/konsole/KonsoleMy.colorscheme"
          ''
        }/share" ${kdePackages.konsole}/bin/konsole --profile ${konsole-profile} --hide-tabbar --hide-menubar --separate -p ColorScheme=KonsoleMy "$@";
        '')
        (writeScriptBin "tmux-launcher" ''
        #!/bin/sh
        export PATH="${tmux-to-use}/bin/:$PATH"
        tmux -f ${
          runCommand "tmux.conf" {} ''
          cat ${tmux-profile} > "$out"
          echo run-shell '"${tmux-profile-sh}"' >> "$out"
          ''
        } "$@"
        '')
        ntp mc ncdu ltrace weechat
        htop iotop powertop mtr bind inotify-tools setxkbmap xev
        curl-impersonate
        xset
        xfig fig2dev 
        netpbm
        firefox vim-full evince alsa-utils xvfb-run
        xmodmap bc xdotool lftp wget unzip gnumake xcape xrandr
        xsel xclip pulseaudio ripmime xsetroot lsof rofi
        graphviz diffutils fontconfig picom xprop xwininfo jq
        xlsclients fortune fuse3 openssl axel arping whois hping badvpn dict
        rdap
        appres
        openvpn iftop file patchutils zip gawk perl btrfs-progs
        man man-pages oath-toolkit m4
        (proxychains.overrideAttrs (x: {
          postPatch = (x.postPatch or "") + ''
            sed -e '/while[(]dll_dirs\[i\])/ii=0;' -i src/main.c
          '';
        }))
        zathura
        /*monotoneViz*/ udftools units texinfoInteractive /*yap*/ _3proxy
        python3Packages.pygments poppler-utils libarchive wdiff ydiff
        pass gnupg age easyrsa
        (import ../texlive-set.nix pkgs)
        p7zip mupdf librsvg sxiv
        /* xpdf vifm ffsend kitty */
        latexrun moreutils gnupatch
        shadowsocks-rust
        (aspellWithDicts (p: with p; [en ru de fr da]))
        cmake
        libsixel
        nixpkgs-fmt
        (runCommand "gcc-gcov" {} ''
          mkdir -p "$out/bin"
          ln -s "${gcc.cc}/bin"/gcov* "$out/bin"
          '')
        colordiff
        /* gdmap */
        scrot
        nix-serve
        xauth xhost
        nixpkgs-review
      ]      
