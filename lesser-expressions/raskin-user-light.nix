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
             x.version == "6.6"
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
let tmux-to-use = tmux.overrideAttrs (x: 
{
  configureFlags = (x.configureFlags or []) ++ ["--enable-sixel"];
  src = fetchFromGitHub {
    owner = "tmux"; 
    repo="tmux"; 
    rev = "4266d3efc89cdf7d1af907677361caa24b58c9eb"; 
    hash = "sha256-LliON7p1KyVucCu61sPKihYxtXsAKCvAvRBvNgoV0/g="; }; 
    patches = []; 
  });
in

fullEnv "main-light-package-set"
      [
        squid git monotone fbida fbterm postgresql_13 expect pmount fdm
        fzf mcabber ii irssi links2 rsync ratpoison xdummy
        elinks
        (matrixcli.overrideAttrs (x: {
          postPatch = ''
            sed -e '
              s@!= client.user_id:@!= client.user_id or args.include_user:@;
              /return parser/i\    parser_tail.add_argument("-i", "--include-user", dest="include_user", action="store_true", help="include own messages")
              /return parser/i\    parser_listen.add_argument("-i", "--include-user", dest="include_user", action="store_true", help="include own messages")
              s@\['"'"'url'"'"'\]@.get('"'"'url'"'"',"mxc://")@
              '"s@\['msgtype']@.get('msgtype')@"'
              s@listen_handler()@listen_handler(sys.exc_info()[0]); sender_name=event["sender"]; avatar_url="unknown://"; break@
              /def listen_handler(e):/a\    try:
              /def listen_handler(e):/a\      print(e)
              /def listen_handler(e):/a\    except:
              /def listen_handler(e):/a\      pass
              s@content = download_url@content = (event\["content"].get("body") or ("{{ no event body, type: " + event\["type"] + " }}")) +"\\n" + (download_url or "no_url://")@
            ' -i matrixcli
          '';
        }))
        matrix-commander
        pv lvm2 mariadb remind xterm zsh 
        (mlterm.override {enableFeatures = mlterm.enableFeatures // {ssh2 = false;};})
        konsole
        tmux-to-use
        (writeScriptBin "konsole-launcher" ''
        #!/bin/sh
        XDG_DATA_DIRS="${
          runCommandNoCC "konsole-colorscheme" {} ''
          mkdir -p "$out/share/konsole"
          cp "${konsole-colorscheme}" "$out/share/konsole/KonsoleMy.colorscheme"
          ''
        }/share''${XDG_DATA_DIRS:+:}$XDG_DATA_DIRS" ${konsole}/bin/konsole --profile ${konsole-profile} --hide-tabbar --hide-menubar --separate -p ColorScheme=KonsoleMy "$@";
        '')
        (writeScriptBin "tmux-launcher" ''
        #!/bin/sh
        export PATH="${tmux-to-use}/bin/:$PATH"
        tmux -f ${
          runCommandNoCC "tmux.conf" {} ''
          cat ${tmux-profile} > "$out"
          echo run-shell '"${tmux-profile-sh}"' >> "$out"
          ''
        }
        '')
        ntp mc ncdu ltrace weechat
        htop iotop powertop mtr bind inotify-tools xorg.setxkbmap xorg.xev
        (callPackage ./curl-impersonate-fork {})
        xorg.xset
        xfig transfig kig netpbm
        firefox vimHugeX evince mplayer alsaUtils xvfb_run
        xorg.xmodmap bc xdotool lftp wget wget2 unzip gnumake xcape xorg.xrandr
        xsel xclip pulseaudio ripmime xscreensaver xorg.xsetroot lsof rofi
        fpc graphviz diffutils fontconfig picom xorg.xprop xorg.xwininfo jq
        xorg.xlsclients fortune fuse3 openssl axel arping whois hping badvpn dict
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
        monotoneViz udftools units texinfoInteractive yap _3proxy
        python3Packages.pygments poppler_utils libarchive wdiff ydiff
        pass gnupg age easyrsa
        (import ./texlive-set.nix pkgs)
        p7zip mupdf librsvg
        /* xpdf vifm ffsend kitty */
        rosie latexrun moreutils gnupatch
        shadowsocks-rust
        (aspellWithDicts (p: with p; [en ru de fr da]))
        cmake
        termplay libsixel
        nixpkgs-fmt
      ]      
