let
NIXPKGS_env = builtins.getEnv "NIXPKGS";
pkgsPath = if NIXPKGS_env == "" then <nixpkgs> else NIXPKGS_env;
pkgs = import pkgsPath { 
  config = {
    allowInsecurePredicate = x: (
      (
        (pkgs.lib.hasPrefix "curl-impersonate-" (x.name or x.pname))
        ||
        ("curl-impersonate" == (x.name or x.pname))
      )
      &&
      (pkgs.lib.all (y: 
        (pkgs.lib.findFirst (z: z == y) null [
                "CVE-2023-32001"  # fopen TOCTOU race condition - https://curl.se/docs/CVE-2023-32001.html
                "CVE-2022-43551"  # HSTS bypass - https://curl.se/docs/CVE-2022-43551.html
                "CVE-2022-42916"  # HSTS bypass - https://curl.se/docs/CVE-2022-42916.html
        ]) != null
      ) x.meta.knownVulnerabilities)
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
        ntp mc ncdu ltrace weechat
        htop iotop powertop mtr bind inotify-tools xorg.setxkbmap xorg.xev
        curl-impersonate
        xorg.xset
        xfig transfig kig
        firefox vimHugeX evince mplayer alsaUtils xvfb_run
        xorg.xmodmap bc xdotool lftp wget unzip gnumake xcape xorg.xrandr
        xsel xclip pulseaudio ripmime xscreensaver xorg.xsetroot lsof rofi
        fpc graphviz diffutils fontconfig compton xorg.xprop xorg.xwininfo jq
        xorg.xlsclients fortune fuse3 openssl axel arping whois hping badvpn dict
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
        (aspellWithDicts (p: with p; [en ru de fr da]))
      ]      
