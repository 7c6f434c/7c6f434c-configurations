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

fullEnv "main-light-package-set"
      [
        squid git monotone fbida fbterm postgresql95 expect pmount fdm python2
        slmenu fzf mcabber ii irssi elinks links2 rsync ratpoison xdummy
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
              s@content = download_url@content = event\["content"].get("body") +"\\n"+ download_url@
            ' -i matrixcli
          '';
        }))
        matrix-recorder
        pv lvm2 mariadb remind xterm zsh mlterm ntp mc vifm ncdu ltrace weechat
        htop iotop powertop mtr bind inotify-tools xorg.setxkbmap xorg.xev
        xfig transfig kig
        firefox vimHugeX evince zathura mplayer alsaUtils xvfb_run
        xorg.xmodmap bc xdotool lftp wget unzip gnumake xcape xorg.xrandr
        xsel xclip pulseaudioLight ripmime xscreensaver xorg.xsetroot lsof rofi
        fpc graphviz diffutils fontconfig compton xorg.xprop xorg.xwininfo jq
        xorg.xlsclients fortune fuse3 openssl axel arping whois hping badvpn dict
        xdaliclock openvpn iftop file patchutils zip gawk parallel perl btrfs-progs
        man manpages oathToolkit wavemon proxychains screenkey untrunc
        monotoneViz udftools units texinfoInteractive kitty yap _3proxy
        python3Packages.pygments ffsend poppler_utils libarchive
        (import ./texlive-set.nix pkgs)
        /* xpdf */
      ]      
