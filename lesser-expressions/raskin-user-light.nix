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
        slmenu mcabber ii irssi elinks links2 androidenv.androidsdk_4_2 rsync
        pv lvm2 mariadb remind xterm zsh mlterm ntp mc ncdu ltrace weechat
        htop iotop powertop mtr bind inotify-tools xorg.setxkbmap xfig
        firefox vimHugeX evince xpdf zathura mplayer alsaUtils xvfb_run
        xorg.xmodmap bc xdotool lftp wget unzip p7zip gnumake xcape xorg.xrandr
        xsel xclip pulseaudioLight ripmime xscreensaver xorg.xsetroot lsof rofi
        fpc nix-repl graphviz diffutils fontconfig compton xorg.xprop xorg.xwininfo 
        xorg.xlsclients fortune fuse3 openssl axel arping whois hping badvpn dict
        xdaliclock openvpn iftop file patchutils zip gawk parallel perl btrfs-progs
        man manpages
        (import ./texlive-set.nix pkgs)
      ]      
