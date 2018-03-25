{pkgs, ...}:
with pkgs;
    [
      vimHugeX ipmitool ipmiutil
      tcpdump subversion screen
      freeipmi utillinux dmraid fbterm
      icewm firefox lftp monotone
      hplip pmount mc evince
      xpdf glxinfo git dhcp emacs
      wpa_supplicant iw btrfsProgs
      htop iotop iftop qemu zsh
      xorg.xmodmap elinks lynx wget
      parted gptfdisk gparted
      wavemon smartmontools hdparm
      slmenu sbcl julia pv mtr
      sshfsFuse fbida imagemagick
      nix-binary-cache powertop
      gcc fuse rlwrap badvpn
      dmtx gnuplot maxima libreoffice
      slmenu dmenu2 nbd mplayer
      python rxvt_unicode geeqie
      (import ./private-packages.nix {}).slimerjs
      vue squid4 which lsof grub2 grub2_efi 
      efibootmgr
      (import ./texlive-set.nix pkgs)
      clisp xdotool ncdu ntfs3g
      xorg.xdpyinfo xorg.xev pciutils usbutils
      midori chromium zsh sqlite openssl
      expect bc xfig transfig asymptote 
      lispPackages.stumpwm
      pdftk inotifyTools libeatmydata p7zip
      multipath_tools textadept fossil unzip
      xfce.xfwm4 xfce.xfdesktop
      xfce.xfce4panel xfce.xfce4settings
      xfce.xfce4taskmanager xfce.exo
      xfce.thunar xfce.xfce4session
      xfce.xfconf
    ]
