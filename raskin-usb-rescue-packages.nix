{pkgs, ...}:
with pkgs;
    [
      vimHugeX ipmitool ipmiutil
      tcpdump subversion screen
      freeipmi utillinux dmraid fbterm
      icewm firefox lftp monotone
      hplip pmount mc evince
      zathura glxinfo git dhcp emacs
      wpa_supplicant iw btrfs-progs
      htop iotop iftop qemu zsh
      xorg.xmodmap elinks lynx wget
      parted gptfdisk gparted
      wavemon smartmontools hdparm
      fzf sbcl /* julia */ pv mtr
      sshfs-fuse fbida /* imagemagick */
      nix-binary-cache powertop
      gcc fuse rlwrap badvpn
      dmtx-utils 
      /* gnuplot maxima libreoffice */
      rofi /* nbd */ /* mplayer */
      /* pypy27 */ rxvt_unicode geeqie
      /* vue */ squid4 which lsof grub2 grub2_efi 
      efibootmgr
      /* (import ./texlive-set.nix pkgs) */
      clisp xdotool ncdu ntfs3g
      xorg.xdpyinfo xorg.xev pciutils usbutils
      /* midori chromium */ zsh sqlite openssl
      expect bc /* xfig transfig asymptote */
      stumpwm
      pdftk inotifyTools libeatmydata p7zip
      multipath-tools /* textadept */ fossil unzip
      /* xfce.xfwm4 xfce.xfdesktop
      xfce.xfce4-panel xfce.xfce4-settings
      xfce.xfce4-taskmanager xfce.exo
      xfce.thunar xfce.xfce4-session
      xfce.xfconf */
    ]
