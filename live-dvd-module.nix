{config, pkgs, ...}:

{
  require = [ ../../../nixos/modules/installer/cd-dvd/installation-cd-base.nix ];

  installer.configModule = "./configurations/misc/raskin/live-dvd-module.nix";

  services = {
    xserver = {
      enable = true;
      autorun = false;
      defaultDepth = 16;
      desktopManager.default = "xterm";
      displayManager.slim.enable = true;
      windowManager.icewm.enable = true;
      desktopManager.kde4.enable = false;
    };
    ttyBackgrounds.enable = false;
  };

  isoImage.isoName = "nixos-live-dvd.iso";
  isoImage.includeSystemBuildDependencies = true;

  boot.kernelPackages = let
    kp = pkgs.linuxPackagesFor pkgs.linux_2_6_35 kp;
    in kp;
  boot.kernelParams = [
      "selinux=0"
      "acpi=on"
      "apm=off"
      "console=tty1"
      "splash=verbose"
    ];
  
  environment = {
    systemPackages = with pkgs; with config.boot.kernelPackages; [
      utillinuxCurses 
      ddrescue
      pciutils usbutils hdparm sdparm hddtemp smartmontools
      xfsprogs jfsutils jfsrec nilfs_utils
      (btrfsProgs.override (x : x // {src= fetchgit{
        url = "http://git.kernel.org/pub/scm/linux/kernel/git/mason/btrfs-progs-unstable.git";
        rev = "075587c96c2f39e227847d13ca0ef305b13cd7d3";
        sha256 = "37412383c46d41301bbee84db4e664e78d122b91bbdf88dfbe02996358e3a45c";
        } + "/" ;
        phaseNames = ["doUnpack" "minInit" "fixMakefile"] ++ x.phaseNames ;
        fixMakefile = builderDefs.fullDepEntry (''
          sed -e 's/progs = /progs= /' -i Makefile
          '') ["minInit" "doUnpack"];
        }))
      iproute iputils inetutils wpa_supplicant wirelesstools
      ipw2200fw iwlwifi1000ucode iwlwifi3945ucode
      iwlwifi5000ucode iwlwifi4965ucode
      gw6c gvpe openvpn
      openssl openssh
      fuse ntfs3g fusesmb sshfsFuse glusterfs
      manpages irssi elinks links2 mcabber mutt lftp wget curl
      fbida feh ghostscriptX 
      midori psi pidgin thunderbird firefox36Pkgs.xulrunner
      gnupg seccure
      patch which file diffutils findutils gcc binutils patchelf
      screen
      bvi joe nvi
      emacs 
      ((import ./custom-vim.nix pkgs).merge {src = fetchgit {
        url = "git://repo.or.cz/vim_extended.git";
        rev = "132e8b73fcd18abe1f5bec16c3eabffe1baaf152";
        sha256 = "baa8e24d61962b905516aa544f5a4390b02d0b05cc662d042e268e02fd84ffb8";
      };})
      unrar unzip zip lzma xz p7zip cabextract cpio
      lsof psmisc pstree ltrace strace
      subversion monotone git darcs mercurial bazaar cvs bazaarTools fossil
      parted gparted
      python ruby lua5 perl guile clisp sbcl tcl
      zsh mc
      sqlite
      xlaunch pinentry
      cdrkit 
      MPlayer openoffice evince imagemagickBig
      socat ncat nc6 netcat nmap wireshark tcpdump
    ];
  };

  fonts = import ./fonts.nix {inherit pkgs;};
}
