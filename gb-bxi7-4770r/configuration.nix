# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

rec {
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the gummiboot efi boot loader.
  boot.loader.grub.enable = false;
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackagesFor pkgs.linux_latest;
  boot.extraModulePackages = [
    /*boot.kernelPackages.sysdig*/
    (pkgs.runCommand "firmware-rtl8821ae" {} ''
      mkdir -p "$out/lib/firmware/rtlwifi"
      cp "${pkgs.firmwareLinuxNonfree}/lib/firmware/rtlwifi/rtl8821aefw.bin" "$out/lib/firmware/rtlwifi/"
    '')
    ];

  # networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless.

  # Select internationalisation properties.
  # i18n = {
  #   consoleFont = "lat9w-16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.openssh.extraConfig = ''
    UseDNS no
  '';

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable the X11 windowing system.
  # services.xserver.enable = true;
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.kdm.enable = true;
  # services.xserver.desktopManager.kde4.enable = true;
  
  services.gogoclient = {
    enable = true;
    username = (import /root/nix-sysconfig/gw6c.nix).brix1_aarnet.username;
    password = "/root/nix-sysconfig/gw6c.brix1.pass";
    server   = (import /root/nix-sysconfig/gw6c.nix).brix1_aarnet.server;
  };

  services.xserver = {
    enable = true;
    autorun = false;
    enableTCP = true;
    virtualScreen = {x=3520; y=2200;};
    layout = "us(altgr-intl),ru(common),gr(basic)";			
    xkbOptions = "grp:caps_toggle, grp_led:caps, lv3:lwin_switch, terminate:ctrl_alt_bksp";	
  };

  services.nixosManual.enable = false;

  services.postgresql = {
	  enableTCPIP = true;
	  enable = true;
	  authentication = ''
		  host all all 192.168.0.0/16 md5
		  '';
	  extraConfig = ''
		  work_mem = 16MB
		  shared_buffers = 1GB
		  '';
	  package = pkgs.postgresql92;
  };

  services.nix-serve = {
    enable = true;
    port = 32062;
  };

  services.logind = {
    extraConfig = ''
      KillUserProcesses = no
    '';
  };

  networking.wireless.enable = false;
  networking.dhcpcd.runHook = ''
  export PATH=${pkgs.iproute}/sbin:${pkgs.iproute}/bin:${pkgs.coreutils}/bin:${pkgs.gnugrep}/bin:${pkgs.gnused}/bin:${pkgs.curl}/bin:$PATH
  iface=$(ip l | grep 74:d4:35:65:c7:5f -B1 | head -n 1 | sed -re 's/^[0-9]+: //; s/:.*//')
  ip l set $iface up || true
  ip a a 192.168.0.202/28 dev $iface || true
  /root/script/update_dns_brix1
  '';
  networking.firewall.enable = false;

  hardware.enableAllFirmware = true;
  #hardware.firmware = ["/var/lib/firmware"];

  environment = {
    pathsToLink = ["/"];
    systemPackages = with pkgs; [
	    vim gptfdisk utillinux git wpa_supplicant subversion mtr
	    btrfsProgs dhcp screen monotone sshfsFuse zsh squid4
	    ecl rlwrap which gcc htop parallel dmtx pdftk iotop iftop
	    bind sbcl mercurial unzip pv lsof qrencode zxing axel
	    (import ../texlive-set.nix pkgs)
	    mplayer lame sox ffmpeg julia_05 octave maxima openssl sqlite 
	    gnumake wget /*sysdig*/ asymptote smbnetfs imagemagick7Big 
      zbar
	    quirc mono xvfb_run xorg.xauth xorg.xwininfo xorg.xkill
	    xdotool x11vnc lynx inotifyTools ghostscript firefox 
	    ratpoison evince xpdf ncdu fbterm nbd postgresql92 elinks
	    dmenu2 slmenu libreoffice nmap pmount clisp fbida espeak
	    wineUnstable emacs qemu p7zip rxvt_unicode edk2 OVMF keynav
	    gparted parted glpk ccl file gfortran tesseract
	    ];
  };

  nix = {
    useSandbox = true;
    buildCores = 8;
    sandboxPaths = ["/home/repos"];
    extraOptions = "
gc-keep-outputs = true       # Nice for developers
gc-keep-derivations = true   # Idem
env-keep-derivations = false

binary-caches = http://cache.nixos.org http://192.168.0.203:32062/
trusted-binary-caches = http://cache.nixos.org http://192.168.0.203:32062/
    ";
    requireSignedBinaryCaches = false;
    package = pkgs.nix.out; 
  };

  security = {
    sudo = {
      configFile = ''
#generated
raskin ALL= NOPASSWD: /etc/sudo-scripts/setfreq,\
  /etc/sudo-scripts/standby,\
  /etc/sudo-scripts/xfs,\
  /var/run/current-system/sw/bin/wodim,\
  /var/run/current-system/sw/sbin/halt,\
  /etc/sudo-scripts/dateupdate,\
  /etc/sudo-scripts/eth,\
  /etc/sudo-scripts/wifi,\
  /etc/sudo-scripts/wvdial,\
  /etc/sudo-scripts/nonet,\
  /etc/sudo-scripts/up-ifs,\
  /etc/sudo-scripts/brightness,\
  /etc/sudo-scripts/renice0,\
  /etc/sudo-scripts/glusterfs-start,\
  /etc/sudo-scripts/gvpe-start,\
  /etc/sudo-scripts/chmod,\
  /etc/sudo-scripts/update-mesa-link,\
  /etc/sudo-scripts/arpflush,\
  /etc/sudo-scripts/wpa-status,\
  /etc/sudo-scripts/nas-halt,\
  /etc/sudo-scripts/wifi-scan,\
  /etc/sudo-scripts/start-home-multiplexor,\
  /etc/sudo-scripts/eth-for-nas,\
  /etc/sudo-scripts/home-alt-net,\
  /etc/sudo-scripts/nix-cleanup-tests,\
  /run/current-system/sw/bin/chvt,\
  /run/current-system/sw/bin/true
raskin ALL= NOPASSWD: SETENV: /etc/sudo-scripts/checkGw6
raskin ALL= /bin/sh
Defaults!/bin/sh rootpw, timestamp_timeout=0
Defaults!/etc/sudo-scripts/wpa-status !syslog
sshguest ALL= /var/run/current-system/sw/bin/ls /home/sshguest
wwwrun	ALL= NOPASSWD: /var/run/current-system/sw/bin/mplayer,\
	/var/run/current-system/sw/bin/amixer
xserver ALL= NOPASSWD: /var/run/current-system/sw/sbin/start xserver,\
	/var/run/current-system/sw/sbin/stop xserver
halt ALL= NOPASSWD: /var/run/current-system/sw/sbin/halt
      '';
    };
    wrappers.fbterm.source ="${pkgs.fbterm}/bin/fbterm";
    wrappers.pmount.source ="${pkgs.pmount}/bin/pmount";
    wrappers.pumount.source="${pkgs.pmount}/bin/pumount";
  };
}
