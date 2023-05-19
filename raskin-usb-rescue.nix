{ config, pkgs, ... }:

{
  system.stateVersion = "18.09";
  boot = rec {
    loader = {
      grub = {
        enable = true;
        #enable = false;
        version = 2;
        device = "nodev";
	configurationLimit = 100;
        copyKernels = true;
        splashImage = null;
        extraConfig = ''
        '';
        efiSupport = true;
      };
      systemd-boot = {
        #enable = true;
        enable = false;
      };
    };
    initrd = {
      kernelModules = [
        "xhci_hcd"
	"ehci_hcd" "ohci_hcd"
	"uhci_hcd"
	"usb_storage"
      ];
    };
    kernelPackages = 
      pkgs.linuxPackagesFor pkgs.linux_latest;
    vesa = false;
    kernelParams = [
      "console=ttyS0,115200,n8r"
      "console=tty0"
    ];
    blacklistedKernelModules = ["ath3k"];
  };

  fileSystems = {
    "/" = {
      mountPoint = "/";
      label = "NixOSRescue";
      fsType = "ext4";
    };
    "/boot" = {
      mountPoint = "/boot";
      device = "/dev/disk/by-uuid/7A89-9B4C";
      fsType = "vfat";
    };
  };

  documentation.nixos.enable = false;
  services = {
    openssh = {
      enable = true;
      settings.PermitRootLogin = "no";
      settings.UseDNS = "no";
    };
    xserver = {
      enable = true;
      autorun = false;
      exportConfiguration = true;
      enableTCP = true;
      videoDrivers = ["modesetting" "nv" "ati" "cirrus" "vesa"];
      synaptics = {
        enable = true;
	dev = null;
      };
      layout = "us(altgr-intl),ru(common),gr(basic)";			
      xkbOptions = "grp:caps_toggle, grp_led:caps, lv3:lwin_switch, terminate:ctrl_alt_bksp";	
    };
    printing = {
      enable = true;
      drivers = [pkgs.hplip];
    };
    nix-serve = {
      enable = true;
      port = 32062;
    };
  };
  
  environment = {
    systemPackages = (import ./raskin-usb-rescue-packages.nix) pkgs;
  };
  
  security = {
    wrappers = {
      fusermount = { owner = "root"; group = "root"; source = "${pkgs.fuse}/bin/fusermount"; };
      fusermount3 = { owner = "root"; group = "root"; source = "${pkgs.fuse3}/bin/fusermount3"; };
      mount = { owner = "root"; group = "root"; source = "${pkgs.utillinux}/bin/mount"; };
      umount = { owner = "root"; group = "root"; source = "${pkgs.utillinux}/bin/umount"; };
      lsof = { owner = "root"; group = "root"; source = "${pkgs.lsof}/bin/lsof"; };
      pmount = { owner = "root"; group = "root"; source = "${pkgs.pmount}/bin/pmount"; };
      pumount = { owner = "root"; group = "root"; source = "${pkgs.pmount}/bin/pumount"; };
      fbterm = { owner = "root"; group = "root"; source = "${pkgs.fbterm}/bin/fbterm"; };
    };
    sudo = {
      configFile = (builtins.readFile ./sudoers); };
  };

  nix = {
    settings.sandbox = true;
    settings.require-sigs = false;
    extraOptions = ''
      binary-caches = http://cache.nixos.org
      trusted-binary-caches = http://cache.nixos.org
    '';
    #package = pkgs.nixUnstable;
  };

  fonts = {
    enableGhostscriptFonts = true;
    fontDir.enable = true;

    fonts = with pkgs; [
      (ghostscript + "/share/ghostscript/fonts/")
      pkgs.dejavu_fonts
      pkgs.liberation_ttf
      pkgs.lmodern
      pkgs.cm_unicode
      pkgs.lmmath
      pkgs.unifont
      pkgs.xorg.fontcronyxcyrillic
    ];
  };

  hardware = {
    enableRedistributableFirmware = true;
  };
}
