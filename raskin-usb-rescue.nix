{ config, pkgs, ... }:

{

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
        memtest86.enable = true;
      };
      systemd-boot = {
        #enable = true;
        enable = false;
      };
    };
    initrd = {
      kernelModules = [
        "fbcon"
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

  fileSystems = [
    {
      mountPoint = "/";
      label = "NixOSRescue";
      fsType = "ext4";
    }
    {
      mountPoint = "/boot/";
      label = "RESCUE_EFI";
      fsType = "vfat";
    }
  ];

  services = {
    openssh = {
      enable = true;
      permitRootLogin = "no";

      extraConfig = ''
        UseDNS no
      '';
    };
    nixosManual.enable = false;
    xserver = {
      enable = true;
      autorun = false;
      exportConfiguration = true;
      enableTCP = true;
      videoDrivers = ["intel" "nv" "ati" "cirrus" "vesa"];
      synaptics = {
        enable = true;
	dev = null;
      };
      layout = "us(altgr-intl),ru(common),gr(basic)";			
      xkbOptions = "grp:caps_toggle, grp_led:caps, lv3:lwin_switch, terminate:ctrl_alt_bksp";	
      useXFS = "unix/:7100";
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
      fusermount = { source = "${pkgs.fuse}/bin/fusermount"; };
      fusermount3 = { source = "${pkgs.fuse3}/bin/fusermount3"; };
      mount = { source = "${pkgs.utillinux}/bin/mount"; };
      umount = { source = "${pkgs.utillinux}/bin/umount"; };
      lsof = { source = "${pkgs.lsof}/bin/lsof"; };
      pmount = { source = "${pkgs.pmount}/bin/pmount"; };
      pumount = { source = "${pkgs.pmount}/bin/pumount"; };
      fbterm = { source = "${pkgs.fbterm}/bin/fbterm"; };
    };
    sudo = {
      configFile = (builtins.readFile ./sudoers); };
  };

  nix = {
    useSandbox = true;
    extraOptions = ''
      binary-caches = http://cache.nixos.org http://nixos.org/binary-cache
      trusted-binary-caches = http://cache.nixos.org http://nixos.org/binary-cache
    '';
    requireSignedBinaryCaches = false;
    #package = pkgs.nixUnstable;
  };

  fonts = {
    enableGhostscriptFonts = true;
    enableFontDir = true;

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
