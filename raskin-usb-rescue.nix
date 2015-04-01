{ config, pkgs, ... }:

{

  boot = rec {
    loader = {
      grub = {
        #enable = true;
        enable = false;
        version = 2;
        device = "/dev/sda";
	configurationLimit = 100;
        copyKernels = true;
        splashImage = null;
        extraConfig = ''
        '';
      };
      gummiboot = {
        enable = true;
        #enable = false;
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
      pkgs.linuxPackagesFor pkgs.linux_latest kernelPackages;
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
      videoDrivers = ["ati" "intel" "nv"];
      synaptics = {
        enable = true;
	dev = null;
      };
      layout = "us(altgr-intl-rich),ru(common),gr(basic)";			
      xkbOptions = "grp:caps_toggle, grp_led:caps, lv3:lwin_switch, terminate:ctrl_alt_bksp";	
      useXFS = "unix/:7100";
    };
    printing = {
      enable = true;
      drivers = [pkgs.hplip];
    };
  };
  
  environment = {
    systemPackages = with pkgs;
    [
      vimHugeX ipmitool ipmiutil
      tcpdump subversion screen
      freeipmi utillinuxCurses
      dmraid fbterm xlaunch
      icewm firefox lftp monotone
      hplip pmount mc evince
      xpdf glxinfo git dhcp emacs
      wpa_supplicant iw btrfsProgs
      htop iotop iftop kvm zsh
      xorg.xmodmap elinks lynx wget
      parted gptfdisk gparted
      wavemon smartmontools hdparm
      slmenu dmenu sbcl julia pv mtr
      sshfsFuse fbida imagemagick
      nix-binary-cache
    ];
  };
  
  jobs = {
    ttyS0 = {
      exec = " 38400";
    };
  };

  security = {
    setuidPrograms = [
      "fusermount" "mount" "umount" "xlaunch"
      "lsof" "pmount" "pumount" "fbterm"
    ];
  };

  nix = {
    useChroot = true;
    extraOptions = ''
      binary-caches = http://cache.nixos.org http://nixos.org/binary-cache http://hydra.nixos.org
      trusted-binary-caches = http://cache.nixos.org http://nixos.org/binary-cache http://hydra.nixos.org
    '';
    package = pkgs.nixUnstable;
  };
}
