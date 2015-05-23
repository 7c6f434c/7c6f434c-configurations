# ASUS UX32LN
# 00:00.0 Host bridge: Intel Corporation Haswell-ULT DRAM Controller (rev 09)
# 00:02.0 VGA compatible controller: Intel Corporation Haswell-ULT Integrated Graphics Controller (rev 09)
# 00:03.0 Audio device: Intel Corporation Device 0a0c (rev 09)
# 00:04.0 Signal processing controller: Intel Corporation Device 0a03 (rev 09)
# 00:14.0 USB controller: Intel Corporation Lynx Point-LP USB xHCI HC (rev 04)
# 00:16.0 Communication controller: Intel Corporation Lynx Point-LP HECI #0 (rev 04)
# 00:1b.0 Audio device: Intel Corporation Lynx Point-LP HD Audio Controller (rev 04)
# 00:1c.0 PCI bridge: Intel Corporation Lynx Point-LP PCI Express Root Port 1 (rev e4)
# 00:1c.3 PCI bridge: Intel Corporation Lynx Point-LP PCI Express Root Port 4 (rev e4)
# 00:1c.4 PCI bridge: Intel Corporation Lynx Point-LP PCI Express Root Port 5 (rev e4)
# 00:1f.0 ISA bridge: Intel Corporation Lynx Point-LP LPC Controller (rev 04)
# 00:1f.2 SATA controller: Intel Corporation Lynx Point-LP SATA Controller 1 [AHCI mode] (rev 04)
# 00:1f.3 SMBus: Intel Corporation Lynx Point-LP SMBus Controller (rev 04)
# 00:1f.6 Signal processing controller: Intel Corporation Lynx Point-LP Thermal (rev 04)
# 02:00.0 Network controller: Qualcomm Atheros AR9462 Wireless Network Adapter (rev 01)
# 03:00.0 3D controller: NVIDIA Corporation Device 1341 (rev a2)

{config, pkgs, ...}:

let
  myTexLive = import ./texlive-set.nix pkgs;
  myKDE = pkgs.kde414;
  kernelToUse = (import ./kernel-options.nix pkgs).baseKernel;
  packageGroups = import /home/raskin/src/nix/configurations/misc/raskin/package-groups.nix {
    inherit pkgs myTexLive myKDE;
    baseKernel = kernelToUse;
  };
  base = {
    require = [
      ./asus-ux32ln-r4031h.nix
    ];
  boot = {
    loader.gummiboot = {
      enable = true;
      timeout=3;
    };
    kernelPackages = kernelToUse.kernelPackages;
    extraModulePackages = kernelToUse.extraModulePackages;
    hardwareScan = false;
    kernelModules = [
      "usb-storage" "mousedev" "evdev" "psmouse" "msr" "tun"
      "ac" "battery" "thermal" "fuse" "loop" "rtc-cmos" 
      "sysdig-probe" "ax88179_178a" "smsc75xx" "asix" "cdc_ether"
      "usbhid" "hid-generic"
    ];
    kernelParams=[];
    postBootCommands = ''
       exec &> /var/log/post-boot-commands

       /var/run/current-system/sw/bin/umount /tmp/
       /var/run/current-system/sw/sbin/mkfs.ext4 -L Tmp /dev/disk/by-label/tmp
       /var/run/current-system/sw/bin/mount /tmp/
       /var/run/current-system/sw/bin/chmod a+rwxt /tmp
       /var/run/current-system/sw/bin/mkdir -m 1777 /tmp/.ICE-unix/

       /var/run/current-system/sw/bin/mkdir -p /dev/cgroup/cpu

       /run/current-system/sw/sbin/sysctl kernel.shmmax=1200000000

       echo 10 > /sys/class/backlight/acpi_video0/brightness
       echo 0 > /sys/class/leds/asus::kbd_backlight/brightness
    '';
  };
  swapDevices = [
    {label = "Swap";}
  ];
  fileSystems = import ./filesystems-asus-ux32ln.nix {};
  services = (import ./services-main.nix {inherit pkgs config;}) //
  {
    xserver = import ./xserver-intel.nix {inherit pkgs;};
  };
  
  i18n = {
    defaultLocale = "en_US.UTF-8";
    								
    consoleFont = "iso01-12x22";
    consoleKeyMap = "ruwin_cplk-UTF-8";
  };

  system = {
    fsPackages = [ pkgs.btrfsProgs ];
  };

  environment = {
    systemPackages = with packageGroups;
    [(pkgs.runCommand "empty" {} "mkdir $out")] 
    ++ (pkgs.lib.concatLists (pkgs.lib.attrValues bootstrap))
    ++ (pkgs.lib.concatLists (pkgs.lib.attrValues constantly_used))
    ;

    pathsToLink = ["/"];
    shellInit = ''
       export PATH=$HOME/script/override:$PATH:$HOME/script
    '';
  };
  
  fonts = import ./fonts.nix {inherit pkgs;};

  time = {
    timeZone = "Etc/GMT-3";
  };
  
  nix = {
    useChroot = true;
    chrootDirs = ["/home/repos"];
    distributedBuilds = true;
    buildMachines = with import ./nix-build-machines.nix; [
      gb-bxi7-4770r-1
      gb-bxi7-4770r-1-i686
    ];

    maxJobs = 4;

    extraOptions = "
gc-keep-outputs = true       # Nice for developers
gc-keep-derivations = true   # Idem
env-keep-derivations = false

binary-caches = http://nixos.org/binary-cache http://cache.nixos.org
trusted-binary-caches = http://nixos.org/binary-cache http://cache.nixos.org http://hydra.nixos.org
    ";
    proxy = "http://127.0.0.1:3128";

    package = pkgs.lib.overrideDerivation pkgs.nixUnstable (x: rec {
      #src = "/home/repos/nix/";
      #revisionStamp = (builtins.readFile (src + "/.git/refs/heads/master"));
      preConfigure = ''
        sed -e '/bin_SCRIPTS = /anix-reduce-build \\' -i scripts/local.mk
        sed -e '/bin_SCRIPTS = /anix-http-export.cgi \\' -i scripts/local.mk
        export AC_LOCAL_PATH="$AC_LOCAL_PATH:${pkgs.autoconf}/share/aclocal:${pkgs.libtool}/share/aclocal:${pkgs.automake}/share/aclocal"
        set
	export
        ./bootstrap.sh
      '' + (if x ? preConfigure then x.preConfigure else "");
      nativeBuildInputs = (with pkgs; [
        autoconf automake libtool bison flex gettext
	perlPackages.WWWCurl perlPackages.DBDSQLite perlPackages.DBI
	libxml2 libxslt w3m
	docbook5 docbook5_xsl docbook_xml_dtd_45
      ]) ++ (x.nativeBuildInputs or []);
      buildInputs = (with pkgs; [
	perlPackages.WWWCurl perlPackages.DBDSQLite perlPackages.DBI
      ]) ++ x.buildInputs;
      configureFlags = x.configureFlags + ''
          --with-docbook-rng=${pkgs.docbook5}/xml/rng/docbook
          --with-docbook-xsl=${pkgs.docbook5_xsl}/xml/xsl/docbook
          --with-xml-flags=--nonet
	  --with-www-curl=${pkgs.perlPackages.WWWCurl}/${pkgs.perl.libPrefix}
      '';
      doInstallCheck = false;
    }); 

  };

    jobs = {
	    # makeUserCgroup = {
	    #   name = "make-user-cgroup";
	    #   description = "Create cgroup where user can create subgroups";
	    #   startOn = "filesystem";
	    #   script = ''
	    #     mkdir -m 0777 /dev/cgroup/user
	    #     echo -n /etc/sudo-scripts/clean-cgroup > /dev/cgroup/release_agent
	    #   '';
	    #   respawn = false;
	    #   task = true;
	    # };
            networkInterfaces.startOn = "never";
	    dhclient.startOn = "never";
    };

  networking = {
    extraHosts = (import /root/nix-sysconfig/hosts.nix);
    hostName = (import /root/nix-sysconfig/hostname.nix).hostname;
    domain = (import /root/nix-sysconfig/hostname.nix).domain;

    # I manage proxy settings manually anyway
    interfaceMonitor = {					
      enable = false;
    };

    nameservers = [
    ] ++ (import /root/nix-sysconfig/nameservers.nix);

    useDHCP = false;
    firewall.enable = false;
  };

  nixpkgs.config = import (builtins.getEnv "NIXPKGS_CONFIG");

  security = {
    setuidPrograms = import ./setuid-programs.nix;
    setuidOwners = [
      {
        program = "sendmail";
	group = "postdrop";
	setgid = true;
      }
      {
        program = "postdrop";
	group = "postdrop";
	setgid = true;
      }
    ];
    sudo = {
      configFile = (builtins.readFile ./sudoers); };

  };

  hardware = {
      firmware = ["/var/lib/firmware"];
      pulseaudio.enable = false;
      bluetooth.enable = false;
      bumblebee.group = "video";
  };
  
  powerManagement = {
    enable = true;
    powerUpCommands = ''
      ${pkgs.hdparm}/sbin/hdparm -B 255 /dev/sda
      echo 10 > /sys/class/backlight/acpi_video0/brightness
      echo 0 > /sys/class/leds/asus::kbd_backlight/brightness
    '';
  };

  programs = {
    ssh = {
      extraConfig = ''
        ConnectTimeout 3
      '';
    };
  };
};
in 
base // {
  nesting = {
    children = [
    ];
  };
}
