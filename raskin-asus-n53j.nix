# ASUS N53J

#00:00.0 Host bridge: Intel Corporation Core Processor DRAM Controller (rev 18)
#00:01.0 PCI bridge: Intel Corporation Core Processor PCI Express x16 Root Port (rev 18)
#00:02.0 VGA compatible controller: Intel Corporation Core Processor Integrated Graphics Controller (rev 18)
#00:16.0 Communication controller: Intel Corporation 5 Series/3400 Series Chipset HECI Controller (rev 06)
#00:1a.0 USB Controller: Intel Corporation 5 Series/3400 Series Chipset USB2 Enhanced Host Controller (rev 06)
#00:1b.0 Audio device: Intel Corporation 5 Series/3400 Series Chipset High Definition Audio (rev 06)
#00:1c.0 PCI bridge: Intel Corporation 5 Series/3400 Series Chipset PCI Express Root Port 1 (rev 06)
#00:1c.1 PCI bridge: Intel Corporation 5 Series/3400 Series Chipset PCI Express Root Port 2 (rev 06)
#00:1c.3 PCI bridge: Intel Corporation 5 Series/3400 Series Chipset PCI Express Root Port 4 (rev 06)
#00:1c.4 PCI bridge: Intel Corporation 5 Series/3400 Series Chipset PCI Express Root Port 5 (rev 06)
#00:1c.5 PCI bridge: Intel Corporation 5 Series/3400 Series Chipset PCI Express Root Port 6 (rev 06)
#00:1d.0 USB Controller: Intel Corporation 5 Series/3400 Series Chipset USB2 Enhanced Host Controller (rev 06)
#00:1e.0 PCI bridge: Intel Corporation 82801 Mobile PCI Bridge (rev a6)
#00:1f.0 ISA bridge: Intel Corporation Mobile 5 Series Chipset LPC Interface Controller (rev 06)
#00:1f.2 SATA controller: Intel Corporation 5 Series/3400 Series Chipset 4 port SATA AHCI Controller (rev 06)
#00:1f.6 Signal processing controller: Intel Corporation 5 Series/3400 Series Chipset Thermal Subsystem (rev 06)
#01:00.0 VGA compatible controller: nVidia Corporation GT215 [GeForce GT 335M] (rev a2)
#03:00.0 Network controller: Atheros Communications Inc. AR9285 Wireless Network Adapter (PCI-Express) (rev 01)
#04:00.0 USB Controller: Device 1b73:1400 (rev 01)
#05:00.0 Class ff00: Realtek Semiconductor Co., Ltd. Device 5209 (rev 01)
#05:00.1 SD Host controller: Realtek Semiconductor Co., Ltd. Device 5209 (rev 01)
#06:00.0 Ethernet controller: Atheros Communications AR8131 Gigabit Ethernet (rev c0)
#ff:00.0 Host bridge: Intel Corporation Core Processor QuickPath Architecture Generic Non-core Registers (rev 05)
#ff:00.1 Host bridge: Intel Corporation Core Processor QuickPath Architecture System Address Decoder (rev 05)
#ff:02.0 Host bridge: Intel Corporation Core Processor QPI Link 0 (rev 05)
#ff:02.1 Host bridge: Intel Corporation Core Processor QPI Physical 0 (rev 05)
#ff:02.2 Host bridge: Intel Corporation Core Processor Reserved (rev 05)
#ff:02.3 Host bridge: Intel Corporation Core Processor Reserved (rev 05)

{config, pkgs, ...}:

let
  texLivePaths = with pkgs; [
          texLive texLiveExtra lmodern texLiveCMSuper 
	  texLiveLatexXColor texLivePGF
	  texLiveBeamer texLiveModerncv tipa texLiveContext 
          texDisser lmmath texinfo5 tex4ht texLiveModerntimeline
     ];
  myTexLive = pkgs.texLiveAggregationFun {
    paths = texLivePaths;
  };
  myKDE = pkgs.kde412;
  kernelToUse = rec {
    kernelPackages = pkgs.linuxPackagesFor pkgs.linux_3_14 kernelPackages;
    extraModulePackages = [kernelPackages.acpi_call /*kernelPackages.aufs */
      kernelPackages.sysdig];
  };
  packageGroups = import /etc/nixos/configurations/misc/raskin/package-groups.nix {
    inherit pkgs myTexLive myKDE;
    baseKernel = kernelToUse;
  };
base = 
{
  require = [
    /etc/nixos/configurations/computer/asus/n53j/n53jn-sz074v.nix
  ];

  boot = rec {
    loader.grub = {
      device = "/dev/sda";
      copyKernels = true;
      configurationLimit = 100;
      version = 2;
    };
    vesa = false;
    kernelPackages = kernelToUse.kernelPackages;
    extraModulePackages = kernelToUse.extraModulePackages;
    initrd = {
	kernelModules = [
		"libata" "sd_mod" 
		"i8042" "pcips2" "serio"  
		"mousedev" "evdev" "psmouse" "sermouse" "synaptics_i2c" 
		"ext3" "lzf" "crc32c" "btrfs" "libcrc32c" 
		"zlib_deflate" "unix" "usbhid" "hid-generic"
		"xhci_hcd" "ehci_hcd" "ehci_pci" "uhci_hcd" "ohci_hcd" 
		"atkbd" "xtkbd" "bfq_iosched" "dm_mod"
		];
    };
    kernelParams = [
    ];
    kernelModules = [
	"fbcon" "i915"
        "usb-storage" "tun" 
	"battery" "ac" "thermal" "fuse" 
	"8139too" "ath9k" 
	"cp2101" "cp210x" "pl2303"
	"smsc75xx"
	"ahci" "loop" "ftdi_sio" "snd_pcm"
	"snd_hda_intel"
	"ipip" "ipv6" "video"
	"fbdev" 
	"pata_atiixp" "sr_mod" "ahci" 
	"pktcdvd" "ide-cd-mod"
        "atl1c" "acpi_call" "kvm-intel" 
        "cpufreq-ondemand" "configs" "rtc-cmos"
	"cdc-acm" "ppp-generic" "ppp-async" 
	"ppp-deflate" "bsd-comp" "cdc-phonet"
	"uvcvideo" "cls-cgroup" "blk-cgroup"
	"mmc-block" "rtsx-pci-sdmmc"
	"uinput" "ax88179_178a" 
	"sysdig-probe"
	];
    hardwareScan = false;
    resumeDevice = "8:2";
    postBootCommands = ''
       exec &> /var/log/post-boot-commands
       /var/run/current-system/sw/bin/umount /tmp/
       /var/run/current-system/sw/sbin/mkfs.ext4 -L tmp /dev/disk/by-label/tmp
       /var/run/current-system/sw/bin/mount /tmp/
       /var/run/current-system/sw/bin/chmod a+rwxt /tmp
       /var/run/current-system/sw/bin/mkdir -m 1777 /tmp/.ICE-unix/
       ! [ -e /tmp/.dev-tty12 ] && /var/run/current-system/sw/bin/mknod -m 0600 /tmp/.dev-tty12 c 4 12
       /bin/sh -c 'while true; do 
         echo -n "Password: " ; read -s pass; 
         [ -e /root/rc/vt12-pass ] && [ "x$pass" = "x$(cat /root/rc/vt12-pass)" ] && /bin/sh; 
	 [ "x$pass" = "xhaltnow" ] && /var/run/current-system/sw/sbin/halt;
	 [ "x$pass" = "xtouchstore" ] && /var/run/current-system/sw/bin/touch /nix/store &
       done < /tmp/.dev-tty12 &> /tmp/.dev-tty12' &
       /var/run/current-system/sw/bin/mount /sys
       /var/run/current-system/sw/bin/mkdir -p /dev/cgroup/cpu

       /run/current-system/sw/sbin/sysctl kernel.shmmax=1200000000
    '';
  };

  swapDevices = [
    {label = "swap";}
  ];
  fileSystems = import ./filesystems-asus-n53j.nix {};

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
    [(pkgs.runCommand "empty" {} "mkdir $out")] ++ 
    (if false then minimal else
    (
	pkgs.lib.concatLists 
	[ 
	    extractors consoleBrowsers programmingLanguages 
	    nonNative hwControl toys filesystems networkTools 
	    fatXApps windowManagers consoleTools buildSupport 
	    utilsX widgets sound graphicView clientServer im
	    misc GCPin versionControl fsTools consoleEditors
	    browsers graphicEdit encryption libraries music
	    monitoring namespacingTools partitionTools debugTools
	    textCrunchers

	   lowPrioPackages
        ]
    ));

    pathsToLink = ["/"];
    shellInit = ''
       export PATH=$HOME/script/override:$PATH:$HOME/script
    '';
  };
  
  fonts = import ./fonts.nix {inherit pkgs;};

  time = {
    timeZone = "Etc/GMT-4";
  };
  
  nix = {
    useChroot = true;
    chrootDirs = ["/home/repos"];

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
      doInstallCheck = false;
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
    setuidPrograms = ["fusermount"
      "mount" "umount" "sudo" "xlaunch"
      "lsof" "suid-chroot" "fbterm" "pmount"
      "pumount"
    ];
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
      configFile = "
#generated
raskin ALL= NOPASSWD: /etc/sudo-scripts/setfreq,\\
		/etc/sudo-scripts/standby,\\
		/etc/sudo-scripts/xfs,\\
		/var/run/current-system/sw/bin/wodim,\\
		/var/run/current-system/sw/sbin/halt,\\
		/etc/sudo-scripts/dateupdate,\\
		/etc/sudo-scripts/eth,\\
		/etc/sudo-scripts/wifi,\\
		/etc/sudo-scripts/wvdial,\\
		/etc/sudo-scripts/nonet,\\
		/etc/sudo-scripts/up-ifs,\\
		/etc/sudo-scripts/brightness,\\
		/etc/sudo-scripts/renice0,\\
		/etc/sudo-scripts/glusterfs-start,\\
		/etc/sudo-scripts/gvpe-start,\\
		/etc/sudo-scripts/chmod,\\
		/etc/sudo-scripts/update-mesa-link,\\
		/etc/sudo-scripts/arpflush,\\
		/etc/sudo-scripts/wpa-status,\\
		/etc/sudo-scripts/nas-halt,\\
		/etc/sudo-scripts/wifi-scan,\\
		/etc/sudo-scripts/start-home-multiplexor,\\
		/etc/sudo-scripts/eth-for-nas,\\
		/etc/sudo-scripts/home-alt-net,\\
		/etc/sudo-scripts/nix-cleanup-tests
raskin ALL= NOPASSWD: SETENV: /etc/sudo-scripts/checkGw6
raskin ALL= /bin/sh
Defaults!/bin/sh rootpw, timestamp_timeout=0
Defaults!/etc/sudo-scripts/wpa-status !syslog
sshguest ALL= /var/run/current-system/sw/bin/ls /home/sshguest
wwwrun	ALL= NOPASSWD: /var/run/current-system/sw/bin/mplayer,\\
	/var/run/current-system/sw/bin/amixer
xserver ALL= NOPASSWD: /var/run/current-system/sw/sbin/start xserver,\\
	/var/run/current-system/sw/sbin/stop xserver
halt ALL= NOPASSWD: /var/run/current-system/sw/sbin/halt
";
    };

  };

  hardware = {
      firmware = ["/var/lib/firmware"];
      pulseaudio.enable = false;
  };
  
  powerManagement = {
    enable = true;
    powerUpCommands = ''
      ${pkgs.hdparm}/sbin/hdparm -B 255 /dev/sda
      echo 1 > /sys/class/backlight/acpi_video0/brightness
    '';
  };

};
in 
base // {
  nesting = {
    children = [
    ];
  };
}
