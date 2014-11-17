/* 
Configuration for NixOS used by Michael Raskin
*/

{pkgs, config, modulesPath, ...}:
let 
	kernels = import /etc/nixos/configurations/misc/raskin/kernel-options.nix {inherit pkgs;};

        texLivePaths = with pkgs; [texLive texLiveExtra lmodern texLiveCMSuper 
	  texLiveLatexXColor texLivePGF
	  texLiveBeamer texLiveModerncv tipa texLiveContext ];
	myTexLive = pkgs.texLiveAggregationFun {
		paths = texLivePaths;
	};

bootEntries = baseKernel: rec {
#   kernelPackages = let 
#     shippedKernelPackages = pkgs.linuxPackagesFor (baseKernel);
#   in
#   shippedKernelPackages //
#     rec { 
#         klibc = shippedKernelPackages.klibc.passthru.function (x: {
#           # version = "1.5.14";
#           # sha256 = "1cmrqpgamnv2ns7dlxjm61zc88dxm4ff0aya413ij1lmhp2h2sfc";
#           # subdir = "Testing/";
#           addPreBuild = ''
#             ln -s $PWD/linux/include/*/errno.h linux/include/asm || echo errno.h already present
#           '';
#         });
#         
#         klibcShrunk = shippedKernelPackages.klibcShrunk.passthru.function {
#           inherit klibc;
#         };
#     };
    
    kernelPackages = pkgs.linuxPackagesFor baseKernel kernelPackages;

    extraModulePackages = [
      /*(kernelPackages.kqemu.passthru.function (x : {
      defU64 = {
        text = ''
		export NIX_CFLAGS_COMPILE="$NIX_CFLAGS_COMPILE -Du64=__u64"
	'';
        deps=[];
      };
      # version = "1.4.0pre1";
    }))*/
	/*(kernelPackages.kqemu.passthru.function (x : {
	      version = "1.4.0pre1";
	}))*/

        (kernelPackages.kqemu)
    	/*(myAtheros kernelPackages)*/
    ]
    ;
};

#kernelToUse = (bootEntries kernels.testingKernel);
kernelToUse = (bootEntries pkgs.linux_2_6_36);

	packageGroups = import /etc/nixos/configurations/misc/raskin/package-groups.nix {
	  inherit pkgs myTexLive;
	  baseKernel = kernelToUse;
	};

base = 
{
  require = [
    /etc/nixos/nixos/modules/services/hardware/bluetooth.nix
    /etc/nixos/configurations/tud/couchdb.nix
    ];

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
	browsers graphicEdit encryption
		/*[
		(pkgs.wrapFirefox pkgs.firefox3_5 "firefox" "")
		pkgs.xulrunner3_5
		pkgs.thunderbird pkgs.gajim pkgs.openoffice
		pkgs.xterm pkgs.xorg.setxkbmap 
		pkgs.rxvt_unicode pkgs.openssh
		pkgs.monotone pkgs.vimHugeX 
		pkgs.diffutils pkgs.subversion16
		pkgs.utillinuxCurses pkgs.cdrkit
		pkgs.zsh pkgs.wpa_supplicant
		pkgs.iproute pkgs.squid pkgs.pythonFull
		pkgs.xlaunch pkgs.screen pkgs.elinks
		pkgs.xorg.xkbcomp pkgs.xsel pkgs.xorg.xorgserver
		pkgs.xscreensaver pkgs.glxinfo 
		pkgs.neverball pkgs.binutils 
		pkgs.MPlayer pkgs.xpdf pkgs.evince
		pkgs.gnupg2 pkgs.bc pkgs.xorg.xev
		pkgs.xorg.xset pkgs.gnupg pkgs.pinentry
		pkgs.btrfsProgs pkgs.lsof pkgs.manpages
		pkgs.jfsUtils pkgs.gcc pkgs.jfsrec pkgs.bind
		pkgs.zip pkgs.ncat pkgs.relfs pkgs.fuse
		pkgs.smbfsFuse
		]*/

		/*(with pkgs.compizFusion;
		[
			pkgs.compiz 
			bcop ccsm ccsmSimple pluginsMain pluginsExtra 
			compizManager
		])*/
        ]
    ));

	# latest SVN or even personal uncommited version
    nix = pkgs.nixCustomFun ("" + /etc/nixos/nix + "/")
        ""
	["nix-reduce-build" "nix-http-export.cgi"]
	["--with-docbook-xsl=${pkgs.docbook5_xsl}/xml/xsl/docbook/"
	 "--with-docbook-rng=${pkgs.docbook5}/xml/rng/docbook"
	];

    pathsToLink = ["/"];
	
	# Most typos leave needed option at default 
	# (mystically..), but create easy-to-find
	# options NixOS has no idea of. Build should break
	# on them.     
    #checkConfigurationOptions = true;
  };

  boot = rec {
    loader.grub = {
    device = "/dev/sda";

	# GRUB name of /boot
	# It is a separate partition here
	# So killing / will leave me a 
	# chance to boot something (initrd)
    bootDevice = "(hd0,0)";								

    copyKernels = true;
	
	# I do not want to go near 
	# GRUB configuration count limit.    								
	# Look in nix-dev 
    configurationLimit = 100;
    version = 2;
    
    };

	# asus-laptop will let me adjust
	# brightness
    kernelModules = [
	"fbcon" "radeonfb" 
        "usb-storage" "tun" 
    	# "asus_laptop" 
	"battery" "ac" "thermal" "fuse" "kqemu" 
	"8139too" "p4_clockmod" "cp2101" 
	"ati_agp"
	"ahci" "loop" "ftdi_sio" "snd_pcm" "snd_hda_intel" "radeon"
	"ipip" "ipv6" "video"
	"fbdev" 
	"pata_atiixp" "sr_mod" "ahci" "pktcdvd" "ide-cd-mod"
	];

	# S3 is broken for now, 
	# TuxOnIce works (not in latest kernel yet)
    extraKernelParams = ["resume=/dev/sda8" "acpi_sleep=s3_bios"];
	
    kernelPackages =      kernelToUse.kernelPackages;
    extraModulePackages = kernelToUse.extraModulePackages;

    initrd = {
	kernelModules = [
		"libata" "sd_mod" 
		"i8042" "pcips2" "serio"  
		"mousedev" "evdev" "psmouse" "sermouse" "synaptics_i2c" 
		"ext3" "lzf" "crc32c" "btrfs" "libcrc32c" 
		"zlib_deflate" "unix" "usbhid" 
		"xhci_hcd" "ehci_hcd" "uhci_hcd" 
		"ohci_hcd" "atkbd" "xtkbd" "bfq_iosched" 
		"tuxonice_core" "tuxonice_swap" "tuxonice_userui"
		];
    };

    kernelParams = [
      "selinux=0"
      "acpi=on"
      "apm=off"
      "console=tty1"
      "splash=verbose"
    ];
    vesa = false;

    resumeDevice = "8:8";

    hardwareScan = false;

    postBootCommands = ''
       ! [ -e /tmp/.dev-tty12 ] && /var/run/current-system/sw/bin/mknod -m 0600 /tmp/.dev-tty12 c 4 12
       /bin/sh -c 'while true; do 
         echo -n "Password: " ; read -s pass; 
         [ -e /root/.vt12-pass ] && [ "x$pass" = "x$(cat /root/.vt12-pass)" ] && /bin/sh; 
	 [ "x$pass" = "xhaltnow" ] && /var/run/current-system/sw/sbin/halt;
	 [ "x$pass" = "xtouchstore" ] && /var/run/current-system/sw/bin/touch /nix/store &
       done < /tmp/.dev-tty12 &> /tmp/.dev-tty12' &
    '';
  };

  fileSystems = import /etc/nixos/configurations/misc/raskin/filesystems-main.nix {};

  i18n = {
	# My system is mixed utf-8/koi8-r
	# Will be so while I ssh to koi8-r server
	# luit solves the problem, though.
    defaultLocale = "en_US.UTF-8";
    								
    #consoleFont = "koi8r-8x16";
    consoleKeyMap = "ruwin_cplk-UTF-8";
  };

  networking = {
    extraHosts = (import /root/nix-sysconfig/hosts.nix);
    hostName = (import /root/nix-sysconfig/hostname.nix);

    # I manage proxy settings manually anyway
    interfaceMonitor = {					
      enable = false;
    };

    nameservers = [
    ] ++ (import /root/nix-sysconfig/nameservers.nix);

  };

  installer = {
	# Life is easier to control like that
	manifests = if false then [] else [
		http://hydra.nixos.org/jobset/nixpkgs/trunk/channel/latest/MANIFEST
		http://hydra.nixos.org/jobset/nixpkgs/stdenv/channel/latest/MANIFEST
		http://hydra.nixos.org/job/nix/trunk/build/channel/latest/MANIFEST
		http://hydra.nixos.org/project/nixos/channel/latest/MANIFEST
		http://nixos.org/releases/nixpkgs/channels/nixpkgs-unstable/MANIFEST
	];

	repoTypes = {
          svn = { valid = "[ -d .svn ]"; env = [ pkgs.coreutils pkgs.subversion ]; };
          git = { valid = "[ -d .git ]"; env = [ pkgs.coreutils pkgs.git pkgs.gnused /*  FIXME: use full path to sed in nix-pull */ ]; };
        };
  };

  swapDevices = [
    { device = "/dev/sda8"; }
  ];
  
  services = import /etc/nixos/configurations/misc/raskin/services-main.nix {inherit pkgs config;};
  security = {
    extraSetuidPrograms = ["fusermount"
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
		/etc/sudo-scripts/nonet,\\
		/etc/sudo-scripts/up-ifs,\\
		/etc/sudo-scripts/brightness,\\
		/etc/sudo-scripts/renice0,\\
		/etc/sudo-scripts/glusterfs-start,\\
		/etc/sudo-scripts/gvpe-start,\\
		/etc/sudo-scripts/chmod
raskin ALL= NOPASSWD: SETENV: /etc/sudo-scripts/checkGw6
sshguest ALL= /var/run/current-system/sw/bin/ls /home/sshguest
wwwrun	ALL= NOPASSWD: /var/run/current-system/sw/bin/mplayer,\\
	/var/run/current-system/sw/bin/amixer
xserver ALL= NOPASSWD: /var/run/current-system/sw/sbin/start xserver,\\
	/var/run/current-system/sw/sbin/stop xserver
halt ALL= NOPASSWD: /var/run/current-system/sw/sbin/halt
";
    };

  };

  fonts = import /etc/nixos/configurations/misc/raskin/fonts.nix {inherit pkgs;};

  time = {
    timeZone = "Europe/Moscow";
  };
  
  nix = {
    useChroot = true;

    extraOptions = "
gc-keep-outputs = true       # Nice for developers
gc-keep-derivations = true   # Idem
env-keep-derivations = false
    ";
    proxy = "http://127.0.0.1:3128";
  };

    jobs = {
	    cleanXserverLock = {
		    name = "clean-xserver-lock";
		    description = "Clean X lock files";
		    startOn = "filesystem";
		    script = ''
			if ! pgrep X; then 
				rm /tmp/.X*-lock;
			fi
		    '';
		    respawn = false;
	    };
            networkInterfaces.startOn = "never";
	    nscd.startOn = "never";
	    dhclient.startOn = "never";
    };


  nixpkgs.config = import (builtins.getEnv "NIXPKGS_CONFIG");
};

in 
	# Everything above is building my base configuration.
	# But I have multi-boot..
base // 

{							
  nesting = {
	
	# nesting.children - they are included in menu.lst
    children = [						      
	# They are all similar to main configuration, which 
	# reflects my taste 
      /*(base // {services = base.services // 
	     { xserver = base.services.xserver //
	     { videoDriver = "vesa";
	     virtualScreen = null;
	     };};
	     boot = base.boot // {
	     	loader = base.boot.loader // {
			grub = base.boot.loader.grub // {
		     		configurationName = "vesa X";
			};
		};
	     };
	     })*/
      /*(base // {
         boot = base.boot // {
	     	loader = base.boot.loader // {
			grub = base.boot.loader.grub // {
           configurationName = "2.6.33-zen";
			};
		};
           kernelPackages = (bootEntries pkgs.linux_2_6_33_zen1).kernelPackages;
           extraModulePackages = (bootEntries pkgs.linux_2_6_33_zen1).extraModulePackages;
         };
       })
      (base // {
         boot = base.boot // {
	     	loader = base.boot.loader // {
			grub = base.boot.loader.grub // {
           configurationName = "2.6.33-zen-bfs";
			};
		};
           kernelPackages = pkgs.linuxPackagesFor pkgs.linux_2_6_33_zen1_bfs;
           extraModulePackages = (bootEntries pkgs.linux_2_6_33_zen1_bfs).extraModulePackages;
         };
       })*/
      /*(base // {
         boot = base.boot // {
	     	loader = base.boot.loader // {
			grub = base.boot.loader.grub // {
           configurationName = "2.6.32";
			};
		};
           kernelPackages = pkgs.linuxPackagesFor pkgs.linux_2_6_32;
           extraModulePackages = (bootEntries pkgs.linux_2_6_32).extraModulePackages;
         };
       })*/
    ];
  };

  hardware = {
      firmware = ["/var/lib/firmware"];
  };

  passthru = {
  };
}
