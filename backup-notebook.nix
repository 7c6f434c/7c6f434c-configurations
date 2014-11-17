/* 
Configuration for NixOS used by Michael Raskin
- for backup notebook..
*/

{pkgs, config, modulesPath, ...}:
let 
	kernels = import /etc/nixos/configurations/misc/raskin/kernel-options.nix {inherit pkgs;};

        texLivePaths = with pkgs; [texLive texLiveExtra texLiveCMSuper 
	  texLiveBeamer lmodern texLiveContext];
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
    
    kernelPackages = pkgs.linuxPackagesFor baseKernel;

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
kernelToUse = (bootEntries pkgs.linux_2_6_34);

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
	utilsX widgets sound graphicView clientServer 
	misc GCPin versionControl fsTools consoleEditors
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
	misc GCPin versionControl consoleEditors]
    );

	# latest SVN or even personal uncommited version
    nix = pkgs.nixCustomFun ("" + /etc/nixos/nix + "/")
        ""
	["nix-reduce-build" "nix-http-export.cgi"]
	["--with-docbook-xsl=${pkgs.docbook5_xsl}/xml/xsl/docbook/"
	 "--with-docbook-rng=${pkgs.docbook5}/xml/rng/docbook"
	];

	# I'd like to build ISO's and have them in profile
    pathsToLink = ["/bin" "/sbin" "/share" "/man" "/info" "/iso" "/lib"];
	
	# Most typos leave needed option at default 
	# (mystically..), but create easy-to-find
	# options NixOS has no idea of. Build should break
	# on them.     
    #checkConfigurationOptions = true;
  };

  boot = rec {
    loader.grub = {
    device = "/dev/sda";

    extraGrubEntries = "
        title Windows
          chainloader (hd0,1)+1
    ";

    copyKernels = true;
	
	# I do not want to go near 
	# GRUB configuration count limit.    								
	# Look in nix-dev 
    configurationLimit = 100;
    
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
		"zlib_deflate" "unix" "usbhid" "uhci_hcd" "ehci_hcd"
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

  fileSystems = [
  	{
		mountPoint ="/";
		device="/dev/sda6";
		fsType="ext3";
		options="acl,user_xattr";
	}
  ];

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
		http://hydra.nixos.org/project/nixpkgs/channel/latest/MANIFEST
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
  ];
  
  services = import /etc/nixos/configurations/misc/raskin/services-main.nix {inherit pkgs config;};
  security = {
    extraSetuidPrograms = ["fusermount"
      "mount" "umount" "sudo" "xlaunch"
      "lsof" "suid-chroot" "fbterm"
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
    ];
  };

  hardware = {
      firmware = ["/var/lib/firmware"];
  };

  passthru = {
  };
}
