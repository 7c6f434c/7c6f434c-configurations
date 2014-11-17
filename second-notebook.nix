/* 
Configuration for NixOS used by Michael Raskin
*/

{pkgs, config, modulesPath, ...}:
let 
base = 
{
  require = [
    ];

  environment = {
    systemPackages = with pkgs;
    [

     openvpn gtkvnc firefox libreoffice icewm kde4.kdegames openssh
     xarchiver vimHugeX gimp scite tigervnc

    p7zip unzip zip unrar bzip2 gzip evince 

    udisks pmount

    xlaunch zsh mtr subversion git screen nmap ncat tcpdump
    sudo monotone

    ];

    pathsToLink = ["/"];
	
  };

  boot = rec {
    loader.grub = {
    device = "/dev/sda";

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
        "ath5k" "ath9k" "tun"
	"hid-logitech-dj"
	];

    kernelPackages =  pkgs.linuxPackages;

    initrd = {
	kernelModules = [
		"libata" "sd_mod" 
		"i8042" "pcips2" "serio"  
		"mousedev" "evdev" "psmouse" "sermouse" "synaptics_i2c" 
		"ext3" "lzf" "crc32c" "btrfs" "libcrc32c" 
		"zlib_deflate" "unix" "usbhid" "ehci_hcd" "uhci_hcd" 
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
  { mountPoint = "/";
	  device = "/dev/sda5";
	  fsType = "ext3";

	# Enable POSIX Acess Control Lists and user-set
	# extended attributes in user.* namespace
	  options = "acl,user_xattr";
  }
  { mountPoint = "/boot";
	  device = "/dev/sda1";
	  neededForBoot = true;
  }
  { mountPoint = "/nix/store";
	  device = "/dev/sda11";
	  neededForBoot = true;
	  fsType = "btrfs";
	  options = "noatime";
  }
  { mountPoint = "/home";
	  device = "/dev/sda7";
	  options = "acl,user_xattr";
	  neededForBoot = true;
  }
  { mountPoint = "/tmp";
	  device = "/dev/sda10";
	  fsType = "btrfs";
	  neededForBoot = true;
	  options = "noatime";
  }
  { mountPoint = "/dev/shm";
	  device = "memory";
	  fsType = "tmpfs";
  }
  ]
  ;

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

    wireless.enable = true;

    nameservers = [
    ] ++ (import /root/nix-sysconfig/nameservers.nix);

  };

  swapDevices = [
    { device = "/dev/sda8"; }
  ];
  
  services = {
	  xserver = {
		  enable = true;	

		# Do not run on startup
		  autorun = true;							

		displayManager = {
		  slim = {
		    autoLogin = true;
		    defaultUser = "raskin";
		  };
		  desktopManagerHandlesLidAndPower=false;
		};

		desktopManager = {
		  xfce.enable = true;
		  xterm.enable = false;
		};

		# Create /etc/X11/xorg.conf for convenience
		  exportConfiguration = true;					

		# Needed by, say, xmove
		  enableTCP = true;							

		# WXGA. 15.4" widescreen
		  resolutions = [{x = 1280; y=800;} {x=1920; y=1080;}];
		  virtualScreen = {x=3072; y=1280;};

		  synaptics = {
			  enable = true;
			  dev = null;
		  };

		# I get Caps-toggle between Latin/Cyrillic/Greek
		# Right Flag key allows entering, say, euro sign
		# or accents (using deadkeys in level 3)
		# Look xkeyboard_config package source.               
		  layout = "us,ru(winkeys)";			
		  xkbOptions = "grp:alt_shift_toggle, grp_led:caps, terminate:ctrl_alt_bksp";	

		  defaultDepth = 24;
		# Will watch free ATi drivers progress...
		# Force DRI on - or do not..
		  deviceSection = ''
			  Option "RenderAccel" "true"
			  Option "AccelMethod" "EXA"
			  '';

		  serverLayoutSection = ''
			  Option "AIGLX" "true"
		#InputDevice  "Mouse[0]"
			  ''; 

	  }
	  ;

	  openssh = {
		  enable = true;
	  };


	  ntp = {
		  enable = true;
	  };
	  printing = {
		  enable = true;
                  drivers = [
			  pkgs.hplip
			  pkgs.foo2zjs pkgs.foomatic_filters 
				];
	  };
	  mingetty = {
		  helpLine = ''
			  0123456789 !@#$%^&*() -=\_+|
			  abcdefghijklmnopqrstuvwxyz
			  ABCDEFGHIJKLMNOPQRSTUVWXYZ
			  []{};:'",./<>?~`
			  '';
	  };
	  gpm = {
		  enable = true;
	  };
	  cron = {
		  systemCronJobs = [
			  "0 0-23/6 * * * rm /var/log/gw6c.*.log"
			  ];
	  };

	  atd = {
		  allowEveryone = true;
	  };

	  nixosManual.enable = false;

	  avahi = {
		  enable = true;
		  hostName = "401a0bf1";
	  };

	  openvpn.enable = true;
	  openvpn.servers = {
            work = {
		config=''

dev tun
persist-tun
persist-key
cipher AES-128-CBC
auth SHA1
tls-client
client
resolv-retry infinite
remote 195.178.216.103 1194 udp
lport 0
verify-x509-name "mccme-gw1 Server Cert." name
pkcs12 /root/nix-sysconfig/work-ovpn/mccme-gw1-udp-1194-raskina.p12
tls-auth /root/nix-sysconfig/work-ovpn/mccme-gw1-udp-1194-raskina-tls.key 1
ns-cert-type server
comp-lzo

auth-user-pass  /root/nix-sysconfig/work-ovpn/mccme-gw1-udp-1194-raskina-config.pass

	'';
    };
          };
  }
  ;
  security = {
    setuidPrograms = ["fusermount"
      "mount" "umount" "sudo" "xlaunch"
      "lsof" "suid-chroot" "fbterm" "pmount"
      "pumount" "udisks"
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
		/etc/sudo-scripts/chmod,\\
		/run/current-system/sw/bin/systemctl restart openvpn-work,\\
		/run/current-system/sw/bin/systemctl stop openvpn-work

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

  fonts = {
	  enableGhostscriptFonts = true;

	# So there is a big directory in current-system 
	# with all fonts.
	  enableFontDir = true;

	  extraFonts = ([
			  (pkgs.ghostscript + "/share/ghostscript/fonts/")
			  pkgs.xorg.fontcronyxcyrillic
			  pkgs.xorg.fontadobe100dpi
			  pkgs.xorg.fontadobe75dpi
			  pkgs.xorg.fontxfree86type1
			  pkgs.mph_2b_damase
			  pkgs.dejavu_fonts
			  pkgs.liberation_ttf
			  pkgs.unifont
			  pkgs.wqy_zenhei
			  pkgs.clearlyU
			  pkgs.ucsFonts
			  pkgs.junicode
			  pkgs.lmodern
			  (pkgs.libertine.passthru.function {
			   createTTF = true;
			   createPFM = false;
			   createPFB = false;
			   createAFM = false;
			   createMAP = false;
			   createENC = false;
			   })
	  pkgs.arkpandora_ttf
		  pkgs.andagii
		  ]);

	  enableCoreFonts = false;
  };

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
  };

  hardware = {
      firmware = ["/var/lib/firmware"];
      opengl.videoDrivers = ["ati" "intel"];
  };


  nixpkgs.config = import (builtins.getEnv "NIXPKGS_CONFIG");
};

in 
	# Everything above is building my base configuration.
	# But I have multi-boot..
base 
