# I keep everything in configuration.nix for automated
# updates. But the list is rather big, so I split it.
# Division is relatively random.
{myTexLive , myKDE, pkgs, baseKernel, ...}: with pkgs; 
let pp = import ./private-packages.nix {inherit pkgs;}; in
rec {

	private = pp;

  nixosDefault = [
      acl attr bashInteractive bzip2 coreutils cpio curl diffutils eject
      findutils gawk glibc gnugrep gnupatch gnused gnutar gzip xz less libcap
      man nano ncurses netcat openssh pciutils perl procps rsync strace pam
      sysvtools su time usbutils utillinux glibcLocales sudo lvm2 shadow
  ];

	bootstrap = {
		extractors = [p7zip];
		programmingLanguages = [
                        bashInteractive
			zsh bash sbcl ecl clisp gcc

			pythonFull pythonPackages.pip
		];
		buildSupport = [
			libtool automake autoconf gnumake patchelf nixUnstable
		];
		utilsX = [
			xterm rxvt_unicode rxvt_unicode.terminfo xmacro
			xsel xclip xorg.xmodmap xorg.xrandr
			dmenu2 rofi xdotool xorg.xkbcomp xorg.setxkbmap
      xorg.xorgserver xkeyboard_config xorg.xprop
		];
		consoleTools = [
			which sqlite bc psmisc file slmenu
			rlwrap screen pv manpages kmod module_init_tools
		];
		debugTools = [
			gdb lsof strace
		];
		clientServer = [
			postgresql openssh wget curl
			(pkgs.squid4 or squid) fdm nbd bind
			lftp ntp
		];
		networkTools = [
			mtr dhcp wpa_supplicant iproute iputils
			ncat socat libidn iptables nettools
		];
		textCrunchers = [
			diffutils patch gnused 
		];
		hwControl = [
			fbterm iw hdparm smartmontools kbd
			pmount wavemon eudev xorg.xf86inputsynaptics
		];
		browsers = [
			firefox conkeror
		];
		editors = [
			(import ./custom-vim.nix pkgs)
		];
		versionControl = [
			git mercurial monotone subversion darcs
		];
		filesystems = [
			sshfsFuse fuse
		];
		partitionTools = [
			gptfdisk
		];
		encryption = [
			gnupg openssl
		];
		nonNative = [
			qemu OVMF
		];
	};
	constantly_used = {
		extractors = [
			unzip zip xarchive lrzip mdbtools_git runzip
		];
		programmingLanguages = [
			fpc lazarus
			asymptote
			gcc maxima guile racket
			eprover coq cvc4
			julia_05 openjdk8 apache-jena
			mono octave nim ruby
      haskellPackages.ghc gfortran
      glpk ccl go gccgo nix-repl abcl gcl

			pythonPackages.ipython
      lispPackages.clwrapper cl-launch
      lispPackages.quicklisp

      valgrind

      myTexLive
      (runCommand "texlive-fixes" {}
        ''
          mkdir -p "$out/bin"
          ln -s "${myTexLive}/bin/epstopdf" "$out/bin/repstopdf"
        '')

      love_0_10 luajit
		];
		utilsX = [
			icewm pp.stumpwm trayer jwm
			keynav x11vnc xorg.xsetroot
			xorg.xdpyinfo xorg.xdriinfo glxinfo
			(xscreensaver.override {forceInstallAllHacks = true;})
      xvidcap apacheHttpd xdg_utils fontconfig
			xcalib xorg.xwd xdaliclock xvfb_run xorg.xev
      xorg.xinput xorg.xset xorg.xauth ratpoison
      xorg.xlsclients xorg.xwininfo xorg.xkill
      myKDE.kdelibs xorg.xrdb xprintidle-ng
		];
		consoleTools = [
			remind expect pinentry fdupes mc debootstrap parallel
			texinfoInteractive /*baseKernel.kernelPackages.sysdig*/
      rhash
		];
		clientServer = [
			dict dbus wgetpaste whois
			ripmime gtkvnc tigervnc samba
      (lowPrio tightvnc)
			lighttpd nix-serve
			openvpn (youtubeDL.override {pandoc = null;})
			tftp_hpa netkittftp atftp
			telnet xinetd nginx vsftpd axel aria2
			transmission nix-prefetch-scripts
      offlineimap
			(dictDBCollector {
			 dictlist = with dictdDBs; map 
			 (x:{
			  name = x.dbName;
			  filename = x.outPath;
			  locale = x.locale;
			  })
			 [ 
			 eng2fra fra2eng eng2nld
			 nld2eng eng2rus
			 mueller_enru_abbr
			 mueller_enru_base
			 mueller_enru_dict
			 mueller_enru_geo
			 mueller_enru_names
			 ];
			 })
		];
		networkTools = [
			wireshark tcpdump
			nmap badvpn tor
		];
		consoleBrowsers = [
			elinks lynx links2
		];
		textCrunchers = [
			xxdiff /*myKDE.kdiff3*/
		];
		media = [
			mplayer /*myKDE.kmplayer*/ timidity sox lame vlc ffmpeg espeak 
      lilypond
		];
		hwControl = [
			alsaLib cups alsaUtils
			xsane udisks2 go-mtpfs
			baseKernel.kernelPackages.kernel
			pp.lcard_ltr_sdk
      multipath-tools

      (bumblebee.override { useNvidia = false; })
      (lib.overrideDerivation xorg.xf86videointel (x: 
         {
           nativeBuildInputs = x.nativeBuildInputs ++ [
             xorg.libXcomposite.dev cairo.dev xorg.libXScrnSaver
           ];
         }))

			androidenv.androidsdk_4_2
		] ++ baseKernel.extraModulePackages;
		graphicEdit = [
			inkscape gimp imagemagick7Big vue dmtx graphviz
			pdftk gnuplot openscad xfig zbar qrencode zxing
      quirc myKDE.kig drgeo potrace slic3r transfig
      povray xournal
		];
		graphicView = [
			xpdf zathura evince djvulibre fbida ghostscript
			geeqie gqview mupdf djview4
		];
		browsers = [
			conkeror firefox chromium 
			pp.slimerjs midori

			nspluginwrapper 
			icedtea7_web
		];
		editors = [
			libreoffice emacs textadept bvi
				emacsPackagesNg.slime
      neovim neovim-qt
		];
		versionControl = [
			monotoneViz gitFull fossil
		];

		monitoring = [
			htop iftop iotop powertop 
		];

		filesystems = [
      dosfstools e2fsprogs btrfsProgs cifs_utils 
      python3Packages.bedup
      inotifyTools ncdu cdrkit smbnetfs genext2fs

      ntfs3g
		];
		
		toys = [
			fortune sgtpuzzles quantumminigolf liquidwar fsg golly n2048
      xpilot-ng pkgs."2048-in-terminal" xmoto atanks lincity_ng
      blobby pioneer xsok xaos
		];

		im = [
			ii mcabber ratox
		];
		sandboxing = [
			lxc firejail
		];
    emulators = [
      wineUnstable pipelight dosbox
    ];
    nonNative = [
      /*baseKernel.kernelPackages.virtualbox*/
    ];
    fonts = (import ./fonts.nix pkgs).fonts;
    icons = [kdeFrameworks.oxygen-icons5];
    libraries = [ /*myKDE.kde_runtime*/ phonon 
      phonon-backend-gstreamer
      lispPackages.command-line-arguments
      asdf
      
      xorg.libX11 xorg.libXxf86vm mesa mesa_noglu
      xorg.libXcursor xorg.libX11 xorg.libXrandr
      ];
	};
}
