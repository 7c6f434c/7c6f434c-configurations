# I keep everything in configuration.nix for automated
# updates. But the list is rather big, so I split it.
# Division is relatively random.
{myTexLive , myKDE, pkgs, baseKernel, ...}: with pkgs; 
let pp = import ./private-packages.nix {inherit pkgs;}; in
rec {

	private = pp;

	bootstrap = {
		extractors = [p7zip];
		programmingLanguages = [
			zsh bash sbcl ecl clisp gcc

			pythonFull pythonPackages.pip
		];
		buildSupport = [
			libtool automake autoconf gnumake patchelf
		];
		utilsX = [
			xlaunch xterm rxvt_unicode xmacro
			xsel xclip xorg.xmodmap xorg.xrandr
			dmenu2 xdotool
		];
		consoleTools = [
			which sqlite bc psmisc file utillinuxCurses
			rlwrap screen pv manpages
		];
		debugTools = [
			gdb lsof strace
		];
		clientServer = [
			postgresql openssh wget curl
			squids.latest fdm nbd bind
			lftp ntp
		];
		networkTools = [
			mtr dhcp wpa_supplicant
			ncat socat libidn iptables 
		];
		textCrunchers = [
			diffutils patch gnused 
		];
		hwControl = [
			fbterm iw hdparm smartmontools
			pmount wavemon
		];
		browsers = [
			firefox-bin conkeror
		];
		editors = [
			(import ./custom-vim.nix pkgs)
		];
		versionControl = [
			git mercurial monotone subversion darcs
		];
		filesystems = [
			sshfsFuse
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
			unzip zip xarchive
		];
		programmingLanguages = [
			fpc lazarus 
			asymptote myTexLive
			gcc maxima guile 
			eprover
			julia icedtea7_jdk apache-jena
			nox

			pythonPackages.ipython
		];
		utilsX = [
			icewm stumpwm trayer 
			keynav x11vnc xorg.xsetroot
			xorg.xdpyinfo xorg.xdriinfo glxinfo
			xscreensaver xvidcap
			xcalib xorg.xwd xdaliclock
		];
		consoleTools = [
			remind expect pinentry fdupes mc
		];
		clientServer = [
			dict 
			ripmime gtkvnc tigervnc samba
			lighttpd nix-binary-cache
			openvpn youtubeDL
			tftp_hpa netkittftp atftp 
			telnet xinetd 
			transmission
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
			nmap badvpn
		];
		consoleBrowsers = [
			elinks lynx links2
		];
		textCrunchers = [
			xxdiff myKDE.kdiff3
		];
		media = [
			mplayer myKDE.kmplayer timidity sox lame vlc ffmpeg
		];
		hwControl = [
			alsaLib cups
			pp.xsane udisks xlaunch
			baseKernel.kernelPackages.kernel
			pp.lcard_ltr_sdk

			androidenv.androidsdk_4_2
		] ++ baseKernel.extraModulePackages;
		graphicEdit = [
			inkscape gimp imagemagick vue dmtx graphviz
			pdftk gnuplot openscad xfig
		];
		graphicView = [
			xpdf zathura evince djvulibre fbida ghostscript
			geeqie gqview mupdf djview4
		];
		browsers = [
			conkeror firefox-bin chromium 
			slimerjs

			nspluginwrapper 
			icedtea7_web
		];
		editors = [
			libreoffice
		];
		versionControl = [
			monotoneViz gitFull 
		];

		monitoring = [
			htop iftop iotop powertop 
		];

		filesystems = [

			inotifyTools ncdu cdrkit
		];
		
		toys = [
			fortune sgtpuzzles quantumminigolf liquidwar fsg golly
		];

		im = [
			gajim ii
		];


	};
}
