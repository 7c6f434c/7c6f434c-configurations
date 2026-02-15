let
  pkgsFun = import /etc/nixos/nixpkgs ;
  pkgsNoParams = pkgsFun {};
  crossSystem = {
      config = "armv6l-unknown-linux-gnueabi";
      bigEndian = false;
      arch = "arm";
      float = "soft";
      withTLS = true;
      libc = "glibc";
      platform = {
        name = "PocketBook";
	kernelMajor = "2.6";
        kernelArch = "arm";
	kernelHeadersBaseConfig = "s3c6400_defconfig";
      };
      uclibc = {
        extraConfig = ''
          CONFIG_ARM_OABI n
          CONFIG_ARM_EABI y
          ARCH_BIG_ENDIAN n
          ARCH_WANTS_BIG_ENDIAN n
          ARCH_WANTS_LITTLE_ENDIAN y
	  LINUXTHREADS_OLD y
        '';
      };
      openssl = {
        system = "linux-generic32";
      };
    };
  pkgs =  pkgsFun
  {
    inherit crossSystem;
    config = {
      packageOverrides = p : {
        linuxHeaders26Cross = p.forceBuildDrv
	(p.linuxHeaders_2_6_28.override {cross = crossSystem;});
	guile = p.guile_1_8;
	glibcLocales = p.glibcLocales.override {
		locales=["en_US.UTF-8/UTF-8" "ru_RU.UTF-8/UTF-8" 
		  "ru_RU.KOI8-R/KOI8-R" "ru_RU.CP1251/CP1251"
		  "C" "POSIX"
		  ];
		allLocales = false;
	};
      };
      gnutls={
        guile = false;
      };
    };
  };
  justUse = str : (builtins.getAttr str pkgs).hostDrv;
  crossWashData = pkg: ((pkgs.stdenv.mkDerivation {
    name = "${pkg.name}-washed-for";
    unpackPhase = " ";
    installPhase = ''
      cp -r "${pkg}" "$out"
    '';
  }).hostDrv);
in 
(pkgs.symlinkJoin "raskin-pb-packages" 
 (
  (map 
   justUse 
   [
   "hello" "bashInteractive"
   "guile" "strace"
   "binutils" "gdb"
   "utillinuxng" "less"
   "gnugrep" "gnused"
   "coreutils" "fakeroot"
   "tmux" "glibcLocales" "nano"
   "lsh" "openssh" "xkeyboard_config"
   ]) 
  ++
  [
    (pkgs.gcc45.hostDrv)
    (pkgs.xkbcomp.hostDrv)
    (pkgs.lib.overrideDerivation
      pkgs.tigervnc.hostDrv
      (p : {
        buildPhase = ''
	  make -C common
	'' + p.postBuild;
        installPhase = p.postInstall;
	propagatedBuildInputs = pkgs.lib.filter 
	  (x: ! (( x ? meta) && 
	    ( x.meta ? homepage) && ( x.meta.homepage == http://www.gnu.org/software/gnutls/ )))
	  p.propagatedBuildInputs;
	buildInputs = (pkgs.lib.filter 
	  (x: ! (( x ? meta) && 
	    ( x.meta ? homepage) && ( x.meta.homepage == http://www.gnu.org/software/gnutls/ )))
	  p.buildInputs) ++ [ pkgs.openssl.hostDrv ];
      })
      )
  ]
  ++ (map
        crossWashData
	pkgs.tigervnc.fontDirectories
        )
 )).hostDrv
