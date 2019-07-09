{pkgs ? (import <nixpkgs> {})} : 
with pkgs;
let pp = 
{
   #warmux = (warmux.override (let x = (warmux.function {}); in
   #    {
   #     src= "/home/repos/warmux/"; 
   #     phaseNames = ["JustStamp" "doAutogen"] ++ x.phaseNames;
   #     JustStamp = x.noDepEntry ''
   #       echo "${builtins.readFile "/home/repos/warmux/.svn/svn-info-output"}" &> /dev/null
   #     '';
   #     buildInputs = x.buildInputs ++ [automake autoconf libtool intltool perl];
   #     }));
   #tbe = (tbe.override (let x = (tbe.function {}); in {
   #      src = "/home/repos/tbe/";
   #      name = "tbe-svn-head";
   #      phaseNames = ["JustStamp" "doCmake"] ++ 
   #      (lib.filter
   #        (y : y!="doConfigure")
   #        x.phaseNames
   #	)
   #	++ ["postInstall"];
   #      makeFlags = ["all" "translations"];
   #      JustStamp = x.noDepEntry ''
   #         echo "${builtins.readFile "/home/repos/tbe/.svn/svn-info-output"}" &> /dev/null
   #      '';
   #      postInstall = x.fullDepEntry ''
   #        cd "$out/bin"
   #	mv tbe .tbe-wrapped
   #	echo -e '#!/bin/sh\ncd "'"$out"'"\n./bin/.tbe-wrapped "$@"' > tbe
   #	chmod a+x tbe
   #      '' ["minInit"];
   #      qt4 = qt48;
   #      buildInputs = [qt48 cmake] ++ (lib.filter (y : y.outPath != qt48.outPath) x.buildInputs);
   #    }));
  #drgeo = (drgeo.override {src="" + /home/repos/drgeo-1.1.0 + "/";});
  btrfsProgs = (lib.overrideDerivation btrfsProgs (x: rec {
    justStamp = "${builtins.readFile "${src}/.git/refs/heads/master"}";
		  src = "/home/repos/btrfs-progs/";
		  name = "btrfs-progs-git";
		  buildInputs = btrfsProgs.buildInputs ++ [
		    attr libuuid zlib acl e2fsprogs
		  ];
		}));
  uzbl = (lib.overrideDerivation uzbl (x : { 
    src = "" + /home/repos/uzbl + "/"; 
    buildInputs = x.buildInputs ++ [python3];
  }));
  grub2Bzr = (stdenv.lib.overrideDerivation grub2 (x: { 
    src = "" + /home/repos/grub; 
    buildInputs = x.buildInputs ++ [
      automake110x autoconf libtool intltool 
      autogen python
    ];
    prePatch = ''
      sh autogen.sh

      ${x.prePatch}
    '';
  }));
  #liquidwar = liquidwar.override (x: {
  #  src = ("" + /home/repos/liquidwar6 + "/");
  #  goSrcDir = ''cd liquidwar6'';
  #  phaseNames = ["setHome" "doAutotools"] ++ x.phaseNames ;
  #  buildInputs = 
  #    (lib.filter (y: y.outPath != gettext_0_18.outPath) x.buildInputs)
  #    ++ [autoconf automake libtool gettext_0_17 texinfo];
  #  setHome = builderDefs.noDepEntry ''
  #    export HOME="$PWD"
  #  '';
  #});
  # vvTest = veracity.override {
  #   runTests = true;
  # };
  gettext_expat = lib.overrideDerivation gettext (x: {
    buildInputs = x.buildInputs ++ [expat gnome.libglade];
    configureFlags = x.configureFlags ++ [ " --with-libexpat-prefix=${expat}/ " ];
  });
  #webdsl = lib.overrideDerivation pkgsi686Linux.webdsl (x : {
  #  src = "/home/repos/webdsl/";
  #  buildInputs = x.buildInputs ++ [glib libtool autoconf automake m4 ant ];
  #  preConfigure = ''
  #    echo "${builtins.readFile "/home/repos/webdsl/.svn//svn-info-output"}" &> /dev/null
  #    libtoolize
  #    ./bootstrap
  #  '';
  #});
  quarter = stdenv.mkDerivation rec {
    name = "quarter-1.0.0";
    src = fetchurl {
      url = "http://ftp.coin3d.org/coin/src/all/Quarter-1.0.0.tar.gz";
      sha256 = "06ns85plg0xwrh7p1wrfsqjz66xg8xya4kvw1fr9vp1bds2jjypd";
    };
    buildInputs = [
      qt4 coin3d
    ];
    preBuild = ''
      sed -e '1i#include <stdio.h>' -i src/Quarter/Quarter.cpp
    '';
    preConfigure = ''
      export configureFlags=" $configureFlags --with-qt-designer-plugin-path=$out/lib/qt4/plugins/designer "

      for i in $(find . -name CMakeLists.txt); do
        substituteInPlace $i \
          --replace '{QT_PLUGINS_DIR}' '{CMAKE_INSTALL_PREFIX}/lib/qt4/plugins'
      done
    '';
  };
  #ode = ode.override (x: {
  #  exportPIC = builderDefs.noDepEntry ''
  #    export NIX_CFLAGS_COMPILE="$NIX_CFLAGS_COMPILE -fPIC "
  #  '';
  #  phaseNames = ["exportPIC"] ++ x.phaseNames;
  #});
  emergent = stdenv.mkDerivation rec {
    name = "emergent-svn-head";
    src = "/home/repos/emergent/";
    justStamp = "${builtins.readFile "${src}/.svn//svn-info-output"}";
    buildInputs = [
      cmake qt4 coin3d pp.quarter readline gsl pp.ode ncurses subversion
    ];
    cmakeFlags = [
      " -DCOIN_INCLUDE_DIR=${coin3d}/include " " -DCOIN_LIB_DIR=${coin3d}/lib "
      " -DQUARTER_INCLUDE_DIR=${pp.quarter}/include " " -DQUARTER_LIB_DIR=${pp.quarter}/lib "
      " -DREADLINE_INCLUDE_DIR=${readline}/include " " -DREADLINE_LIB_DIR=${readline}/lib "
      " -DTERMCAP_INCLUDE_DIR=${ncurses}/include " " -DTERMCAP_LIBRARY=${ncurses}/lib/libncurses.so " 
    ];
    preConfigure = ''
       patchShebangs .
       find . -name '*.cpp' -exec sed -re 's@([ "])/bin/rm@\1rm@g' -i '{}' ';'
       find . -name 'configure' -exec sed -re 's@([ "])/bin/rm@\1rm@g' -i '{}' ';'
    '';
 };
 _3dc = pkgsi686Linux.stdenv.mkDerivation rec {
   name = "3dc-git-head";
   src= /home/repos/3dc;
   installPhase = ''
     mkdir -p "$out/bin"
     cp bin/3dc "$out/bin"
   '';
   buildInputs = [
     boost
   ];
   meta = {
     homepage = "https://github.com/malexw/3dc";
   };
 };
 julia = pkgs.lib.overrideDerivation julia (x: rec{
   src = "/home/repos/julia";
   JustStamp = "${builtins.readFile "${src}/.git/refs/heads/master"}";
   name = "julia-git-head";
 });
 #sbcl = pkgs.sbcl.override (rec{
 #  src = "/home/repos/sbcl/";
 #  JustStamp = "${builtins.readFile "${src}/.git/refs/heads/master"}";
 #  name = "sbcl-git-head";
 #});
#saneBackends = saneBackends.override {
#   gt68xxFirmware = x: {
#     fw = /var/lib/firmware/SBfw.usb ; 
#     name = "SBfw.usb";
#   };
#};
#saneFrontends = saneFrontends.override {
#  saneBackends = pp.saneBackends;
#};
#xsane = xsane.override {
#  saneBackends = pp.saneBackends;
#  saneFrontends = pp.saneFrontends;
#};
 lcard_ltr_sdk = pkgs.stdenv.mkDerivation rec {
   name = "lcard-ltr-sdk";
   src = /home/repos/ltr_cross_sdk;
   revision = builtins.readFile "${src}/.hg/cache/branch2-served";
   buildInputs = with pkgs; [cmake qt59.qtbase qt59.qttools libusb1 pkgconfig];
   preConfigure = ''
     sed -e '/ltr35api/d' -i ltrapi/CMakeLists.txt
     sed -e "s@ /etc/@ $out/etc/@" -i ltrd/CMakeLists.txt
     sed -e "s@ /lib/@ $out/lib/@" -i ltrd/CMakeLists.txt
   '';
   cmakeFlags = ["-DLTR_BUILD_LTRMANAGER=ON" "-DLTRD_USB_ENABLED=ON"];
 };
 slimerjs = pkgs.lib.overrideDerivation pkgs.slimerjs (x: {
   buildPhase = ''
     (
     mkdir omni.ja.unpacked
     cd omni.ja.unpacked
     unzip ../omni.ja
     patch -Np0 -i ${/home/repos/slimerjs-omni-ja-overrides/overrides.patch}
     zip ../omni.ja -r . 
     cd ..
     test -d chrome && cp -r omni.ja.unpacked/* .
     )
   '';
 });
 stumpwm = pkgs.lib.overrideDerivation pkgs.stumpwm (x: {
   nativeBuildInputs = x.nativeBuildInputs ++ [
     pkgs.stumpwm.lispPackages.clx-truetype
     pkgs.stumpwm.lispPackages.clx-xkeyboard
   ];
   installPhase = (x.installPhase or "") + ''
     echo "$nativeBuildInputs" > "$out/nix-support/build-inputs"
   '';
 });
 TannerRogalsky-demoloops = stdenv.mkDerivation rec {
   name = "TannerRogalsky-demoloops";
   src = fetchFromGitHub {
     owner = "TannerRogalsky";
     repo = "Demoloops";
     rev = "999972cc2056481175dd4c5b77e77d63bc58a00b";
     sha256 = "1vi9np77xc3lgnhkgk2dfnra0wrs1vkizih7lvpchmrd0n12h642";
     fetchSubmodules = true;
   };
   nativeBuildInputs = [cmake pkgconfig];
   buildInputs = [SDL2 SDL2_image SDL2_ttf mesa_glu openal glew
     bullet libvorbis];
   preConfigure = ''
     export BULLET_DIR="$PWD"
     export NIX_CFLAGS_COMPILE="$NIX_CFLAGS_COMPILE -I${SDL2_ttf}/include/SDL2 -I${bullet}/include/bullet"
     ln -s "${bullet}/lib/cmake/bullet/BulletConfig.cmake" BULLETConfig.cmake
     sed -e '1i#include <stdexcept>' -i lib/demoloop-lib/src/graphics/canvas.cpp
   '';
   installPhase = ''
     mkdir -p "$out"/{bin,lib,share/doc}
     cp ../{README*,LICEN?E} "$out/share/doc"
     cp loop* "$out"/bin
     cp lib/demoloop-lib/libdemoloop-lib.so "$out"/lib
   '';
   enableParallelBuilding = true;
 };
}; in 
pp
