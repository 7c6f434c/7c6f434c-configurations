{ lib, stdenv, fetchurl, pkgconfig, gtk3 }:

stdenv.mkDerivation {
  pname = "scite";
  version = "4.4.6";

  src = fetchurl {
    url = "https://www.scintilla.org/scite446.tgz";
    sha256 = "sha256:1l8nbpjvkz5d6a4yr8r4ccvnrl4p3i5klwfrhclyb8ladkp6wbhw";
  };

  nativeBuildInputs = [ pkgconfig ];
  buildInputs = [ gtk3 ];
  sourceRoot = "scintilla/gtk";

  buildPhase = ''
    make GTK3=1
    cd ../../scite/gtk
    make prefix=$out/ GTK3=1
  '';

  installPhase = ''
    make install prefix=$out/
  '';

  meta = with lib; {
    homepage = "https://www.scintilla.org/SciTE.html";
    description = "SCIntilla based Text Editor";
    license = licenses.mit;
    platforms = platforms.linux;
    maintainers = [ maintainers.rszibele ];
  };
}
