{ stdenv, fetchFromGitHub, lib }:
stdenv.mkDerivation {
  pname = "obj2hmap";
  version = "0.0-unstable-2019-10-09";
  src = fetchFromGitHub {
    owner = "ryobg";
    repo = "obj2hmap";
    rev = "35fb0c78c74ecc689a72672d1a574996e7a76000";
    hash = "sha256-wWUvRdm/3UiOfjwktPPsKtAKKw9PIljRc/zv8sREXiA=";
  };

  patchPhase = ''
    sed -ie '1i#include <cstdint>' obj2hmap.cpp
    sed -ie '1i#include <cstdint>' hmap2obj.cpp
  '';

  buildPhase = ''
    runHook preBuild

    c++ obj2hmap.cpp -O2 -o obj2hmap
    c++ hmap2obj.cpp -O2 -o hmap2obj

    runHook postBuild
  '';
  
  installPhase = ''
    runHook preInstall

    mkdir -p "$out"/{bin,share/doc/obj2hmap}

    cp obj2hmap hmap2obj "$out/bin"
    cp LICENSE *.md "$out/share/doc/obj2hmap"

    runHook postInstall
  '';

  meta = {
    license = lib.licenses.gpl3Plus;
    maintainers = with lib.maintainers; [ raskin ];
    platforms = with lib.platforms; unix;
  };
}
