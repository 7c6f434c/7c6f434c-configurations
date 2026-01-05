{ stdenv, fetchgit, fetchFromGitHub, git, cmake, shaderc, vulkan-headers, vulkan-loader, SDL2, libGL, libX11, 
ggml ? import ./ggml.nix { inherit stdenv fetchFromGitHub cmake shaderc vulkan-headers vulkan-loader git; }
}:
stdenv.mkDerivation (finalAttrs: {
  pname = "sam-cpp";
  version = "0.0.0-pre-untagged-2023-10-24";
  src = fetchgit {
    url = "https://github.com/YavorGIvanov/sam.cpp";
    rev = "81002818eb0e2cb3b9a523286b067f80f8424431";
    hash = "sha256-hmcXnb/TeX7S+qZz8DwJtjgL0/HcxwnGyv5tp2t2Vns=";
    # url = "https://github.com/andriiryzhkov/sam.cpp";
    # rev = "6580f87577ad20e04c653f9ca5e4be52f078e46b";
    # hash = "sha256-mSZ2xjvh7MwqXiFO9B2m6CpjQIdb5Hz08Zt7vkhm1yc=";
    fetchSubmodules = true;
  };
  postPatch = ''
    sed -e 's/cmake_minimum_required *(VERSION .*/cmake_minimum_required(VERSION 3.10)/' -i CMakeLists.txt ggml/CMakeLists.txt
  '';
  buildInputs = [ vulkan-headers vulkan-loader SDL2 libGL libX11 ];
  nativeBuildInputs = [ cmake shaderc git ];
  env = {
    NIX_CFLAGS_COMPILE=" -std=gnu++17 -std=gnu17 -Wno-error=strict-prototypes ";
  };
  postInstall = ''
    cp bin/lib*.so "$out"/lib
    for i in "$out"/lib/lib*.so; do
      echo "$i"
      patchelf --set-rpath "$(
        patchelf --print-rpath "$i" | sed -e 's@^:*@@; s@^/build/[^:]*:@@' | tee /dev/stderr
      )" "$i"
      echo
    done
  '';
})
