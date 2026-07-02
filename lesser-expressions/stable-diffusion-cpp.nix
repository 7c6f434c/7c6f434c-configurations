{ stdenv, fetchgit, cmake, git, vulkan-headers, vulkan-loader, shaderc }:
stdenv.mkDerivation (finalAttrs: {
  pname = "stable-diffusion-cpp";
  version = "0.0-unnamed-2026-06-29-master-737-3b6c9ca";

  src = fetchgit {
    url = "https://github.com/leejet/stable-diffusion.cpp";
    rev = "3b6c9ca97cfcda8e68e719e6670d06379fcbe943";
    hash = "sha256-TQLCsbHpJlpi7o8SOXh3wogCqNeOHf+SNq+tkW/977E=";
    fetchSubmodules = true;
  };

  nativeBuildInputs = [ cmake git shaderc ];
  cmakeFlags = [ "-DSD_VULKAN=ON" ];

  buildInputs = [ vulkan-headers vulkan-loader ];
})
