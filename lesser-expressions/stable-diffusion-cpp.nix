{ stdenv, fetchgit, cmake, git, vulkan-headers, vulkan-loader, shaderc }:
stdenv.mkDerivation (finalAttrs: {
  pname = "stable-diffusion-cpp";
  version = "0.0-unnamed-2026-05-16-master-bd17f53";

  src = fetchgit {
    url = "https://github.com/leejet/stable-diffusion.cpp";
    rev = "b3d56d0ba1bd437886079e339118e8e75bb79ee7";
    hash = "sha256-vdILCnrpRXg/cXoasENjjTTjpN3Z0B+6pSmmLKsKNzM=";
    fetchSubmodules = true;
  };

  nativeBuildInputs = [ cmake git shaderc ];
  cmakeFlags = [ "-DSD_VULKAN=ON" ];

  buildInputs = [ vulkan-headers vulkan-loader ];
})
