{ stdenv, fetchgit, cmake, git, vulkan-headers, vulkan-loader, shaderc }:
stdenv.mkDerivation (finalAttrs: {
  pname = "stable-diffusion-cpp";
  version = "0.0-unnamed-2026-01-23-master-fa61ea7";

  src = fetchgit {
    url = "https://github.com/leejet/stable-diffusion.cpp";
    rev = "fa61ea744d1a87fa26a63f8a86e45587bc9534d6";
    hash = "sha256-6CtcA4DEwhjiJuJZe0cB0Z13WHDbMrYk7jVf5ixsPiA=";
    fetchSubmodules = true;
  };

  nativeBuildInputs = [ cmake git shaderc ];
  cmakeFlags = [ "-DSD_VULKAN=ON" ];

  buildInputs = [ vulkan-headers vulkan-loader ];
})
