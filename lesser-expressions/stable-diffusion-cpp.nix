{ stdenv, fetchgit, cmake, git, vulkan-headers, vulkan-loader, shaderc }:
stdenv.mkDerivation (finalAttrs: {
  pname = "stable-diffusion-cpp";
  version = "0.0-unnamed-2026-01-23-master-fa61ea7";

  src = fetchgit {
    url = "https://github.com/leejet/stable-diffusion.cpp";
    rev = "359eb8b8de6b68b9025b9204085feb300174284a";
    hash = "sha256-L4mq2CAunsuSHUlVxLXqvnEdnWjjm/A47rzySWniNpY=sha256-6CtcA41EwhjiJuJZe0cB0Z13WHDbMrYk7jVf5ixsPiA=";
    fetchSubmodules = true;
  };

  nativeBuildInputs = [ cmake git shaderc ];
  cmakeFlags = [ "-DSD_VULKAN=ON" ];

  buildInputs = [ vulkan-headers vulkan-loader ];
})
