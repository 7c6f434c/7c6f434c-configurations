{ stdenv, fetchgit, cmake, git, vulkan-headers, vulkan-loader, shaderc }:
stdenv.mkDerivation (finalAttrs: {
  pname = "stable-diffusion-cpp";
  version = "0.0-unnamed-2025-03-09-master-d7c7a34";

  src = fetchgit {
    url = "https://github.com/leejet/stable-diffusion.cpp";
    rev = "8823dc48bcc1598eb9671da7b69e45338d0cc5a5";
    hash = "sha256-ZqjbU7lL0gkU81qgU7ndksbiuqc8yesEjolHVWUflb4=";
    fetchSubmodules = true;
  };

  nativeBuildInputs = [ cmake git shaderc ];
  cmakeFlags = [ "-DSD_VULKAN=ON" ];

  buildInputs = [ vulkan-headers vulkan-loader ];
})
