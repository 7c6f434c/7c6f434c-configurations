{ stdenv, fetchgit, cmake, git, vulkan-headers, vulkan-loader, shaderc }:
stdenv.mkDerivation (finalAttrs: {
  pname = "stable-diffusion-cpp";
  version = "0.0-unnamed-2026-05-16-master-bd17f53";

  src = fetchgit {
    url = "https://github.com/leejet/stable-diffusion.cpp";
    rev = "bd17f53b7386fb5f60e8587b75e73c4b2fed3426";
    hash = "sha256-4s7JtUOtST8SS4trvVZXAtHoq1/ktAkydqLSQlwqU74=";
    fetchSubmodules = true;
  };

  nativeBuildInputs = [ cmake git shaderc ];
  cmakeFlags = [ "-DSD_VULKAN=ON" ];

  buildInputs = [ vulkan-headers vulkan-loader ];
})
