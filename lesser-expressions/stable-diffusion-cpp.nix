{ stdenv, fetchgit, cmake, git, vulkan-headers, vulkan-loader, shaderc }:
stdenv.mkDerivation (finalAttrs: {
  pname = "stable-diffusion-cpp";
  version = "0.0-unnamed-2025-03-09-master-d7c7a34";

  src = fetchgit {
    url = "https://github.com/leejet/stable-diffusion.cpp";
    tag = "master-d7c7a34";
    hash = "sha256-xFrpdVJOE5Yhvwh+ZWp0Z5QQLNahQ2e47J7K6yZPkUo=";
    fetchSubmodules = true;
  };

  nativeBuildInputs = [ cmake git shaderc ];
  cmakeFlags = [ "-DSD_VULKAN=ON" ];

  buildInputs = [ vulkan-headers vulkan-loader ];
})
