{ stdenv, fetchFromGitHub, cmake, git, vulkan-headers, vulkan-loader, shaderc }:
stdenv.mkDerivation (finalAttrs: {
  pname = "ggml";
  version = "0.9.5";
  src = fetchFromGitHub {
    owner = "ggml-org";
    repo = "ggml";
    tag = "v${finalAttrs.version}";
    hash = "sha256-lNrON8vKUJU7cxfpRKsVCIWqZj3xtkaf/Fv8zNZFN6o=";
  };
  nativeBuildInputs = [ cmake shaderc ];
  buildInputs = [ vulkan-headers vulkan-loader ];
  cmakeFlags = ["-DGGML_VULKAN=ON"];
  postPatch = ''
    sed -e 's|''${prefix}/@CMAKE_INSTALL_|@CMAKE_INSTALL_FULL_|' -i ggml.pc.in
  '';
})
