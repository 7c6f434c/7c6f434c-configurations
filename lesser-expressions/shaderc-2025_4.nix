{
  lib,
  stdenv,
  fetchFromGitHub,
  cmake,
  python3,
  cctools,
}:
# Like many google projects, shaderc doesn't gracefully support separately
# compiled dependencies, so we can't easily use the versions of glslang and
# spirv-tools used by vulkan-loader. Exact revisions are taken from
# https://github.com/google/shaderc/blob/known-good/known_good.json

# Future work: extract and fetch all revisions automatically based on a revision
# of shaderc's known-good branch.
let
  glslang = fetchFromGitHub {
    owner = "KhronosGroup";
    repo = "glslang";
    # No corresponding tag for efd24d75bcbc55620e759f6bf42c45a32abac5f8 on 2025-06-23
    rev = "d213562e35573012b6348b2d584457c3704ac09b";
    hash = "sha256-tXmGSWqqBE2jEO3UohyjJfbriI2N+lWqCgC8j/yldus=";
  };
  spirv-tools = fetchFromGitHub {
    owner = "KhronosGroup";
    repo = "SPIRV-Tools";
    rev = "19042c8921f35f7bec56b9e5c96c5f5691588ca8";
    hash = "sha256-aUk2hcUhMRKBjmnhA3E1xko2TngLrvwqOnEZuvqSI7Q=";
  };
  spirv-headers = fetchFromGitHub {
    owner = "KhronosGroup";
    repo = "SPIRV-Headers";
    # No corresponding tag for 2a611a970fdbc41ac2e3e328802aed9985352dca on 2025-06-19
    rev = "01e0577914a75a2569c846778c2f93aa8e6feddd";
    hash = "sha256-gewCQvcVRw+qdWPWRlYUMTt/aXrZ7Lea058WyqL5c08=";
  };
in
stdenv.mkDerivation (finalAttrs: {
  pname = "shaderc";
  version = "2025.4";

  outputs = [
    "out"
    "lib"
    "bin"
    "dev"
    "static"
  ];

  src = fetchFromGitHub {
    owner = "google";
    repo = "shaderc";
    rev = "v${finalAttrs.version}";
    hash = "sha256-7RGcQAQZCKgyUl6G1U41ikoT0l5j7rWPUrAqPVZ8lSI=";
  };

  postPatch = ''
    cp -r --no-preserve=mode ${glslang} third_party/glslang
    cp -r --no-preserve=mode ${spirv-tools} third_party/spirv-tools
    ln -s ${spirv-headers} third_party/spirv-tools/external/spirv-headers
    patchShebangs --build utils/
  '';

  nativeBuildInputs =
    [
      cmake
      python3
    ];

  postInstall = ''
    moveToOutput "lib/*.a" $static
  '';

  cmakeFlags = [ "-DSHADERC_SKIP_TESTS=ON" ];

  # Fix the paths in .pc, even though it's unclear if all these .pc are really useful.
  postFixup = ''
    substituteInPlace "$dev"/lib/pkgconfig/*.pc \
      --replace-fail '=''${prefix}//' '=/' \
      --replace-fail "$dev/$dev/" "$dev/"
  '';

  meta = {
    description = "Collection of tools, libraries and tests for shader compilation";
    inherit (finalAttrs.src.meta) homepage;
    license = lib.licenses.asl20;
    platforms = lib.platforms.all;
  };
})
