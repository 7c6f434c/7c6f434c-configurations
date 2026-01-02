{ mesa, fetchFromGitLab, lib, cmake, libvdpau }:
let
  version = "25.2.6";
in
(mesa.overrideAttrs (x: {
  inherit version;
  src = fetchFromGitLab {
    domain = "gitlab.freedesktop.org";
    owner = "mesa";
    repo = "mesa";
    rev = "mesa-${version}";
    hash = "sha256-erfvNR1Yor3k8gVVClyiBJsgyaOv7BMFd1NSgieTY5I=";
  };
  patches = (lib.filter (x: (baseNameOf x == "opencl.patch")) 
  mesa.patches);
  outputs = (lib.filter (x: x!= "spirv2dxil") x.outputs);
  nativeBuildInputs = x.nativeBuildInputs ++ [ cmake ];
  buildInputs = x.buildInputs ++ [ libvdpau ];
})).override
  {
    galliumDrivers = [
      "i915" "crocus" "iris"
      "llvmpipe" "softpipe"
      "nouveau"
      "radeonsi"
      "virgl"
      "zink"
    ];
    vulkanDrivers = [
      "amd"
      "intel_hasvk" "intel"
      "nouveau"
      "swrast"
      "virtio"
    ];
    vulkanLayers = [
      "device-select"
      "intel-nullhw"
      "overlay"
      "screenshot"
      "vram-report-limit"
    ];
  }
