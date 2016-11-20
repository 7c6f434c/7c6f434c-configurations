self: {
  nixBuildMaxJobs = "1";
  nixBuildCores = "1";
  nixBuildUseSandbox = "true";
  nixBuildSandboxPaths = [
    "/bin/sh=${self.pkgs.bash}/bin/sh"
    self.pkgs.bash
    self.pkgs.glibc
  ];
  nixGcKeepOutputs = "false";
  nixBinaryCacheKeys = [
    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
  ];
  nixBinaryCaches = ["https://cache.nixos.org/"];
  nixConfText = ''
    build-users-group = nixbld
    build-max-jobs = ${self.nixBuildMaxJobs}
    build-cores = ${self.nixBuildCores}
    build-use-sandbox = ${self.nixBuildUseSandbox}
    build-sandbox-paths = ${self.lib.concatStringsSep " " self.nixBuildSandboxPaths}
    gc-keep-derivations = true
    gc-keep-outputs = ${self.nixGcKeepOutputs}
    binary-cache-public-keys = ${self.lib.concatStringsSep " " self.nixBinaryCacheKeys}
    binary-caches = ${self.lib.concatStringsSep " " self.nixBinaryCaches}
  '';
  nixConf = self.pkgs.writeText "nix.conf" self.nixConfText;
  etcNix = self.pkgs.runCommand "etc-nix" {} ''
    mkdir "$out"
    ln -s "${self.nixConf}" "$out/nix.conf"
  '';
}
