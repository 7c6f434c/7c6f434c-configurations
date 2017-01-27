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
    ${self.lib.optionalString (self.nixRemoteMachines != []) ''
      ln -s "${self.nixMachinesConf}" "$out/nix-machines.conf"
    ''}
    ${self.lib.optionalString (self.nixSigningKey != null) ''
      ln -s "${self.nixSigningKey}" "$out/"
      ln -s "${self.nixSigningKey}.pub" "$out/"
    ''}
  '';
  nixRemoteMachines = [];
  nixMachinesConf = self.pkgs.writeText "nix-machines.conf"
    (self.lib.concatMapStrings
     (machine:
      "${machine.sshUser}@${machine.hostName} "
      + (if machine ? system then machine.system else 
                self.lib.concatStringsSep "," machine.systems)
      + " ${machine.sshKey} ${toString machine.maxJobs} "
      + (if machine ? speedFactor then toString machine.speedFactor else "1" )
      + " "
      + (if machine ? supportedFeatures then self.lib.concatStringsSep "," 
                machine.supportedFeatures else "" )
      + " "
      + (if machine ? mandatoryFeatures then self.lib.concatStringsSep "," 
                machine.mandatoryFeatures else "" )
      + "\n"
     ) self.nixRemoteMachines);
  nixRemoteBuildEnv = ''
    export NIX_BUILD_HOOK=${
        self.nix.bin or self.nix.out or self.nix
      }/libexec/nix/build-remote.pl
    export NIX_CURRENT_LOAD=/run/nix/current-load
    export NIX_REMOTE_SYSTEMS=/etc/nix/nix-machines.conf
  '';
  nixSigningKey = null;
}
