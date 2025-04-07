let
NIXPKGS_env = builtins.getEnv "NIXPKGS";
pkgsPath = if NIXPKGS_env == "" then <nixpkgs> else NIXPKGS_env;
pkgs = import pkgsPath { 
    config = {
        allowInsecurePredicate = x: (
            (
             (
              (pkgs.lib.hasPrefix "curl-impersonate-" (x.name or x.pname))
              ||
              ("curl-impersonate" == (x.name or x.pname))
             )
             &&
             (pkgs.lib.all (y: 
                            (pkgs.lib.findFirst (z: z == y) null [
                             "CVE-2023-38545"  # socks5h long hostname heap overflow; I don't use that combo for impersonate
                             "CVE-2023-32001"  # fopen TOCTOU race condition - https://curl.se/docs/CVE-2023-32001.html
                             "CVE-2022-43551"  # HSTS bypass - https://curl.se/docs/CVE-2022-43551.html
                             "CVE-2022-42916"  # HSTS bypass - https://curl.se/docs/CVE-2022-42916.html
                            ]) != null
                           ) x.meta.knownVulnerabilities)
            ) 
            ||
            (
             x.pname == "squid"
             &&
             (
               x.version == "7.0.1"
               )
            )
        );
    };
};
allOutputNames = packages: builtins.attrNames
      (pkgs.lib.fold
        (a: b: b //
          (builtins.listToAttrs (map (x: {name = x; value = x;}) a.outputs or ["out"])))
        {} packages);
fullEnv = name: packages:
  pkgs.buildEnv {
      name = name;
      paths = packages;
      ignoreCollisions = false;
      checkCollisionContents = true;
      pathsToLink = ["/"];
      extraOutputsToInstall = (allOutputNames packages);
    };
in with pkgs;

fullEnv "main-package-set"
      [
      (symlinkJoin { name = "whisper-cpp" ; paths = [ whisper-cpp.out ]; 
                     postBuild = ''
                       rm -rf "$out/include" "$out/lib"
                     '';
                   })
      (llama-cpp.override {
        rocmSupport = false;
        vulkanSupport = true;
      })
      ollama
      ]      
