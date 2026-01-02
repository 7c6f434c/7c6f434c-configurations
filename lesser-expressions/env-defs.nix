rec {
  NIXPKGS_env = builtins.getEnv "NIXPKGS";
  pkgsPath = if NIXPKGS_env == "" then <nixpkgs> else NIXPKGS_env;

  pkgs = import pkgsPath {
    config = {
      allowInsecurePredicate =
        x:
        (
          (
            (
              (pkgs.lib.hasPrefix "curl-impersonate-" (x.name or x.pname))
              || ("curl-impersonate" == (x.name or x.pname))
            )
            && (pkgs.lib.all (
              y:
              (pkgs.lib.findFirst (z: z == y) null [
                "CVE-2023-38545" # socks5h long hostname heap overflow; I don't use that combo for impersonate
                "CVE-2023-32001" # fopen TOCTOU race condition - https://curl.se/docs/CVE-2023-32001.html
                "CVE-2022-43551" # HSTS bypass - https://curl.se/docs/CVE-2022-43551.html
                "CVE-2022-42916" # HSTS bypass - https://curl.se/docs/CVE-2022-42916.html
              ]) != null
            ) x.meta.knownVulnerabilities)
          )
          || (x.pname == "squid" && (x.version == "7.0.1"))
          || (
            ("SDL_ttf" == x.pname)
            && (pkgs.lib.all (
              y:
              pkgs.lib.findFirst (z: z == y) null [
                "CVE-2022-27470" # Corrupted font file issue
              ] != null
            ) x.meta.knownVulnerabilities)
          )
          || (
            ("ecdsa" == x.pname)
            && (pkgs.lib.all (
              y:
              pkgs.lib.findFirst (z: z == y) null [
                "CVE-2024-23342" # Timing attack
              ] != null
            ) x.meta.knownVulnerabilities)
          )
        );
    };
  };
  allOutputNames =
    packages:
    builtins.attrNames (
      pkgs.lib.foldr (
        a: b:
        b
        // (builtins.listToAttrs (
          map (x: {
            name = x;
            value = x;
          }) a.outputs or [ "out" ]
        ))
      ) { } packages
    );
  fullEnv =
    name: packages:
    pkgs.buildEnv {
      name = name;
      paths = packages;
      ignoreCollisions = false;
      checkCollisionContents = true;
      pathsToLink = [ "/" ];
      extraOutputsToInstall = (allOutputNames packages);
    };
  justUse = str: {
    name = str;
    path = builtins.getAttr str pkgs;
  };
  justUseMult = output: str: {
    name = "${str}.${output}";
    path = builtins.getAttr output (builtins.getAttr str pkgs);
  };
  justUseMultAll = str: pkgs.lib.map (n: justUseMult n str) (builtins.getAttr str pkgs).outputs;
  pack =
    path:
    pkgs.runCommand "source.tar.gz" { } ''
      cd ${builtins.storeDir}
      tar -cvzf "$out" "$(basename "${path}")"
    '';
  wrapperEnv =
    package:
    pkgs.buildEnv {
      name = package.name;
      paths = [ package ];
      ignoreCollisions = false;
      checkCollisionContents = false;
      pathsToLink = [ "/" ];
      extraOutputsToInstall = [ ];
    };
    linkFarm = pkgs.linkFarm;
    callPackage = pkgs.callPackage;
}
