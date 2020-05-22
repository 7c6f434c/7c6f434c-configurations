let 
NIXPKGS_env = builtins.getEnv "NIXPKGS";
pkgsPath = if NIXPKGS_env == "" then <nixpkgs> else NIXPKGS_env;
pkgs = import pkgsPath {};
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

linkFarm "raskin-toy-packages" ([
  { name = "main-toy-package-set";
    path = (fullEnv "main-heavy-package-set"
      [
        fsg kobodeluxe extremetuxracer golly lincity construo
        sgtpuzzles xconq pysolfc xaos _2048-in-terminal blobby
        xpilot-ng liquidwar
      ]);}
])

