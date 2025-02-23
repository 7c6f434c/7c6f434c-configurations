let 
NIXPKGS_env = builtins.getEnv "NIXPKGS";
pkgsPath = if NIXPKGS_env == "" then <nixpkgs> else NIXPKGS_env;
pkgs = import pkgsPath {
  config = {
    allowInsecurePredicate = x: (
      ("SDL_ttf" == x.pname)
      &&
      (pkgs.lib.all
      (y: pkgs.lib.findFirst (z: z==y) null [
        "CVE-2022-27470" # Corrupted font file issue
      ] != null)
      x.meta.knownVulnerabilities
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

linkFarm "raskin-toy-packages" ([
  { name = "main-toy-package-set";
    path = (fullEnv "main-heavy-package-set"
      [
        fsg kobodeluxe extremetuxracer 
        golly
        lincity construo
        sgt-puzzles xconq pysolfc xaos _2048-in-terminal blobby
        xpilot-ng liquidwar
        quantumminigolf liquidwar5 xmoto
        (renpy.override { ffmpeg = ffmpeg_6; })
        (runCommandNoCC "rpatool" {} ''
          mkdir -p "$out"/bin
          cp "${(fetchFromGitHub {
            owner = "shizmob";
            repo = "rpatool";
            rev = "74f26d5dfdd645483e02552aa766ca447ad6b191";
            sha256 = "sha256-S4aX+bDy3Gu5so1V2tq2kRhfmjT888hCI6g2MNVCtYE=";
          })}"/rpatool "$out/bin"
          chmod a+x "$out/bin/rpatool"
        '')
      ]);}
])

