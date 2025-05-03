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
      (((llama-cpp.override {
        rocmSupport = false;
        vulkanSupport = true;
      }).overrideAttrs (x: 
      if (lib.versionAtLeast x.version "5269") then {} else {
        version = "5269";
      })).overrideAttrs (x: if x.version == "5269" then {
        src = x.src.override { 
          hash = "sha256-bz8SGtZIctonmrJlrhuOoNrZT8L0Jtb2i47DexFErto="; 
        };
      } else {}))
      ollama
      (let ov = callPackage ../ollama-vulkan.nix {}; in
       runCommand "ollama-vulkan" {} ''
         mkdir -p "$out/bin"
         for i in "${ov}/bin"/*; do 
           ln -s "$i" "$out/bin/ollama-vulkan-$(basename "$i")";
         done
       '')
      (let sd = callPackage ../stable-diffusion-cpp.nix {}; in
       runCommand "stable-diffusion-cpp" {} ''
         mkdir -p "$out/bin"
         for i in "${sd}/bin"/*; do 
           ln -s "$i" "$out/bin/stable-diffusion-cpp-$(basename "$i")";
         done
       '')
      (let 
         pp = python3Packages;
         hf-t = pp.transformers.overridePythonAttrs (x: {
          dependencies = x.dependencies ++ (with pp; [
            rich torchWithVulkan 
            (accelerate.override (y: {
              torch = torchWithVulkan;
            }))
          ]);
        });
       in
        runCommand "huggingface-transformers" {}
        ''
          mkdir -p "$out/bin"
          ln -s "${hf-t}"/bin/* "$out/bin"
          ln -s "${pp.python.withPackages (p: [
            hf-t
          ])}"/bin/python "$out/bin/python-with-transformers"
        ''
        )
      (let lv = (fetchFromGitHub {
        owner = "ggml-org";
        repo = "llama.vim";
        rev = "dafa50acc4df4fe8b173c7cbfa3c5901fb7e0dec";
        hash = "sha256-EhJjVfWAx0+q1zHFVCNJgvEe3vu4kjn5sg01pwVyehI=";
      }); in runCommand "llama.vim" {} ''
        mkdir -p "$out/share/vim"
        ln -s "${lv}" "$out/share/vim/llama.vim"
      '')
      ]      
