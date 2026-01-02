with import ../env-defs.nix;
with pkgs;

let 
  shaderc = if (lib.versionAtLeast pkgs.shaderc.version "2025.4")
    then pkgs.shaderc else (callPackage ../shaderc-2025_4.nix {});
in

fullEnv "main-package-set"
      [
      (symlinkJoin { name = "whisper-cpp" ; paths = [ whisper-cpp.out ]; 
                     postBuild = ''
                       rm -rf "$out/include" "$out/lib"
                     '';
                   })
      shaderc
      (let llama-target-version= "0" /*"7134"*/; in
      ((llama-cpp.override {
        rocmSupport = false;
        vulkanSupport = true;
        inherit shaderc;
      }).overrideAttrs (x: 
      if (lib.versionAtLeast x.version llama-target-version) then {} else {
        version = llama-target-version;
        src = x.src.override {
          tag = "b" + llama-target-version;
          hash = "sha256-eIvYSPvbxIopuRg0Wzq01hGN1XxLc2FiqqacQDo3Q/Q="; 
        };
        patches = [];
      })))
      ollama
      /*(let ov = callPackage ../ollama-vulkan.nix {}; in
       runCommand "ollama-vulkan" {} ''
         mkdir -p "$out/bin"
         for i in "${ov}/bin"/*; do 
           ln -s "$i" "$out/bin/ollama-vulkan-$(basename "$i")";
         done
       '')*/
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
      (let w = whisper-cpp; in 
        runCommand "whisper-cpp" {} ''
          mkdir -p "$out/bin"
          ln -s "${w}"/bin/* "$out/bin"
        '')
      upscayl-ncnn
      (python3Packages.mistral-common.overridePythonAttrs (x: {
        dependencies = x.dependencies ++ (with python3Packages; [
          click uvicorn fastapi pydantic pydantic-settings pycountry
          huggingface-hub
        ]);
      }))
      llama-swap
      ]
