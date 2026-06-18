with import ./env-defs.nix;
with pkgs;
fullEnv "js-package-set" ([ nodejs emscripten opencode pi-coding-agent ])
