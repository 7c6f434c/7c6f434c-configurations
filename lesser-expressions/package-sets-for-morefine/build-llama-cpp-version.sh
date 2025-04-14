#! /bin/sh
nix-build -E 'with import <nixpkgs> {}; ((llama-cpp.override (x: {vulkanSupport = true;})).overrideAttrs (x: { version = "'"$1"'";})).overrideAttrs (x: { src = x.src.override {hash = "'"${2:-\${lib.fakeHash}}"'";};})' --no-out-link

