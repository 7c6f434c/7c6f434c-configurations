{ nixpkgs ? ../../../nixpkgs }:
let system = builtins.currentSystem; in
with import nixpkgs {inherit system;};
let
  config = (import ../../../nixos/lib/eval-config.nix {
    inherit system nixpkgs;
    modules = [ ./live-dvd-module.nix ];
  }).config;
in config.system.build.isoImage
