let 
NIXPKGS_env = builtins.getEnv "NIXPKGS";
pkgsPath = if NIXPKGS_env == "" then <nixpkgs> else NIXPKGS_env;
pkgs = import pkgsPath {};

in with pkgs;

linkFarm "raskin-packages" ([
  { name = "nixpkgs-manual"; path = import <nixpkgs/doc> {}; }
  { name = "nixos-empty-container"; path = (import <nixpkgs/nixos> {
    configuration = {
      boot.isContainer = true;
      documentation.dev.enable = true;
      documentation.nixos.enable = true;
      system.stateVersion = "99.99";
    };
  }).system; }
  { name = "nixos-empty-vm"; path = (import <nixpkgs/nixos> {
    configuration = {
      documentation.dev.enable = true;
      documentation.nixos.enable = true;
      system.stateVersion = "99.99";
    };
  }).vm; }
  { name = "python-black"; path = pkgs.python3Packages.black; }
  { name = "nixos-test"; path = (import <nixpkgs/nixos/release.nix> {}).tests.login.x86_64-linux; }
])
