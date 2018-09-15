{pkgs ? import <nixpkgs> {
    inherit system;
  }, system ? builtins.currentSystem, nodejs ? pkgs."nodejs-6_x"}:

let super = import ./default.nix { inherit pkgs system nodejs; };
    args = {
    src = pkgs.fetchFromGitHub {
      owner = "matrix-org";
      repo = "node-purple";
      rev = "f5ad4ef7989046bb1c1e963a0a7510737d527eef";
      sha256 = "182hlh130ap5vjbkf1a67dj022d3254j4gqxq2lv5jl3spqicfqd";
    };
    postInstall = ''
      sed -e '/var LIBRARY_PATHS/aLIBRARY_PATHS.push("${pkgs.lib.getLib pkgs.pidgin}/lib/libpurple.so");' -i "$out"/lib/node*/*/purple.js
    '';
  };
in
{
  package = super.package.override args;
  shell = super.shell.override args;
}
