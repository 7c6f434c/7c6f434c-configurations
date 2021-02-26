
let 
NIXPKGS_env = builtins.getEnv "NIXPKGS";
pkgsPath = if NIXPKGS_env == "" then <nixpkgs> else NIXPKGS_env;
pkgs = import pkgsPath {}; in with pkgs;

let customVim = import /home/raskin/src/nix/configurations/misc/raskin/custom-vim.nix; in
let pp = import /home/raskin/src/nix/configurations/misc/raskin/private-packages.nix {inherit pkgs;}; in
let justUse = str: {name = str; path = builtins.getAttr str pkgs;}; in
let justUseMult = output: str: {name = "${str}.${output}"; path = builtins.getAttr output (builtins.getAttr str pkgs);}; in
let ppUse = str: {name = str; path = builtins.getAttr str pp;}; in
let julia_used = julia_15; in
let myLispPackages = import ./lisp-packages.nix { inherit pkgs; }; in

linkFarm "raskin-packages" ([
  { name = "hide-webdriver.xpi"; path = runCommand "hide-wedriver.xpi" {} ''
     cd ${/home/raskin/src/js/hide-webdriver}
     ${zip}/bin/zip "$out" *.*
  '';}
])
