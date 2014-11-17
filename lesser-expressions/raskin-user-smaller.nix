let 
NIXPKGS_env = builtins.getEnv "NIXPKGS";
pkgsPath = if NIXPKGS_env == "" then /etc/nixos/nixpkgs else NIXPKGS_env;
pkgs = import pkgsPath {}; in with pkgs;

let customVim = import /etc/nixos/configurations/misc/raskin/custom-vim.nix; in
let pp = import /etc/nixos/configurations/misc/raskin/private-packages.nix {inherit pkgs;}; in
let justUse = str: {name = str; path = builtins.getAttr str pkgs;}; in
let ppUse = str: {name = str; path = builtins.getAttr str pp;}; in

linkFarm "raskin-packages" ([
		{name="mime"; path=shared_mime_info;}
		]
		++ 
		(map justUse [
		"gsettings_desktop_schemas"
		])
		)
