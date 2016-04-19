let 
NIXPKGS_env = builtins.getEnv "NIXPKGS";
pkgsPath = if NIXPKGS_env == "" then <nixpkgs> else NIXPKGS_env;
pkgs = import pkgsPath {}; in with pkgs;

let customVim = import /home/raskin/src/nix/configurations/misc/raskin/custom-vim.nix; in
let pp = import /home/raskin/src/nix/configurations/misc/raskin/private-packages.nix {inherit pkgs;}; in
let justUse = str: {name = str; path = builtins.getAttr str pkgs;}; in
let justUseMult = output: str: {name = "${str}.${output}"; path = builtins.getAttr output (builtins.getAttr str pkgs);}; in
let ppUse = str: {name = str; path = builtins.getAttr str pp;}; in

linkFarm "raskin-packages" ([
		{name="mime"; path=shared_mime_info;}
    { name="query-fs"; 
      path = lib.overrideDerivation lispPackages.query-fs (x: {
        linkedSystems = x.linkedSystems ++ ["clsql" "ironclad" "esrap-peg" 
          "md5" "sb-bsd-sockets"]; 
        nativeBuildInputs = x.nativeBuildInputs ++ (with lispPackages; 
          [clsql ironclad esrap-peg md5]);
      });
    }
		{name="clx-truetype"; path=lispPackages.clx-truetype;}
		{name="clx-xkeyboard"; path=lispPackages.clx-xkeyboard;}
		{name="bordeaux-threads"; path=lispPackages.bordeaux-threads;}
		]
		++ 
		(map justUse [
		"gsettings_desktop_schemas"
		"fuse" "mysql" "openssl" "opencv" "postgresql" "sqlite"
		"sbcl_1_2_5" "graphviz_2_0"
		])
    ++
    (map (justUseMult "out") [
    "openssl"
    ])
		)
