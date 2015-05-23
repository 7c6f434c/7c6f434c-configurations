let 
NIXPKGS_env = builtins.getEnv "NIXPKGS";
pkgsPath = if NIXPKGS_env == "" then <nixpkgs> else NIXPKGS_env;
pkgs = import pkgsPath {}; in with pkgs;
let customVim = import ../custom-vim.nix; in
let pp = import ../private-packages.nix {inherit pkgs;}; in
let justUse = str: {name = str; path = builtins.getAttr str pkgs;}; in
let ppUse = str: {name = str; path = builtins.getAttr str pp;}; in

linkFarm "raskin-packages" ([
		{name="hg"; path=mercurial;}
		{name="mime"; path=shared_mime_info;}
		{name="vim"; path=customVim pkgs;}
		{name="pandas"; path=pythonPackages.pandas;}
		{name="libX11"; path=xorg.libX11;}
		{name="gtkglext"; path=gnome.gtkglext;}
		] ++ (map justUse [
			"openssl" 
			"graphviz_2_0" "pipelight"
			"libfixposix" "opencv"
			"gmp" "mpfr" "fuse" "libffi"
			"gsettings_desktop_schemas"

			"cairo" "mesa" "freeglut" "postgresql" 
			"sqlite" "mysql" "python" "libuuid"
			"libev" "gtk" "glib" "pango" "gsl" 
			"ncurses"

			"zlib"
			])
		++
		(map ppUse [])
		++ 
		[
		{name="dbus-services"; path=(symlinkJoin "dbus-services" 
		  []);
		}
                {
		  name="glib-schemas"; 
		  path=(runCommand "glib-schemas"
				  {
				  inherit glib;
				  schemasFiles = 
				  (symlinkJoin "glib-schemas-raw" 
				   [
				   pkgs.gtk3
				   pkgs.gsettings_desktop_schemas
				   ]);
				  }
				  ''
				    mkdir -p "$out"/share/glib-2.0/schemas
				    ln -s "$schemasFiles"/share/glib-2.0/schemas/*.xml  "$out"/share/glib-2.0/schemas
				    $glib/bin/glib-compile-schemas "$out"/share/glib-2.0/schemas
				  '');
		}
		{name="nix-serve"; path=(import /home/repos/nix-serve);}
    { name="query-fs"; 
      path = lib.overrideDerivation lispPackages.query-fs (x: {
        linkedSystems = x.linkedSystems ++ ["clsql" "ironclad" "esrap-peg" 
          "md5" "sb-bsd-sockets"]; 
        nativeBuildInputs = x.nativeBuildInputs ++ (with lispPackages; 
          [clsql ironclad esrap-peg md5]);
      });
    }
		])
