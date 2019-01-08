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
                                NIX_LISP_EARLY_OPTIONS = " --dynamic-space-size 4096 ";
                                linkedSystems = x.linkedSystems ++ ["clsql" "clsql-postgresql" "clsql-sqlite3"
                                "ironclad" "esrap-peg" "md5" "sb-bsd-sockets"]; 
                                nativeBuildInputs = x.nativeBuildInputs ++ (with lispPackages; 
                                                [clsql ironclad md5 clsql-postgresql clsql-sqlite3]) ++ (with lispPackages; [esrap-peg]);
                                });
                }
                {name = "gnome_themes_standard"; path = gnome3.gnome_themes_standard;}
                {name = "adwaita_icon_theme"; path = gnome3.adwaita-icon-theme;}
                {name="clx-truetype"; path=lispPackages.clx-truetype;}
                {name="xkeyboard"; path=lispPackages.xkeyboard;}
                {name="clx-xkeyboard"; path=lispPackages.xkeyboard;}
                {name="clwrapper"; path=lispPackages.clwrapper;}
                {name="python-mozilla-marionette"; path=pythonPackages.marionette-harness;}
                {name="ipython"; path=pythonPackages.ipython;}
                {name="bordeaux-threads"; path=lispPackages.bordeaux-threads;}
                {name="gimp-resynthesizer"; path=gimpPlugins.resynthesizer;}
                { name = "gimp-resynthesizer2"; path = gimpPlugins.resynthesizer2; }
                { name = "words"; path = scowl; }
                { name = "dicts"; path = dictDBCollector {
                        dictlist = (with dictdDBs; map 
                                (x:{
                                 name = x.dbName;
                                 filename = x.outPath;
                                 locale = x.locale;
                                 })
                        [ 
                                eng2fra fra2eng eng2nld
                                        nld2eng eng2rus
                                        eng2deu deu2eng
                                        mueller_enru_abbr
                                        mueller_enru_base
                                        mueller_enru_dict
                                        mueller_enru_geo
                                        mueller_enru_names
                        ]) ++ [
                          { 
                            name = "wiktionary-en";
                            filename = "${dictdDBs.wiktionary}/share/dictd/wiktionary-en";
                            locale = "en_US.UTF-8";
                          }
                          { 
                            name = "wordnet";
                            filename = "${dictdDBs.wordnet}/share/dictd/wn";
                            locale = "en_US.UTF-8";
                          }
                        ];
                                                         };
                }
                {
                  name = "julia-fhs-env";
                  path = buildFHSUserEnv {
                    name = "julia-fhs-env"; 
                    targetPkgs = p: with p; [
                      julia_10
                      cmake hdf5 gnuplot glibc zlib
                      xorg.libXt xorg.libX11 qt4];
                    };
                }
                {name = "julia_10"; path=julia_10;}
                {name= "queryfs-deps";path = linkFarm
                  "queryfs-deps"
                  (map (x: {name=x.name+"."+x.outputName;path=x;})
                  (lib.concatLists (map (x: x.all)
                    [ sqlite postgresql fuse openssl ])));}
]
++ 
(map justUse [
 "gsettings_desktop_schemas" "gtk3" "weechat-matrix-bridge"
 "fuse" "mysql" "openssl" "opencv" "postgresql" "sqlite"
 "icedtea_web" "love_0_10" "love_11" "libpulseaudio"
 "wgetpaste" "gdmap" "netcat" "python3" "kdiff3"
])
++
(map (justUseMult "out") [
 "openssl"
])
)
