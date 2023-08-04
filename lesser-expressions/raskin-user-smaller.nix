let 
NIXPKGS_env = builtins.getEnv "NIXPKGS";
pkgsPath = if NIXPKGS_env == "" then <nixpkgs> else NIXPKGS_env;
pkgs = import pkgsPath {}; in with pkgs;

let customVim = import /home/raskin/src/nix/configurations/misc/raskin/custom-vim.nix; in
let pp = import /home/raskin/src/nix/configurations/misc/raskin/private-packages.nix {inherit pkgs;}; in
let justUse = str: {name = str; path = builtins.getAttr str pkgs;}; in
let justUseMult = output: str: {name = "${str}.${output}"; path = builtins.getAttr output (builtins.getAttr str pkgs);}; in
let ppUse = str: {name = str; path = builtins.getAttr str pp;}; in
let julia_used = julia_16-bin; in
let myLispPackages = import ./lisp-packages.nix { inherit pkgs; }; in

linkFarm "raskin-packages" ([
                {name="mime"; path=shared-mime-info;}
                { name="query-fs"; 
                path = lib.overrideDerivation lispPackages.query-fs (x: {
                                NIX_LISP_EARLY_OPTIONS = " --dynamic-space-size 4096 ";
                                linkedSystems = x.linkedSystems ++ ["clsql" "clsql-postgresql" "clsql-sqlite3"
                                "ironclad" "esrap-peg" "md5" "sb-bsd-sockets"]; 
                                buildInputs = 
                                let
                                cl-fuse = lispPackages.cl-fuse.override (x: {
                                                src = /home/raskin/src/lsp/venv-cl-fuse/src/cl-fuse;
                                                });
                                cl-fuse-meta-fs = lispPackages.cl-fuse-meta-fs.override (x: {
                                                src = /home/raskin/src/lsp/venv-cl-fuse/src/cl-fuse-meta-fs;
                                                });
                                in
                                []
                                ++ [ cl-fuse cl-fuse-meta-fs ]
                                ++ x.buildInputs
                                ++ (with lispPackages; 
                                                [clsql ironclad md5 clsql-postgresql clsql-sqlite3])
                                ++ (with lispPackages; [esrap-peg])
                                ++ [ cl-fuse cl-fuse-meta-fs ]
                                ;
                });
                }
                { name = "openai-whisper-cpp";
                  path = openai-whisper-cpp.overrideAttrs (
                    x: { 
                      makeFlags = x.makeFlags ++ ["command"]; 
                      installPhase = x.installPhase + ''
                        cp ./command $out/bin/whisper-cpp-command
                      ''; 
                      }
                  );
                }
                {name = "gnome_themes_standard"; path = gnome3.gnome-themes-extra;}
                {name = "gnome-themes-standard"; path = gnome3.gnome-themes-extra;}
                {name = "adwaita_icon_theme"; path = gnome3.adwaita-icon-theme;}
                {name="clx-truetype"; path=lispPackages.clx-truetype;}
                {name="xkeyboard"; path=lispPackages.xkeyboard;}
                {name="clx-xkeyboard"; path=lispPackages.xkeyboard;}
                {name="clwrapper"; path=lispPackages.clwrapper;}
                {name="python-mozilla-marionette"; path=(import ../lang-os/marionette-python-packages.nix {inherit pkgs;}).marionette-harness;}
                {name="bordeaux-threads"; path=lispPackages.bordeaux-threads;}
                {name="lparallel"; path=lispPackages.lparallel;}
                {name="lfarm-client"; path=lispPackages.lfarm-client;}
                {name="lfarm-server"; path=lispPackages.lfarm-server;}
                {name="pcall"; path=lispPackages.pcall;}
                {name="parse-number"; path=lispPackages.parse-number;}
                /*{name="gimp-resynthesizer"; path=gimpPlugins.resynthesizer;}*/
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
                      julia_used
                      cmake hdf5 gnuplot glibc zlib
                      xorg.libXt xorg.libX11 xorg.libXrender 
                      xorg.libXtst xorg.libXext xorg.libXi qt4];
                    };
                }
                {name= "queryfs-deps";path = linkFarm
                  "queryfs-deps"
                  (map (x: {name=x.name+"."+x.outputName;path=x;})
                  (lib.concatLists (map (x: x.all)
                    [ sqlite postgresql fuse openssl ])));}
               /* {name="pypi2nix"; path = import /home/repos/pypi2nix/default.nix {};} */
               { name = "local-lisp-binaries"; path = buildEnv { name="lisp-binaries"; paths = 
               with myLispPackages; [
                 cl-mailer squid-url-rewrite rare-words
               ];};}
               /* { name = "pypy3-as-python3"; path = runCommandNoCC "pypy3-as-python3" {} ''
                 mkdir -p "$out/bin"
                 ln -s "${pypy37}/bin/pypy3" "$out/bin/python3"
               '';}
               { name = "cpython3-instead-of-pypy3"; path = runCommandNoCC "pypy3-as-python3" {} ''
                 mkdir -p "$out/bin"
                 ln -s "${python3}/bin/python3" "$out/bin/pypy3"
               '';} */
               { name = "julia"; path = julia_used; }
               /* { name = "heron-python"; path = 
                       python38.withPackages
                               (p: with p; [
                                (clingo.overrideAttrs
                                 (x: { buildInputs = (x.buildInputs or []) ++ [p.python];
                                  cmakeFlags = x.cmakeFlags ++ [
                                  "-DPYCLINGO_USER_INSTALL=OFF"
                                  "-DCLINGO_BUILD_WITH_PYTHON=ON"
                                  "-DPYCLINGO_USE_INSTALL_PREFIX=${builtins.placeholder "out"}/lib/${python3.libPrefix}"
                                  ]; 
                                  passthru = {
                                  pythonModule = p.python;
                                  };
                                  }))
                                z3 jinja2 lark
                               ])
                               ; } */
               { name = "wordnet-data"; path = runCommandNoCC "wordnet-data" {} ''
                 mkdir -p "$out/share/"
                 cd "$out/share"
                 tar -xf "${fetchurl {
                   url = "http://wordnetcode.princeton.edu/wn3.1.dict.tar.gz";
                   hash = "sha256:0an226fl5zpav7vmgkdxnn044wzcailxc44vsdkp3k3fxzl8nz9z";
                 }}"
               ''; }
               /* { name = "conpra-deps"; path = (import /home/raskin/.conpra-shell.nix).env; } */
               { name = "love_11_luajit_2_0"; path = love_11.override { luajit = luajit_2_0; }; }
               { name = "asy-graphtheory"; 
                 path = fetchFromGitHub {
                   owner = "taoari";
                   repo = "asy-graphtheory";
                   rev = "v5.0";
                   hash = "sha256:076h364j4jh9nqr1f7dqp7y22gxqq93dbz3x5bvz10za9ap0n13i";
                 };
               }
               { name = "gsettings_desktop_schemas"; path = gsettings-desktop-schemas; }
               { name = "weechat-matrix-bridge"; path = weechatScripts.weechat-matrix-bridge; }
               { name = "weechat-matrix"; path = weechatScripts.weechat-matrix; }
]
++ 
(map justUse [
 "gsettings-desktop-schemas" "gtk3"
 "fuse" "mysql" "openssl" "opencv" "postgresql" "sqlite"
 "icedtea_web" "love_0_10" "love_11" "libpulseaudio"
 "wgetpaste" "gdmap" "netcat" "python3" "kdiff3" "meld"
 "gfxtablet" "keynav"
 "tigervnc" "fbvnc"
 "glpk" "clingo" "urn"
 "plan9port" "sway" "syslogng" "rsyslog"
 "xmacro" "man-pages" "posix_man_pages" "mpv" "zbar" "lsb-release"
 "pinentry" "bfs" "moreutils" "spaceFM"
 "nix-prefetch-github" "nim"
])
++
(map (justUseMult "out") [
 "openssl"
])
)
