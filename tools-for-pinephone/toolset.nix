let pkgs = import <nixpkgs> {}; in
with pkgs;
with rec {
  toolset = pkgs.buildEnv {
    name = "tools";
    paths = [
      nix
      monotone git
      screen slmenu fzf
      wlrctl wl-clipboard
      file remind strace gdb
      pv util-linux wavemon nmap curl htop powertop iotop
        (proxychains.overrideAttrs (x: {
          postPatch = (x.postPatch or "") + ''
            sed -e '/while[(]dll_dirs\[i\])/ii=0;' -i src/main.c
          '';
        }))
      gcc11 m4 rsync gnumake zip unzip
      sbcl pypy37
      (import ../lesser-expressions/texlive-set.nix pkgs)
      dmtx-utils
      librsvg pdftk transfig graphviz
      diffutils findutils fdupes age
      vim_configurable
      element-desktop gnome.geary 
      fractal gomuks
      #tdesktop
      links2
      dict
      (dictDBCollector {
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
            })
    (glibcLocales.override {
      locales=["en_US.UTF-8/UTF-8" "ru_RU.UTF-8/UTF-8" 
      "C.UTF-8/UTF-8" "en_GB.UTF-8/UTF-8"];
		allLocales = false;
	})
    (runCommandCC "in-pty" {} ''
      mkdir -p "$out/bin"
      cc -lutil ${../lang-os/c/in-pty.c} -o "$out/bin/in-pty"
    '')
    ];
  };
};
toolset
