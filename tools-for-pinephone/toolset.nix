let pkgs = import <nixpkgs> {}; in
with pkgs;
with rec {
  toolset = pkgs.buildEnv {
    name = "tools";
    paths = [
      nix
      monotone
      file remind
      pv wavemon nmap
      gcc11 m4 rsync gnumake
      (import ../lesser-expressions/texlive-set.nix pkgs)
      dmtx-utils
      diffutils findutils
      vim_configurable
      element-desktop
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
    ];
  };
};
toolset
