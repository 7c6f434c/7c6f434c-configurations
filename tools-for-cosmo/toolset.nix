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
      gcc11
      (import ../lesser-expressions/texlive-set.nix pkgs)
    ];
  };
};
toolset
