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
      age fzf
      (import ../lesser-expressions/texlive-set.nix pkgs)
    (runCommandCC "in-pty" {} ''
      mkdir -p "$out/bin"
      cc -lutil ${../lang-os/c/in-pty.c} -o "$out/bin/in-pty"
    '')
    ];
  };
};
toolset
