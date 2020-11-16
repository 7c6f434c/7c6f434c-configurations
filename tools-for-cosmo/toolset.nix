with import <nixpkgs> {};
with rec {
  toolset = pkgs.buildEnv {
    name = "tools";
    paths = [
      nix
      (monotone.override{
        botan = (botan.override { openssl = null; }).overrideAttrs (x: { meta = null; });
      })
      file remind
      pv wavemon nmap
    ];
  };
};
toolset
