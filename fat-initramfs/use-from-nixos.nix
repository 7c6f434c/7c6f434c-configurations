{pkgs, nixos}:
{
  serviceScript = name: config:
    (builtins.getAttr name (nixos {configuration = config;}).config.systemd.services).runner;

  etcSelect = filename: config:
    let
      nixosInstance = nixos {configuration = config;};
      selected = (pkgs.lib.filterAttrs (k: v: v.target == filename) nixosInstance.config.environment.etc);
        source = (builtins.getAttr (builtins.head (builtins.attrNames selected)) selected).source;
    in (if pkgs.lib.isString source then source else source.outPath);
}
