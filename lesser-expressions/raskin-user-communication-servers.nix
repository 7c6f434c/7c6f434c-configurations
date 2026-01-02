with import ./env-defs.nix;
with pkgs;

linkFarm "raskin-heavy-packages" ([
  { name = "main-heavy-package-set";
    path = (fullEnv "main-heavy-package-set"
      [
        prosody matrix-synapse dovecot postfix
        bind kea dnsmasq lighttpd nginx
      ]);}
])
