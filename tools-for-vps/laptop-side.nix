with import <nixpkgs> {};
with rec {
        easy-rsa = stdenv.mkDerivation rec {
          pname = "easy-rsa";
          version="3.0.6";

          src = fetchFromGitHub {
            owner = "OpenVPN";
            repo = "easy-rsa";
            rev = "v${version}";
            sha256 = "sha256:0nwj3byj7n1l6hy03h2km93z1h68r2jsm7nfbf0g08ivgmc10k45";
          };

          buildCommand = ''
            mkdir -p "$out/share/easy-rsa/"
            cp -Tfr ${src}/easy*rsa* "$out/share/easy-rsa/"
            chmod u+w "$out/share/easy-rsa/vars.example"
            echo 'set_var EASYRSA "$PWD"' >> "$out/share/easy-rsa/vars.example"
            echo 'set_var EASYRSA_OPENSSL "${lib.getBin openssl}/bin/openssl"' >> "$out/share/easy-rsa/vars.example"
            echo 'alias awk="${lib.getBin gawk}/bin/awk"' >> "$out/share/easy-rsa/vars.example"
            mkdir -p "$out/bin"
            echo '#!/bin/sh' >> "$out/bin/easy-rsa-deploy"
            echo 'mkdir "$1"' >> "$out/bin/easy-rsa-deploy"
            echo "ln -sf '$out/share/easy-rsa'/* \"\$1\"" >> "$out/bin/easy-rsa-deploy"
            echo 'cp "$1/vars.example" "$1/vars"' >> "$out/bin/easy-rsa-deploy"
            chmod a+x "$out/bin/easy-rsa-deploy"
          '';
        };

        toolset = pkgs.buildEnv {
          name = "tools-for-vpn";
          paths = [ easy-rsa ];
        };
};
toolset
