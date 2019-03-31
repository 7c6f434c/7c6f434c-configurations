with import <nixpkgs> {};
with rec {
        toolset = pkgs.buildEnv {
          name = "tools-for-vps";
          paths = [ openvpn matrix-synapse ii screen nginx less
                rsync git monotone fossil mercurial vim strace transmission
                glibcLocales host dnsutils mtr htop iotop hping socat iftop
                curl wget youtube-dl
          ];
        };
};
toolset

