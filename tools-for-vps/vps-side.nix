with import <nixpkgs> {};
with rec {
        globalLinks = pkgs.runCommand "global-links" {} ''
          ncp () {
                mkdir -p "$2"
                cp "$1" "$2"/"$(basename "$1" | sed -re 's/^[a-z0-9]+-//')"
          }
          mkdir -p "$out/global/"
          cd "$out/global/"
          ncp ${./ii-starter} ./home/raskin
          ncp ${./ii-summarise} ./home/raskin
          ncp ${./openvpn.private/server.conf} ./etc/openvpn
          ncp ${./openvpn.private/server-tcp.conf} ./etc/openvpn
          ncp ${./ii.service} ./lib/systemd/system
          ncp ${./openvpn.service} ./lib/systemd/system
          ncp ${./nat} ./etc/network/if-up.d
          ncp ${./env} ./etc
        '';
        toolset = pkgs.buildEnv {
          name = "tools-for-vps";
          paths = [ openvpn matrix-synapse ii screen nginx less
                rsync git monotone fossil mercurial vim strace transmission
                glibcLocales host dnsutils mtr htop iotop hping socat iftop
                curl wget youtube-dl jemalloc nix dehydrated netcat
                globalLinks
          ];
        };
};
toolset

