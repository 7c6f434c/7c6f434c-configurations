#! /bin/sh

test -n "$1" || {
        "$0" $(cat target.private)
        exit
}

nix-instantiate ./vps-side.nix --add-root ~/.nix-personal/derivations/vps-tools.drv
nix-store -r  ~/.nix-personal/derivations/vps-tools.drv --add-root ~/.nix-personal/vps-tools
toolset="$(readlink -f ~/.nix-personal/vps-tools)"
nix copy "$toolset" -s --to ssh://root@"$1"

ssh root@"$1" ln -sfT "$toolset" ./tools
ssh root@"$1" ./tools/bin/nix-store --indirect --add-root ./tools -r "$toolset"
ssh root@"$1" ln -sfT "$toolset" /opt/tools
ssh root@"$1" ./tools/bin/remote-deploy
ssh raskin@"$1" ln -sfT "$toolset" ./tools
ssh matrix@"$1" ln -sfT "$toolset" ./tools
scp openvpn-keys.private/pki/ca.crt  openvpn-keys.private/pki/private/server.key   openvpn-keys.private/pki/issued/server.crt openvpn-keys.private/2048.dhparam "root@$1:/etc/openvpn/"
