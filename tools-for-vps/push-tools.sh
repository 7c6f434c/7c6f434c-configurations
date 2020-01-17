#! /bin/sh

test -n "$1" || {
        "$0" $(cat target.private)
        exit
}

nix build -f ./vps-side.nix -o ~/.nix-personal/vps-tools
toolset="$(readlink -f ~/.nix-personal/vps-tools)"
nix copy "$toolset" --to ssh://root@"$1"

ssh root@"$1" nix-store --indirect --add-root ./tools -r "$toolset"
ssh raskin@"$1" ln -sfT "$toolset" ./tools
ssh matrix@"$1" ln -sfT "$toolset" ./tools
ssh root@"$1" "bash -c 'cd ./tools/global/; find . -type d | while read i; do mkdir -vp /\$i; done; find . -type f | while read i; do ln -vsfT \$(readlink -f \$i) /\$i; done'"
scp openvpn-keys.private/pki/ca.crt  openvpn-keys.private/pki/private/server.key   openvpn-keys.private/pki/issued/server.crt openvpn-keys.private/2048.dhparam "root@$1:/etc/openvpn/"
ssh root@"$1" "bash -c 'for i in ii openvpn openvpn-tcp; do systemctl enable \$i; done'"
ssh raskin@"$1" "sh -c 'grep /etc/env .bashrc || echo . /etc/env >> .bashrc'"
ssh   root@"$1" "sh -c 'grep /etc/env .bashrc || echo . /etc/env >> .bashrc'"
ssh matrix@"$1" "sh -c 'grep /etc/env .bashrc || echo . /etc/env >> .bashrc'"
