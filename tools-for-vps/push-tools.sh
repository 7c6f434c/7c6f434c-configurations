#! /bin/sh

test -n "$1" || {
        "$0" $(cat target.private)
        exit
}

toolset="$(test-build ./vps-side.nix)"
list="$(mktemp)"
echo "$toolset" | xargs nix-store -qR > "$list"

rsync -arRz --progress --stats --files-from="$list" / root@"$1":/

rm "$list"

ssh root@"$1" chmod a+rX -R /nix
ssh root@"$1" ln -sfT "$toolset" ./tools
ssh raskin@"$1" ln -sfT "$toolset" ./tools
scp ii-starter ii-summarise raskin@"$1":
ssh root@"$1" mkdir -p /etc/openvpn/
scp openvpn.private/server*.conf "root@$1:/etc/openvpn/"
scp openvpn-keys.private/pki/ca.crt  openvpn-keys.private/pki/private/server.key   openvpn-keys.private/pki/issued/server.crt openvpn-keys.private/2048.dhparam "root@$1:/etc/openvpn/"
scp ii.service openvpn.service "root@$1:/lib/systemd/system/"
scp nat "root@$1:/etc/network/if-up.d/nat"
ssh root@"$1" systemctl enable ii
ssh root@"$1" systemctl enable openvpn
scp env "root@$1:/etc/env"
ssh raskin@"$1" "sh -c 'grep /etc/env .bashrc || echo . /etc/env >> .bashrc'"
ssh   root@"$1" "sh -c 'grep /etc/env .bashrc || echo . /etc/env >> .bashrc'"
