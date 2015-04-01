#! /bin/sh

tgt="$1"
nix="${2:-$(nix-build --no-out-link '<nixpkgs>' -A nixUnstable)}"
extra="$3"

deps="$(nix-store -qR $nix $extra)"
nixdeps="$(nix-store -qR $nix )"

echo "Deploying $nix and also [ $extra ] to $tgt"
echo "Adding [ $deps ]"
echo "Starting with [ $nixdeps ]"

test -n "$nixdeps" && rsync -aRr $nixdeps "$tgt"
nix-store --export $deps | chroot "$tgt" "$nix/bin/nix-store" --import
