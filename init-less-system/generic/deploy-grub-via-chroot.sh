#! /bin/sh

"$(dirname "$0")"/build-derivation-in-chroot.sh "$1" "$(nix-instantiate '<nixpkgs>' -A grub2_efi)" '' --option binary-caches "https://cache.nixos.org http://127.0.0.1:32062/ $NIX_BINARY_CACHES"
mkdir /new-root/boot/EFI
chroot "$1" "$(nix-store -q --outputs "$(nix-instantiate '<nixpkgs>' -A grub2_efi)" | head -n 1)/bin/grub-install" --efi-directory=/boot
