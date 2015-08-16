#! /bin/sh

nix-env -p /nix/var/nix/profiles/everescue-nix --set -f "$(dirname "$0")"/pseudo-system.nix -A system --fallback "$@"
rm /boot/loader/entries/everescue-nix-generation-*.conf
"$(dirname "$0")"/create-efi-entry.sh - - "$(readlink -f "/run/booted-system")" "previous boot"
for i in /nix/var/nix/profiles/everescue-nix-*-link; do
	n="${i}"
	n="${n%-link}"
	n="${n##*-}"
	"$(dirname "$0")"/create-efi-entry.sh - - "$(readlink -f "$i")" "$n"
done
maxn="$( ls -d /nix/var/nix/profiles/everescue-nix-*-link | sed -e 's@-link$@@; s@.*-@@' | sort -n | tail -n 1 )"
EFI_SET_DEFAULT=1 "$(dirname "$0")"/create-efi-entry.sh - - "$(readlink -f "/nix/var/nix/profiles/everescue-nix-${maxn}-link")" "$maxn" 
