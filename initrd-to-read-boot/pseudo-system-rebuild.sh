#! /bin/sh

nix-env -p /nix/var/nix/profiles/everescue-nix --set -f "$(dirname "$0")"/pseudo-system.nix -A system --fallback
rm /boot/loader/entries/everescue-generation-*.conf
for i in /nix/var/nix/profiles/everescue-nix-*-link; do
	n="${i}"
	n="${n%-link}"
	n="${n##*-}"
	"$(dirname "$0")"/create-efi-entry.sh - - "$(readlink -f "$i")" "$n"
done
