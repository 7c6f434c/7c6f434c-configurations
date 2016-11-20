#! /bin/sh

nix-env --set -p /nix/var/nix/profiles/everescue-nix -f "$(dirname "$0")/pseudo-system-thinkpad.nix" -A system --fallback "$@" &&
	/nix/var/nix/profiles/everescue-nix/bin/switch
