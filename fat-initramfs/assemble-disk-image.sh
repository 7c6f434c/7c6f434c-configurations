#! /bin/sh

command -v sgdisk >/dev/null || export PATH="$PATH:$(nix-build '<nixpkgs>' -f gptfdisk --no-out-link)/bin"

output="$1"
shift

size="$1"
dd if=/dev/zero count="$size" bs=1M of="$output"
shift

n=0
while test -n "$1"; do
        part="$1"
        type="$2"
        label="$3"
        shift; shift; shift

        n=$((n+1))
        psize="$(stat -c "%s" "$part")"
        pssize="$((psize/512))"

        sgdisk -n "$n::+$pssize" -c "$n:$label" "$output"

        offset="$(sgdisk -i$n "$output" | grep 'First sector: ' | sed -e 's/^First sector: //; s/ .*//')"
        offset_large="$((offset/2048))"
        dd if="$part" of="$output" conv=notrunc,nocreat bs=1M seek="$offset_large"
done
