#! /bin/sh

command -v nix || export PATH="$PATH:$(nix-build --no-out-link '<nixpkgs>' -A nixUnstable)/bin"
command -v genext2fs || export PATH="$PATH:$(nix-build --no-out-link '<nixpkgs>' -A genext2fs)/bin"

echo "Using PATH: $PATH" >&2

temproot="$(mktemp -d)"
echo "Working in $temproot" >&2

output="$1"
shift ;

layout="$1"

echo "Copying to $temproot: $*" >&2
nix copy "$layout" -f '<nixpkgs>' "$@" --to local\?root="$temproot" --no-check-sigs

echo "Applying $layout to $temproot" >&2
cp -rfT "$layout" "$temproot"

echo "Creating the image $output from $temproot" >&2
test -n "$E2FS_OPTIONS" && echo "Applying options: $E2FS_OPTIONS" >&2

blocks="$(du -s "$temproot" | cut -f 1)"
inodes="$(find "$temproot" | wc -l)"

genext2fs "$output" -d "$temproot" -U -b "$(((blocks * 11 / 10) / 2048 * 2048 + 2048 + 4096))" -N "$((inodes * 11 / 10 + 4096))" -f $E2FS_OPTIONS

echo "Cleaning up $temproot" >&2
chmod u+rwx -R "$temproot"
rm -rf "$temproot"

tune2fs -j "$output"

test -e "$output" && echo "Done. Created $output" >&2
