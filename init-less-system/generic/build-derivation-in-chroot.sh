#! /bin/sh

tgt="$1"
drv="$2"
nix="${3:-$(nix-build --no-out-link '<nixpkgs>' -A nixUnstable)}"

shift ; shift; shift 

"$(dirname "$0")/deploy-nix-to-chroot.sh" "$tgt" "$nix" "$drv" &> /dev/null

mkdir -p "$tgt"/{dev,sys,proc}

test -e "$tgt/dev/null" || mount --bind /dev "$tgt/dev"
test -e "$tgt/sys/class" || mount sys -t sysfs "$tgt/sys"
test -e "$tgt/proc/self" || mount proc -t proc "$tgt/proc"

test -e "$tgt/etc/resolv.conf" || cat /etc/resolv.conf >> "$tgt/etc/resolv.conf"
test -e "$tgt/etc/services" || 
test -L "$tgt/etc/services" || 
cat /etc/services >> "$tgt/etc/services"
test -e "$tgt/etc/protocols" || 
test -L "$tgt/etc/protocols" || 
cat /etc/protocols >> "$tgt/etc/protocols"
test -e "$tgt/bin/sh" || 
test -L "$tgt/bin/sh" || 
{
  bash="$(nix-build --no-out-link '<nixpkgs>' -A bash)"
  "$(dirname "$0")/deploy-nix-to-chroot.sh" "$tgt" "$nix" "$bash" &> /dev/null
  mkdir -p "$tgt/bin"
  ln -sf "$bash/bin/sh" "$tgt/bin/sh"
}
test -e "$tgt/$SSL_CERT_FILE" || 
test -L "$tgt/$SSL_CERT_FILE" || 
{
  cas="$(nix-build --no-out-link '<nixpkgs>' -A cacert)"
  "$(dirname "$0")/deploy-nix-to-chroot.sh" "$tgt" "$nix" "$cas" &> /dev/null
  mkdir -p "$(dirname "$tgt/$SSL_CERT_FILE")"
  ln -sf "$cas/etc/ca-bundle.crt" "$tgt/$SSL_CERT_FILE"
}

chroot "$tgt" "$nix/bin/nix-store" -r "$drv" "$@"
