#! /bin/sh

tgt="$1"
num="${2:-16}"
shd="${3:-$(nix-build --no-out-link '<nixpkgs>' -A shadow)}"

echo deploying shadow
"$(dirname "$0")/deploy-nix-to-chroot.sh" "$tgt" "" "$shd" &> /dev/null
echo shadow deployed

( test -e "$tgt/etc/pam.d/useradd" && test -e "$tgt/etc/pam.d/groupadd") || {
  mkdir -p "$tgt/etc/pam.d"
  {
    echo "account sufficient pam_unix.so"  
    echo "auth sufficient pam_rootok.so"  
    echo "auth sufficient pam_unix.so likeauth"  
    echo "password requisite pam_unix.so nullok sha512"
    echo "session required pam_unix.so"
  } > "$tgt/etc/pam.d/useradd"
  cp "$tgt/etc/pam.d/useradd" "$tgt/etc/pam.d/groupadd"
}
test -e "$tgt/etc/pam.d/passwd" || cp "$tgt/etc/pam.d/useradd" "$tgt/etc/pam.d/passwd"
grep '^root:' "$tgt/etc/passwd" || echo 'root:x:0:0:System administrator:/root:/run/current-system/sw/bin/bash' >> "$tgt/etc/passwd"
grep '^root:' "$tgt/etc/group" || echo 'root:x:0:' >> "$tgt/etc/group"
grep '^root:' "$tgt/etc/shadow" || echo 'root:!:1::::::' >> "$tgt/etc/shadow"
grep . "" "$tgt/etc/login.defs" > /dev/null || {
  echo "DEFAULT_HOME yes"
  echo "SYS_UID_MIN  400"
  echo "SYS_UID_MAX  499"
  echo "UID_MIN      1000"
  echo "UID_MAX      29999"
  echo ""
  echo "SYS_GID_MIN  400"
  echo "SYS_GID_MAX  499"
  echo "GID_MIN      1000"
  echo "GID_MAX      29999"
  echo ""
  echo "TTYGROUP     tty"
  echo "TTYPERM      0620"
  echo ""
  echo "# Ensure privacy for newly created home directories."
  echo "UMASK        077"
  echo "# Uncomment this to allow non-root users to change their account"
  echo "#information.  This should be made configurable."
  echo "#CHFN_RESTRICT frwh"
} >> "$tgt/etc/login.defs"

test -e "$tgt/etc/nsswitch.conf" || {
  for i in passwd group shadow ethers services protocols; do
    echo "$i: files"
  done
  for i in hosts networks; do
    echo "$i: files dns"
  done
} > "$tgt/etc/nsswitch.conf"

chroot "$tgt" "$shd/bin/groupadd" -g 0 root
chroot "$tgt" "$shd/bin/groupadd" -g 1 wheel
chroot "$tgt" "$shd/bin/groupadd" -g 100 users
chroot "$tgt" "$shd/bin/groupadd" -g 30000 nixbld
chroot "$tgt" "$shd/bin/groupadd" -g 65534 nogroup

for i in $(seq $num); do
chroot "$tgt" "$shd/bin/useradd" -M -d /var/empty -g nixbld -G nixbld -u $((30000 + i)) -s /run/current-system/sw/bin/bash nixbld$i
done
