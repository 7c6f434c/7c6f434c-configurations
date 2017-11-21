#! /bin/sh

export PATH="$(dirname "$0")/../sw/bin":"$PATH":/var/current-system/sw/bin

grubHeader="
# Automatically generated

search --set=drive1 --fs-uuid "$(grub-probe "${1:-/boot}" -t fs_uuid)"

if [ -s \$prefix/grubenv ]; then
  load_env
fi

insmod efi_gop
insmod efi_uga
insmod font

if loadfont (\$drive1)//grub/fonts/unicode.pf2; then
  insmod gfxterm
  set gfxmode=auto
  set gfxpayload=keep
  terminal_output gfxterm
fi

set default=0 
set timeout=15
"

mkdir -p /boot/kernels
cp -f /boot/grub/grub.cfg{,.old}"$2"
sync

mkdir -p /boot/grub/fragments.new
for i in /nix/var/nix/profiles/*/ /run/booted-system/ /var/current-system/; do
  test -e "$i/boot/for-bootloader/grub.part.cfg" && {
    test -d "/boot/grub/fragments/$(basename "$i")" &&
      mv "/boot/grub/fragments/$(basename "$i")" "/boot/grub/fragments.new/$(basename "$i")"
  }
done
sync
rm -rf /boot/grub/fragments
mv /boot/grub/fragments.new /boot/grub/fragments
sync
for i in /boot/kernels/*.efi; do
  grep "$(basename "$i")" /boot/grub/fragments/*/grub.part.cfg -m1 > /dev/null || rm "$i"
done
n=0
rm /boot/grub/fragment-index/*
mkdir -p /boot/grub/fragment-index/
for i in /var/current-system/ /run/booted-system/ /nix/var/nix/profiles/*-link/ ; do
  test -e "$i/boot/for-bootloader/grub.part.cfg" && {
    n=$((n+1))
    echo "/boot/grub/fragments/$(basename "$i")" > /boot/grub/fragment-index/$(printf "%06d" $n)
    test -d "/boot/grub/fragments/$(basename "$i")" || {
      mkdir "/boot/grub/fragments/$(basename "$i")"
    }
    cp -fL "$i/boot/for-bootloader/"/grub.part.cfg "/boot/grub/fragments/$(basename "$i")"/ 2>/dev/null
    yes n | cp -iL "$i"/boot/for-bootloader/*.efi /boot/kernels/ 2>/dev/null
  }
done
sync
for i in /boot/grub/fragments/*; do
  sed -re "s@^menuentry[^\"]*\"@&$(basename "$i") @"  "$i/grub.part.cfg" > "$i/grub.part.labeled.cfg"
  ( echo "$grubHeader"; cat "$i/grub.part.labeled.cfg" ) > "$i/grub.one.cfg"
done
cp /boot/grub/grub.fragmented.cfg{,.old}"$2"
sync
( echo "$grubHeader"; cat /boot/grub/fragment-index/* |
    sed -e 's@$@/grub.part.labeled.cfg@' | xargs cat ) > /boot/grub/grub.fragmented.cfg"$2"
cp /boot/grub/grub{.fragmented,}.cfg"$2"
sync
