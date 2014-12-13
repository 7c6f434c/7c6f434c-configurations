#! /bin/sh

loaderdir="/boot/EFI/everescue-nix/"
mkdir -p "$loaderdir"

kernel="$1"
initrd="$2"
target="$3"
version="$4"

[ "$kernel" = - ] && kernel=
[ "$initrd" = - ] && initrd=

kernel="${kernel:-$target/boot/kernel-package}"
initrd="${initrd:-$target/boot/initrd-package}"

kernel="$(readlink -f "$kernel")"
initrd="$(readlink -f "$initrd")"

kbasename="$(basename "$kernel")-bzImage.efi"
ibasename="$(basename "$initrd")-initrd.efi"
cp "$kernel/bzImage" "$loaderdir/$kbasename"
cp "$initrd/initrd" "$loaderdir/$ibasename"

echo "
title EveRescue/Nix
version Generation $version
machine-id $(cat /etc/machine-id)

linux /EFI/everescue-nix/$kbasename
initrd /EFI/everescue-nix/$ibasename
options targetSystem=$target
" | tee /boot/loader/entries/everescue-generation-"$version".conf
