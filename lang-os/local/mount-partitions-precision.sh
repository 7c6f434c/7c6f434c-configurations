set -x

cryptsetup open --allow-discards /dev/disk/by-partlabel/NVMe-4T-Precision-Crypt NVMe-4T-Precision-CryptVolume

    udevadm trigger --action=add
    udevadm settle

vgchange -ay

    udevadm trigger --action=add
    udevadm settle

readlink -f /dev/mapper/NVMe--4T--Precision--MainCrypt-Swap > /sys/power/resume
swapon /dev/mapper/NVMe--4T--Precision--MainCrypt-Swap

mount -t ext4 /dev/mapper/NVMe--4T--Precision--MainCrypt-SystemRoot /new-root
mount -t btrfs /dev/mapper/NVMe--4T--Precision--MainCrypt-Nix /new-root/nix
ls -ld /new-root/nix/store
chmod a-r /new-root/nix/store
ls -ld /new-root/nix/store
mount /dev/mapper/NVMe--4T--Precision--MainCrypt-Home /new-root/home
mount /dev/mapper/NVMe--4T--Precision--MainCrypt-Root /new-root/root
mount /dev/mapper/NVMe--4T--Precision--MainCrypt-Var /new-root/var
mount /dev/mapper/NVMe--4T--Precision--MainCrypt-VarDb /new-root/var/db
mount /dev/mapper/NVMe--4T--Precision--MainCrypt-VarLog /new-root/var/log

yes y | mkfs.ext4 /dev/mapper/NVMe--4T--Precision--MainCrypt-Tmp

mount /dev/mapper/NVMe--4T--Precision--MainCrypt-Tmp /new-root/tmp
chmod a+rwxt /new-root/tmp

mount -t vfat /dev/disk/by-partlabel/NVMe-4T-EFI /new-root/boot

modprobe fuse

mount fuse -t fusectl /sys/fs/fuse/connections/
