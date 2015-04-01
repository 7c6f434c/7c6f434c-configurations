
export HOME=/root

grep boot.no-mount /proc/cmdline && exec /init-tools/bin/sh

readlink -f /dev/NotebookMain/Swap > /sys/power/resume

udevadm trigger -c add
udevadm trigger
udevadm settle

modprobe dm-mod
vgchange -ay

udevadm settle

readlink -f /dev/NotebookMain/Swap > /sys/power/resume

for i in /dev/sd?; do hdparm -B 255 $i; done

yes y | mkfs.ext4 /dev/NotebookMain/Tmp

mount /dev/NotebookMain/SystemRoot /new-root

{
mount /dev/NotebookMain/Var /new-root/var
mount /dev/NotebookMain/VarDB /new-root/var/db &
mount /dev/NotebookMain/VarLog /new-root/var/log &
} &
mount /dev/NotebookMain/Nix /new-root/nix &
mount /dev/NotebookMain/Tmp /new-root/tmp &
mount /dev/NotebookMain/Home /new-root/home &
mount /dev/NotebookMain/Root /new-root/root &
{
mkswap /dev/NotebookMain/Swap 
swapon $( readlink -f /dev/NotebookMain/Swap ) 
} &
mount /dev/disk/by-label/NIXOS_EFI /new-root/boot &

while pgrep mount; do sleep 0.1; done

