
export HOME=/root

mkdir -p /tmp

grep boot.no-mount /proc/cmdline && exec /init-tools/bin/sh

readlink -f /dev/ThinkPadMain/Swap > /sys/power/resume

udevadm trigger -c add
udevadm trigger
udevadm settle

modprobe dm-mod
vgchange -ay

udevadm settle

readlink -f /dev/ThinkPadMain/Swap > /sys/power/resume

for i in /dev/sd?; do hdparm -B 255 $i; done

yes y | mkfs.ext4 /dev/ThinkPadMain/Tmp

mount /dev/ThinkPadMain/SystemRoot /new-root

{
mkdir -p /new-root/var
mount /dev/ThinkPadMain/Var /new-root/var
mkdir -p /new-root/var/db
mkdir -p /new-root/var/log
mount /dev/ThinkPadMain/VarDb /new-root/var/db &
mount /dev/ThinkPadMain/VarLog /new-root/var/log &
} &
mkdir -p /new-root/nix
mkdir -p /new-root/tmp
mkdir -p /new-root/home
mkdir -p /new-root/root
mkdir -p /new-root/boot
{
        mount /dev/ThinkPadMain/Nix /new-root/nix
        mount /new-root/nix/store /new-root/nix/store -o bind,ro
        mount /new-root/nix/store -o remount,ro,bind
} &
mount /dev/ThinkPadMain/Tmp /new-root/tmp &
mount /dev/ThinkPadMain/Home /new-root/home &
mount /dev/ThinkPadMain/Root /new-root/root &
{
mkswap /dev/ThinkPadMain/Swap 
swapon $( readlink -f /dev/ThinkPadMain/Swap ) 
} &
mount /dev/disk/by-label/NIXOS_EFI /new-root/boot &
{
mkdir /dev/pstore
mount pstore -t pstore /dev/pstore
} &

while pgrep mount; do sleep 0.1; done

chmod a+rwxt /new-root/tmp
