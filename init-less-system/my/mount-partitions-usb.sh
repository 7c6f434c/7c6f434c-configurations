
export HOME=/root

grep boot.no-mount /proc/cmdline && exec /init-tools/bin/sh

readlink -f /dev/disk/by-label/128_USB_SWAP > /sys/power/resume

udevadm trigger -c add
udevadm trigger
udevadm settle

readlink -f /dev/disk/by-label/128_USB_SWAP > /sys/power/resume

for i in /dev/sd?; do hdparm -B 255 $i; done

mkdir /new-root

mount /dev/disk/by-label/128_USB_MAIN /new-root

{
mkdir -p /new-root/boot
mount /dev/disk/by-label/USB128BOOT /new-root/boot
} &
{
mkswap /dev/disk/by-label/128_USB_SWAP -L 128_USB_SWAP
swapon $( readlink -f /dev/disk/by-label/128_USB_SWAP ) 
} &

while pgrep mount; do sleep 0.1; done

