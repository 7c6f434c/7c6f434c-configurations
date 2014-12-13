export HOME=/root

grep boot.no-mount /proc/cmdline && exec /init-tools/bin/sh

{

readlink -f /dev/NotebookMain/Swap > /sys/power/resume

udevadm settle

modprobe dm-mod
vgchange -ay

udevadm settle

for i in /dev/sd?; do hdparm -B 255 $i; done

mkfs.ext4 /dev/NotebookMain/Tmp

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
swapon /dev/NotebookMain/Swap &
mount /dev/disk/by-label/NIXOS_EFI /new-root/boot &

while pgrep mount; do sleep 0.1; done

for i in /dev /proc /run /sys ; do
	mount --move $i /new-root/$i &
done

while pgrep mount; do sleep 0.1; done

ln -s $targetSystem /new-root/run/current-system
ln -s $targetSystem /new-root/run/booted-system

udevadm control --exit
pkill udevd
pkill udev
kill $(pgrep udev)
ps -ef | grep udev

mkdir /new-root/run/initrd
cd /new-root

staticCpio=$(chroot . readlink -f $targetSystem/static-tools/cpio)
staticGzip=$(chroot . readlink -f $targetSystem/static-tools/gzip)
initrdFile=$(chroot . readlink -f $targetSystem/boot/initrd)

} 2>/var/log/medium-boot-stderr >/var/log/medium-boot-out

pivot_root . initrd
if ! test -d /initrd; then
	cat ./proc/mounts
	mount --bind / ./run/initrd
fi

chroot . ./$targetSystem/bin/setup

if chroot . test -e ./$targetSystem/bin/init; then
	chroot . ./$targetSystem/bin/init
else
	chroot . ./usr/bin/env bash -i
fi

cd /

echo Send TERM
kill -cont -1
sleep 0.1
kill -term -1
sleep 0.1
kill -cont -1
sleep 1

echo Send QUIT
kill -cont -1
sleep 0.1
kill -quit -1
sleep 0.1
kill -cont -1
sleep 0.5

echo Send PIPE
kill -cont -1
sleep 0.1
kill -pipe -1
sleep 0.1
kill -cont -1
sleep 0.1

echo Send ABRT
kill -cont -1
sleep 0.1
kill -abrt -1
sleep 0.1
kill -cont -1
sleep 0.1

echo Sigh. Send KILL
kill -cont -1
sleep 0.1
kill -kill -1 
sleep 0.1
kill -cont -1
sleep 0.1

/new-root/$staticGzip -d < /new-root/$initrdFile | /new-root/$staticCpio -i 

mount --move /new-root/proc /proc
mount --move /new-root/dev /dev
cat /proc/mounts | cut -d ' ' -f 2 | tac | xargs umount
mount procfs -t proc /proc
cat /proc/mounts | cut -d ' ' -f 2 | tac | xargs umount
sleep 0.5
mount procfs -t proc /proc
cat /proc/mounts | cut -d ' ' -f 2 | tac | xargs umount
mount procfs -t proc /proc
cat /proc/mounts
sync
