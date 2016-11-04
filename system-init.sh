timestamp="$(date -u +%Y%m%d-%H%M%S)"

for i in /run/initrd/var/log/*; do
        cp "$i" "/var/log/$(basename "$i")-$timestamp"
done

dmesg > /var/log/dmesg-boot-"$timestamp"

{

cat /proc/mounts
grep boot.keep-initrd /proc/cmdline || {
  rm -rf /run/initrd/{nix,lib,init,init-tools,etc,var}
  umount /run/initrd
  rm -rf /run/initrd
}

mount --bind /nix/store /nix/store
mount /nix/store /nix/store -o remount,bind,ro
mkdir /dev/shm
mount -t tmpfs devshm /dev/shm -o rw,nosuid,nodev,size=1024M
mkdir -p /run/lock

hostname "401a0bf1.ignorelist.com"

export MODULE_DIR="$targetSystem/boot/kernel-modules/lib/modules"
for i in "snd-usb-audio" "snd-hda-codec-conexant" "asus-nb-wmi"                \
         "snd-hda-intel power_save=5 index=1,0 model=auto position_fix=0"      \
	 "af_packet" "acpi-cpufreq" "configs"                                  \
	 "cpufreq-ondemand" "coretemp" "battery" "xhci-hcd" "uvcvideo" "tun"   \
	 "snd-pcm" "kvm-intel" "aesni-intel" "crc32c-intel" "intel-powerclamp" \
	 "usb-storage" "mousedev" "evdev" "psmouse" "msr" "nbd" "ac" "battery" \
	 "thermal" "fuse" "loop" "rtc-cmos" "sysdig-probe" "ax88179_178a"      \
	 "smsc75xx" "asix" "cdc_ether" "usbhid" "hid-generic" "xhci-pci"       \
	 "af-packet" "wmi" "ctr" "arc4" "ath9k nohwcrypt=1" "ccm"              \
         "iptable_filter" "nf_conntrack_ipv4" "iptable_nat" "ipt-MASQUERADE"   \
	 "ipv6" "cifs" "hmac" "md4" "nls-utf8" "uas" "rndis-host"              \
         ;
  do      
	  echo $i
	  modprobe $i
done

mount fuse -t fusectl /sys/fs/fuse/connections/

mkdir -p /run/wpa_supplicant

chmod a+rwxt /tmp

rm -rf /run/udev/rules.d/*

} >/var/log/preinit-stdout-$timestamp 2>/var/log/preinit-stderr-$timestamp

dmesg > /var/log/dmesg-boot-"$timestamp"

sh -c 'while true; do agetty tty2 -l /run/current-system/sw/bin/login; done' & 
sh -c 'while true; do agetty tty3 -l /run/current-system/sw/bin/login; done' & 
sh -c 'while true; do agetty tty4 -l /run/current-system/sw/bin/login; done' & 
sh -c 'while true; do agetty tty5 -l /run/current-system/sw/bin/login; done' & 

pkill lighttpd; for i in bind nix-binary-cache postgresql sshd cron gogoc udev cups ; do pkill $i; /run/current-system/bin/run-service $i & done; pkill nix-daemon ; nix-daemon &

{ while ! test -f /run/shutdown; do sleep 1; done; ps -ef | grep '/var/setuid-wrappers/su -c true' | awk '{print $2}' | xargs kill ; } & 
while ! test -f /run/shutdown && ! su nobody -s /bin/sh -c "/var/setuid-wrappers/su -c true"; do echo Please enter root password to continue; done
mv /run/shutdown /run/shutdown.old &>/dev/null || true
chvt 1
/usr/bin/env HOME=/root bash -i
