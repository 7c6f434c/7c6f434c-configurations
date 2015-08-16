targetSystem="${0%/bin/setup}"
export PATH="$targetSystem/sw/bin:$targetSystem/sw/sbin"
targetSystem="$(readlink -f "$targetSystem")"
echo "Target system configuration: $targetSystem"
export PATH="$targetSystem/sw/bin:$targetSystem/sw/sbin"
ln -sfT "$targetSystem" /run/current-system

rm -rf /bin
mkdir -p /bin
rm -rf /usr
mkdir -p /usr/bin

ln -sfT /run/current-system/sw/bin/sh /bin/sh
ln -sfT /run/current-system/sw/bin/env /usr/bin/env

rm /var/setuid-wrappers/*
cd /run/current-system/setuid/programs
for i in *; do
	readlink -n -f "$PWD/$i" > /var/setuid-wrappers/"$i".real
	cp -L /run/current-system/setuid/wrapper/bin/setuid-wrapper /var/setuid-wrappers/"$i"
        chown root /var/setuid-wrappers/"$i"
        chmod a+x,u+s /var/setuid-wrappers/"$i"
done
cd /run
mkdir -p /run/nix
pgrep nix-daemon >/dev/null || LANG=C LC_ALL=C nix-daemon &
mkdir -p /var/log/services
ip link set lo up
echo -n /run/current-system/bin/modprobe > /proc/sys/kernel/modprobe
for i in /run/current-system/etc/*; do 
	ln -sfT "$i" /etc/"${i#/run/current-system/etc/}"
done

rm /etc/crontab
cat /run/current-system/etc/crontab > /etc/crontab
chmod 0600 /etc/crontab

ln -sf /run/current-system/sw/share/timezone/UTC /etc/localtime
