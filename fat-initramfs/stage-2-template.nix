{
  stage1
  , systemCaption ? "Layered-Nix-System"
  , startupScript ? "/bin/sh -i;"
  , setupScript ? ""
  , pkgs ? (import <nixpkgs> {})
  , setuidProgramList ? [ "fusermount" "fuserumount"
    "fusermount3" "fuserumount3"
    { src = "unix_chkpwd.orig" ; dst = "unix_chkpwd"; }
  ]
  , systemPackages ? pkgs : []
  , firmwarePackages ? pkgs: [pkgs.firmwareLinuxNonfree]
  , systemEtc ? null
  , kernelParameters ? []
  , allOutputs ? true
}:
rec {
  maybeCall = f: arg: if builtins.isFunction f then f arg else f;

  _sinit = pkgs.sinit.override {
          rcshutdown = "/run/current-system/bin/back-to-initrd";
          rcreboot = "/run/current-system/bin/reinit";
          rcinit = "/run/current-system/bin/startup";
  };

  systemGrubConfig = ''
    menuentry "${systemCaption}" {
      linux ($drive1)/kernels/${builtins.baseNameOf stage1.kernel}.linux.efi BOOT_IMAGE=${stage1.kernel} targetSystem=@TARGET_SYSTEM@ ${builtins.toString kernelParameters}
      initrd ($drive1)/kernels/${builtins.baseNameOf stage1.initrd}.initrd.efi
    }
  '';
  systemModprobe = pkgs.writeScript "modprobe" ''#!/bin/sh
    export PATH="${pkgs.coreutils}/bin:${pkgs.kmod}/bin"
    test -z "$targetSystem" && export targetSystem="$(dirname "$0")/../"
    MODULE_DIR="$targetSystem/boot/kernel-modules/lib/modules" modprobe "$@" ||
    MODULE_DIR=/run/booted-system/boot/kernel-modules/lib/modules modprobe "$@"
  '';
  systemInit = pkgs.writeScript "init" ''#!${pkgs.stdenv.shell}
    "$targetSystem"/bin/setup
    export PATH="/var/current-system/sw/bin"
    ln -sfT "$targetSystem" /run/booted-system
    echo switch happened

    cd /run
    cp ${_sinit}/bin/sinit .

    exec ./sinit

    echo sinit failed

    while true; do bash -i || /bin/sh -i; done
  '';
  systemSetup = pkgs.writeScript "setup" ''#!${pkgs.stdenv.shell}
    export PATH="$PATH:$targetSystem/sw/bin"
    targetSystem="$(readlink -f "$(dirname "$0")/..")"
    export PATH="$targetSystem/sw/bin:${pkgs.coreutils}/bin"

    echo -n "$targetSystem"/bin/modprobe > /proc/sys/kernel/modprobe
    echo -n "$targetSystem"/sw/lib/firmware > /sys/module/firmware_class/parameters/path

    for i in etc bin usr; do
      if ! ( test "$(readlink "/$i")" = "/var/current-system/global/$i" ); then
        mv "/$i" "/$i-$( date +%Y%m%d-%H%M%S )"
        ln -sfT "/var/current-system/global/$i" "/$i"
      fi
    done

    targetSetuidWrappers="/var/setuid-wrapper-storage/$(basename "$(readlink -m "$targetSystem/setuid/wrappers")")"
    mkdir -p "$targetSetuidWrappers"
    cp -a "$targetSystem/setuid/programs"/* "$targetSetuidWrappers"
    for i in "$targetSystem"/setuid/programs/*; do
      ib="$(basename "$i")"
      chown 0:0 "$targetSetuidWrappers/$ib"
      chmod u+s,a+x "$targetSetuidWrappers/$ib"
    done

    ln -sfT /var/current-system/setuid/wrappers /var/setuid-wrappers

    mkdir -p /run/wrappers
    ln -sfT /var/current-system/setuid/wrappers /run/wrappers/bin

    ln -sf /var/current-system /nix/var/nix/gcroots/
    ln -sf /var/latest-booted-system /nix/var/nix/gcroots/

    ${setupScript}

    "$targetSystem"/bin/modprobe af-packet
    ip link set lo up

    bootedSystem="$(cat /proc/cmdline | tr ' ' '\n' | grep targetSystem= | tr '=' '\n' | tail -n 1)"

    ln -sfT "/var/latest-booted-system" /run/booted-system
    ln -sfT "$bootedSystem" /var/latest-booted-system

    ln -sfT /var/current-system/ /run/current-system
    ln -sfT "$targetSystem" /var/current-system
  '';
  systemActivate = pkgs.writeScript "activate" ''#!/bin/sh
    "$(dirname "$0")"/setup'';
  systemSwitch = pkgs.writeScript "switch" ''#!/bin/sh
    "$(dirname "$0")"/activate
    "$(dirname "$0")"/boot'';
  systemBoot = pkgs.writeScript "boot" ''#!/bin/sh
    "$(dirname "$0")"/create-grub-config'';
  createGrubCfgScript = pkgs.writeScript "create-grub-config" ''#!/bin/sh
      test -f "/boot/EFI/Boot/BOOTX64.EFI" || {
        ${pkgs.grub2_efi}/bin/grub-install --efi-directory=/boot
        mkdir /boot/EFI/Boot
        cp /boot/EFI/{grub/grubx64.efi,Boot/BOOTX64.EFI}
      }

      mkdir -p /boot/kernels
      grubHeader="$(${./grub-print-header.sh} /boot)"
      cp -f /boot/grub/grub.cfg{,.old}
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
        grep "''${i#/boot}" /boot/grub/fragments/*/grub.part.cfg -m1 > /dev/null || rm "$i"
      done
      n=0
      rm /boot/grub/fragment-index/*
      mkdir -p /boot/grub/fragment-index/
      for i in /var/current-system/ /run/booted-system/ /nix/var/nix/profiles/*-link/ ; do
        test -e "$i/boot/for-bootloader/grub.part.cfg" && {
          n=$((n+1))
          echo "/boot/grub/fragments/$(basename "$i")" > /boot/grub/fragment-index/$(printf "%06d" $n)
          cp -fL "$i/boot/for-bootloader/"/grub.part.cfg "/boot/grub/fragments/$(basename "$i")"/ 2>/dev/null
          rm "/boot/grub/fragments/$(basename "$i")" 2> /dev/null
          test -d "/boot/grub/fragments/$(basename "$i")" || {
            mkdir "/boot/grub/fragments/$(basename "$i")"
            cp -fL "$i/boot/for-bootloader/"/grub.part.cfg "/boot/grub/fragments/$(basename "$i")"/ 2>/dev/null
            cp -L "$i/boot/for-bootloader/"/*.efi /boot/kernels/
          }
        }
      done
      sync
      for i in /boot/grub/fragments/*; do
        sed -re "s@^menuentry[^\"]*\"@&$(basename "$i") @"  "$i/grub.part.cfg" > "$i/grub.part.labeled.cfg"
        ( echo "$grubHeader"; cat "$i/grub.part.labeled.cfg" ) > "$i/grub.one.cfg"
      done
      cp /boot/grub/grub.fragmented.cfg{,.old}
      sync
      ( echo "$grubHeader"; cat /boot/grub/fragment-index/* |
          sed -e 's@$@/grub.part.labeled.cfg@' | xargs cat ) > /boot/grub/grub.fragmented.cfg
      cp -f /boot/grub/grub{.fragmented,}.cfg
      sync
    '';

  backToInitrd = pkgs.writeScript "back-to-initrd" ''
    #!/bin/sh
    export PATH=/run/current-system/sw/bin
    cd /
    mkdir /initrd
    mount initramfs -t tmpfs initrd
    cd initrd
    gunzip < /run/current-system/boot/initrd | cpio -i
    mkdir new-root tmp
    echo "#!/bin/sh" >> post-pivot
    cat << EOF >> post-pivot
      chvt 1
      mkdir /run

      mount --move /new-root/proc /proc
      mount --move /new-root/sys /sys
      mount --move /new-root/dev /dev
      mount --move /new-root/run /run

      for signal in 15 2 9 9; do
              cat /proc/mounts | cut -d ' ' -f 2 | grep /new-root | tac | xargs -n1 umount
              mountpoint /new-root || break
              cat /proc/mounts | cut -d ' ' -f 2 | grep /new-root | tac | xargs -n1 fuser -k -$signal
              cat /proc/mounts | cut -d ' ' -f 2 | grep /new-root | tac | xargs -n1 umount
              mountpoint /new-root || break
              sleep 0.3
      done

      mountpoint /new-root

      while true; do /bin/sh -i; done
    EOF
    chmod a+x post-pivot
    export PATH=/run/current-system/sw/bin:/init-tools/bin:/busybox/bin

    kill -15 -1
    sleep 1
    kill -2 -1
    sleep 0.5
    kill -3 -1
    sleep 0.5
    kill -6 -1
    sleep 0.2
    kill -9 -1
    sleep 0.2

    pivot_root . ./new-root
    exec chroot . ./post-pivot
  '';
  systemStartup = pkgs.writeScript "startup" (''#!/bin/sh
  '' + startupScript);

  systemInstance = pkgs.runCommand "system-instance" {} ''
    mkdir -p "$out/bin"
    for i in ${systemInit} ${systemModprobe} ${backToInitrd} ${systemStartup} ${systemSetup} ${createGrubCfgScript} ${systemActivate} ${systemSwitch} ${systemBoot} ;
    do
      ln -s "$i" "$out/bin/$(basename "$i" | sed -e 's/^[^-]*-//')"
    done

    mkdir -p "$out/global"/bin
    mkdir -p "$out/global"/usr/bin

    ln -s "${_systemEtc}" "$out/global/etc"

    ln -s "${pkgs.bashInteractive}/bin/sh" "$out/global/bin/sh"
    ln -s "${pkgs.coreutils}/bin/env" "$out/global/usr/bin/env"

    mkdir -p "$out"/boot/for-bootloader
    ln -s ${stage1.bzImage} "$out"/boot/for-bootloader/${builtins.baseNameOf stage1.kernel}.linux.efi
    ln -s ${stage1.initrd}/initrd "$out"/boot/for-bootloader/${builtins.baseNameOf stage1.initrd}.initrd.efi
    echo '${systemGrubConfig}' | sed -e "s%@TARGET_SYSTEM@%$out%" > "$out"/boot/for-bootloader/grub.part.cfg
    ln -s ${stage1.kernelModules} "$out/boot/kernel-modules"
    ln -s ${stage1.initrd} "$out/boot/initrd-package"
    ln -s ${stage1.kernel} "$out/boot/kernel-package"
    ln -s ${stage1.kernel}/bzImage "$out/boot/kernel"
    ln -s ${stage1.initrd}/initrd "$out/boot/initrd"

    mkdir "$out/setuid"
    ln -s "${setuidPrograms}" "$out"/setuid/programs
    ln -s "/var/setuid-wrapper-storage/$(basename "${setuidPrograms}")" "$out/setuid/wrappers"

    ln -s "${systemPath}" "$out/sw"
  '';

  setuidWrapperSource = <nixpkgs> + "/nixos/modules/security/wrappers/wrapper.c";
  setuidPrograms = pkgs.runCommand "setuid-programs" {
     preferLocalBuild = true;
  } ''
    mkdir -p "$out"
    ${pkgs.gcc}/bin/gcc -Wall -O2 -DWRAPPER_DIR="\"/var/setuid-wrapper-storage/$(basename "$out")\"" ${setuidWrapperSource} -o "$out/canonical-wrapper" -L${pkgs.libcap.lib}/lib -I${pkgs.libcap.dev}/include -L${pkgs.libcap_ng}/lib -I${pkgs.libcap_ng}/include -lcap -lcap-ng
    ${pkgs.lib.concatMapStrings (x:
    let
      src = if builtins.isAttrs x then x.src else x;
      dst = if builtins.isAttrs x then x.dst else x;
    in
    ''
      cp "$out/canonical-wrapper" "$out/${dst}"
      (
        fullSrc="${systemPath}/bin/${src}"
        test -e "$fullSrc" && echo -n "$fullSrc" > "$out/${dst}.real"
      ) ||
      (
        fullSrc="${systemPath}/sbin/${src}"
        test -e "$fullSrc" && echo -n "$fullSrc" > "$out/${dst}.real"
      ) ||
      true
    '') setuidProgramList}
  '';

  _systemPackages = (with pkgs; [
    coreutils utillinux grub2_efi bashInteractive nix shadow
    _sinit iproute openssh curl procps gnugrep gnused gptfdisk
    cpio dhcp less nettools iw wpa_supplicant findutils parted
    gzip bzip2 xz e2fsprogs dosfstools glibc gnutar psmisc
    pam kbd lynx fuse fuse3 ncurses acl eudev kmod git
  ]) ++ (maybeCall systemPackages pkgs) ++ (maybeCall firmwarePackages pkgs);
  allOutputNames = builtins.attrNames
      (pkgs.lib.fold
        (a: b: b //
          (builtins.listToAttrs (map (x: {name = x; value = x;}) a.outputs or ["out"])))
        {} _systemPackages);
  systemPath = pkgs.buildEnv {
      name = "system-path";
      ignoreCollisions = true;
      paths = _systemPackages;
      pathsToLink = ["/"];
      extraOutputsToInstall = if allOutputs then allOutputNames else [];
  };

  _systemEtc = if systemEtc != null then systemEtc else
  pkgs.runCommand "etc" {} ''
    mkdir -p "$out"
    dd if=/dev/urandom bs=16 count=1 | od -t x1 -An | tr -d ' ' > "$out/machine-id"
    echo "root:x:0:0::/root:/bin/sh" >> "$out"/passwd
    echo "nobody:!:65534:65534::/var/empty:/bin/nologin" >> "$out"/passwd
    for i in $(seq -w 01 99); do
            echo "nixbld$i:!:300$i:30000::/var/empty:/bin/nologin" >> "$out"/passwd
    done
    echo "root:x:0:" >> "$out"/group
    echo "wheel:x:1:" >> "$out"/group
    echo "users:x:100:" >> "$out"/group
    echo "nogroup:x:65534:" >> "$out"/group
    echo "nixbld:x:30000:$(for i in $(seq -w 01 99); do echo nixbld$i; done | xargs | tr ' ' ',')" >> "$out"/group
    echo "root:!:1::::::" >> "$out/shadow"

    echo "127.0.0.1 localhost" >> "$out/hosts"
    echo "localhost" >> "$out/hostname"

    ln -s "${pkgs.tzdata}/share/zoneinfo" "$out"
    ln -s "${pkgs.tzdata}/share/zoneinfo/UTC" "$out"/localtime
    ln -s "${pkgs.iana_etc}/etc/protocols" "$out"
    ln -s "${pkgs.iana_etc}/etc/services" "$out"

    ln -s /proc/mounts "$out"/mtab

    mkdir -p "$out/ssl/certs"
    ln -s "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt" "$out/ssl/certs/ca-bundle.crt"
    ln -s "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt" "$out/ssl/certs/ca-certificates.crt"

    ln -s /var/dhcp/resolv.conf "$out/resolv.conf"
    ln -s /var/dhcp/resolv.conf.dhclient "$out/resolv.conf.dhclient"
  '';

  systemLayout = pkgs.runCommand "layout" {} ''
    mkdir -p "$out"/{boot,dev,sys,proc,var/log,var/db,var/dhcp,home,root,tmp,run}
    for i in usr bin etc; do
      ln -sfT /var/current-system/global/$i "$out/$i"
    done
  '';
}
