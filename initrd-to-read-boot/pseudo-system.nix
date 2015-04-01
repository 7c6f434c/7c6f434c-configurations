{ pkgs ? import <nixpkgs> {}
  , tools ? import ./tools.nix { inherit pkgs; }
  , initrds ? import ./initrd.nix { inherit pkgs tools; }
  , services ? import ./services.nix { inherit pkgs; }
  }:
with 
pkgs // tools // initrds;
rec {
  kernel-to-use = (import ../kernel-options.nix pkgs).baseKernel;
  initrd = initrdSh.override (x: {
    modulesTree = [kernel-to-use.kernelPackages.kernel] ++ 
      kernel-to-use.extraModulePackages;
    modules = [ 
    "i915" "ahci" "sd-mod" 
    ] ++ x.modules ++ [
    "btrfs" "ext4" "nls-cp437" "vfat"
    ];
    extraToolset = [
      "${e2fsprogs}/bin/mkfs.ext4" 
      "${e2fsprogs}/bin/mke2fs" 
      "${e2fsprogs}/lib/libcom_err.*so*"
      "${e2fsprogs}/lib/libe2p.*so*"
      "${e2fsprogs}/lib/libext2fs.*so*"
      "${utillinux}/lib/libblkid.*so*"
      "${utillinux}/lib/libuuid.*so*"

      "${hdparm}/bin/hdparm"
      ];
    initScript = (builtins.readFile ../mount-my-partitions.sh) 
      + "/init-tools/bin/sh -i; /run/current-system/sw/bin/sh -i"
      ;
  });
  package-groups = import ../package-groups.nix {
    pkgs = pkgs;
    myTexLive = import ../texlive-set.nix pkgs;
    myKDE = pkgs.kde414;
    baseKernel = rec {
      kernel = linux_latest;
      kernelPackages = linuxPackagesFor kernel kernelPackages;
      extraModulePackages = kernel-to-use.extraModulePackages;
    };
  };
  system-packages = with package-groups; 
    [(runCommand "empty" {} "mkdir $out")] 
    ++ (lib.concatLists (lib.attrValues bootstrap))
    ++ (lib.concatLists (lib.attrValues constantly_used))
    ++ nixosDefault
    ;
  system-sw = buildEnv {
    name = "system-software-set";
    ignoreCollisions = true;
    paths = system-packages;
  };
  setuidPrograms = import ../setuid-programs.nix;
  system = runCommand "system" {
    preferLocalBuild = true;
  } ''
    makeLink() {
      mkdir -p "$out/$(dirname "$2")"
      if test "$(basename "$2")" = .; then
        ln -sf "$1" "$out/$2"
      else
        rm -f "$out/$2"
        ln -sfT "$1" "$out/$2"
      fi
      test -e "$1"
    }

    makeLink ${system-sw} sw
    makeLink ${initrd} boot/initrd-package
    makeLink ${initrd}/initrd boot/initrd
    makeLink ${initrd.kernel} boot/kernel-package
    makeLink ${initrd.kernel}/bzImage boot/kernel
    makeLink ${initrd.kernelModules} boot/kernel-modules

    makeLink ${services.bindConfig} services/configs/bind/bind.conf
    makeLink ${services.bindScript} services/scripts/bind
    
    makeLink ${services.postgresqlConfigHBA}   services/configs/postgresql/pg_hba.conf
    makeLink ${services.postgresqlConfigIdent} services/configs/postgresql/pg_ident.conf
    makeLink ${services.postgresqlConfig}      services/configs/postgresql/postgresql.conf
    makeLink ${services.postgresqlScript}      services/scripts/postgresql

    makeLink ${services.nixBinaryCacheScript}  services/scripts/nix-binary-cache
    makeLink ${services.gpmScript}             services/scripts/gpm

    makeLink ${services.udevConfigs}/etc/udev/rules.d  services/configs/udev
    makeLink ${services.udevScript}                    services/scripts/udev

    makeLink ${services.XorgConfig}        services/configs/xorg/xorg.conf
    makeLink ${services.XorgScript}        services/scripts/xorg

    makeLink ${services.sshdConfig}        services/configs/sshd/sshd_config
    makeLink ${services.sshdScript}        services/scripts/sshd

    makeLink ${services.cronScript}        services/scripts/cron
    
    makeLink ${services.gogocScript}       services/scripts/gogoc

    makeLink ${services.cupsCupsdConfig}   services/configs/cups/cupsd.conf
    makeLink ${services.cupsFilesConfig}   services/configs/cups/files.conf
    makeLink ${services.cupsScript}        services/scripts/cups

    makeLink ${services.fontsConf} services/configs/fontconfig/fonts.conf
    makeLink ${services.fontsConf} etc/fonts/fonts.conf

    makeLink ${setuidWrapper} "setuid/wrapper"

    for i in ${toString setuidPrograms.plainSetuidPrograms}; do
      makeLink "$out/sw/bin/$i"  setuid/programs/. ||
      makeLink "$out/sw/sbin/$i" setuid/programs/. ||
      true
    done
    ${lib.concatMapStrings (x: ''
      makeLink  "$out/sw/bin/${x.src}" setuid/programs/"${x.dst}" ||
      makeLink "$out/sw/sbin/${x.src}" setuid/programs/"${x.dst}" ||
      true
    '') (setuidPrograms.renamedSetuidPrograms or [])}

    makeLink ${cpioStatic}/bin/cpio static-tools/cpio
    makeLink ${gzipStatic}/bin/gzip static-tools/gzip

    makeLink ${
      writeScript "init" ("#! ${stdenv.shell}\n"
        + ''"$(dirname "$0")"/setup'' + "\n"
        + (builtins.readFile ../system-init.sh)
        )
    } bin/init

    makeLink ${
      writeScript "setup" ("#! ${stdenv.shell}\n"
        + (builtins.readFile ./system-setup.sh)
        + (builtins.readFile ../system-setup.sh)
        )
    } "bin/setup"
    
    makeLink ${
      writeScript "modprobe" ("#! ${stdenv.shell}\n" +
        ''
          export PATH="$(${coreutils}/bin/dirname "$0")/../sw/bin"
          export MODULE_DIR="$(dirname "$0")/../boot/kernel-modules/lib/modules"
          echo "$@" > /dev/kmsg
          modprobe "$@"
        ''
        )
    } "bin/modprobe"

    makeLink ${
      writeScript "run-service"
      ''#! ${stdenv.shell}
          export theSystem="$(${coreutils}/bin/dirname "$0")/..";
          export PATH="$PATH:$theSystem/sw/bin"
          script="$1"
          shift
          "$theSystem"/services/scripts/"$script" start "$@" \
            0</dev/null                                 \
            1>/var/log/services/"$script"-stdout-"$(date -u +%Y%m%d-%H%M%S)" \
            2>/var/log/services/"$script"-stderr-"$(date -u +%Y%m%d-%H%M%S)" \
            ;
      ''
    } "bin/run-service"

    makeLink ${writeText "sudoers" (builtins.readFile ../sudoers)} "etc/sudoers"

    makeLink ${cacert}/etc/ca-bundle.crt    etc/ssl/certs/ca-bundle.crt

    makeLink ${services.nixConf}           etc/nix/nix.conf
    makeLink ${services.nixMachinesConf}   etc/nix/machines
    
    for i in /var/cert/nix/signing-key.{pub,sec,sec.pub}; do
      makeLink $i etc/nix/. || true
    done

    makeLink ${services.profileScript} etc/profile
    makeLink ${tzdata}/share/zoneinfo etc/zoneinfo

    makeLink ${iana_etc}/etc/protocols etc/protocols
    makeLink ${iana_etc}/etc/services  etc/services

    makeLink ${services.pamLoginConf}  etc/pam.d/login
    makeLink ${services.pamLoginConf}  etc/pam.d/xscreensaver
    makeLink ${services.pamLoginConf}  etc/pam.d/cups
    makeLink ${services.pamLoginConf}  etc/pam.d/xlock
    makeLink ${services.pamLoginConf}  etc/pam.d/vlock
    makeLink ${services.pamSuConf}     etc/pam.d/su
    makeLink ${services.pamSuConf}     etc/pam.d/sudo
    makeLink ${services.pamSshdConf}   etc/pam.d/sshd
    makeLink ${services.pamPasswdConf} etc/pam.d/passwd
    makeLink ${services.pamSuConf} etc/pam.d/groupadd
    makeLink ${services.pamSuConf} etc/pam.d/useradd
    makeLink ${services.pamSuConf} etc/pam.d/usermod
    makeLink ${services.pamSuConf} etc/pam.d/chsh

    makeLink ${
      writeText "ssh_config" ''
        XAuthLocation ${xorg.xauth}/bin/xauth
        ConnectTimeout 3
      ''
    } etc/ssh/ssh_config

    makeLink ${
      writeText "crontab" ''
        SHELL=${stdenv.shell}
        PATH=/run/current-system/sw/bin:/run/current-system/sw/sbin
        NIX_CONF_DIR=/etc/nix
        ${
        lib.concatStringsSep "\n" (import ../services-main.nix {pkgs=pkgs; config={};}).cron.systemCronJobs
        }
      ''
    } etc/crontab
  '';
  qemuScript = qemuLauncherFun { initrd = initrd ; };
}
