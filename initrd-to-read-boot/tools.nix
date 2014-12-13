{pkgs ? import <nixpkgs> {}}:
with pkgs;
rec {
  init-hello = runCommand "init-hello" {} ''
    gcc ${./hello.c} -o "$out" -static
    strip -s "$out"
    ${nukeReferences}/bin/nuke-refs "$out"
  '';

  buildStaticC = name: extraParameters: source: 
    runCommand name {} ''
      gcc ${extraParameters} ${writeTextFile 
        { name = (name+".c"); 
          text = source;}} -o "$out" -static 
      strip -s "$out"
      ${nukeReferences}/bin/nuke-refs "$out"
    '';

  glibcLibsForToolset = [
    "${glibc}/lib/ld*.so*"
    "${glibc}/lib/libc[-.]*so*"
    "${glibc}/lib/libm[-.]*so*"
    "${glibc}/lib/librt[-.]*so*"
    "${glibc}/lib/libdl[-.]*so*"
    "${glibc}/lib/libpthread[-.]*so*"
    "${gcc.gcc}/lib/libgcc_s[-.]*so*"
    ];

  busyboxForToolset = [
    "${busybox}/bin/*"
  ];

  straceForToolset = ["${strace}/bin/strace"];

  eudevForToolset = [
    "${eudev}/bin/udev*"
    "${eudev}/lib/libudev.*so*"
    ];

  eudevForToolsetTargeted = [
    { object = "${eudev}/lib/udev/ata_id"   ; target = "bin/"; }
    { object = "${eudev}/lib/udev/cdrom_id" ; target = "bin/"; }
    { object = "${eudev}/lib/udev/scsi_id"  ; target = "bin/"; }
    { object = "${eudev}/lib/udev/mtdprobe" ; target = "bin/"; }
    ];

  lvmForToolset = [
    "${lvm2}/bin/*"
    "${lvm2}/lib/libdevmapper*.so*"
    ];

  mdadmForToolset = ["${mdadm}/bin/mdadm"];

  blkidForToolset = [
    "${utillinux}/sbin/blkid"
    "${utillinux}/lib/libblkid*.so*"
    "${utillinux}/lib/libuuid*.so*"
    ];

  kmodForToolset = [
    { object = "${kmod}/bin/kmod"; target = "bin/modprobe"; }
  ];

  closedToolset = {
    name, contents, extraCommands ? "", checkCommands ? ""
    , contentsWithTargets ? []
  }: stdenv.mkDerivation rec {
    inherit name;
    buildInputs = [nukeReferences];
    allowedReferences = ["out"];
    doublePatchelf = stdenv.isArm;
    targetedContents = map (x: x.object) contentsWithTargets;
    targets = map (x: x.target) contentsWithTargets;
    buildCommand = ''
      set +o pipefail

      mkdir "$out"
      cd "$out"

      mkdir bin
      mkdir lib
      ln -s bin sbin
      ln -s lib lib64
      ln -s lib lib32

      for ii in ${toString contents}; do
        for i in $(ls $ii); do
          irel="''${i#$NIX_STORE/*/}"
          ireldir="$(dirname "$irel")"
          mkdir -p "$ireldir"
          cp -f -r "$i" "$irel"
        done
      done

      declare -a targetedContents
      declare -a targets

      targetedContents=(${toString targetedContents})
      targets=(${toString targets})

      for iin in $(seq ${toString (builtins.length targetedContents)}); do
        ii="''${targetedContents[iin-1]}";
        tgt="''${targets[iin-1]}";
        echo $ii
        for i in $(ls $ii); do
          trel="''${tgt#$NIX_STORE/*/}"
          if [ "$trel" = "''${trel%/}" ]; then
            treldir="$(dirname "$trel")"
          else
            treldir="$trel"
          fi
          echo "$iin $ii $tgt $trel $treldir"
          mkdir -p "$treldir"
          cp -f -r "$i" "$trel"
        done
      done
      
      ${extraCommands}

      chmod -R u+w "$out"
      stripDirs "lib bin" "-s"
      
      for i in "$out"/bin/* "$out"/lib/*; do 
        if ! test -L "$i"; then 
          nuke-refs "$i";
        fi; 
      done

      for i in "$out"/bin/* "$out"/lib/lib*.so*; do
          if ! test -L "$i"; then
              echo "PatchELF $i"
              patchelf --set-interpreter "$out"/lib/ld*.so.? "$i" || true
              patchelf --set-rpath "$out"/lib "$i" || true
              if [ -n "$doublePatchelf" ]; then
                  patchelf --set-interpreter "$out"/lib/ld*.so.? --set-rpath "$out"/lib "$i" || true
              fi
          fi
      done

      echo "Looking for parasite glibc references"
      grep -rl "${glibc}" . || echo "Cleanup succesful"

      ${checkCommands}
    '';
  };

  makeBasicInitrd = { initScript, modules ? [], modulesTree ? [linux_latest]
  , extraToolset ? [], extraToolsetWithTargets ? [], extraToolsetCommands ? ""
  , extraContents ? []}: 
  let
    toolset = closedToolset {name = "tools";
          contents = glibcLibsForToolset ++ busyboxForToolset ++ extraToolset;
          contentsWithTargets = extraToolsetWithTargets;
          extraCommands = extraToolsetCommands;
        };
  in
    (makeInitrd {contents = [
      { object = toolset;
        symlink = "/init-tools";
      }
      {object = writeScript "init" initScript; symlink = "/init";}
    ]
    ++ extraContents
    ++ (lib.optional (modulesTree != [] && modules != []) 
        {object = (makeModulesClosure {
           allowMissing = true; 
           rootModules = (map (x: if builtins.isString x then x else x.name) modules);
           kernel = aggregateModules modulesTree;
          }) + "/lib/modules"; 
          symlink = "/lib/modules";
         })
    ;})//{
      kernel = builtins.head modulesTree;
      kernelModules = aggregateModules modulesTree;
      inherit toolset;
    };

  makeUdevRules = {ruleFiles, extraCommands, extraEnv ? {}}: runCommand "udev-rules"
  extraEnv
  ''
    mkdir -p "$out"
    ${toString (map (x: "cp ${x} \$out/ ;") ruleFiles)}
    ${extraCommands}

    # Let's just set the PATH correctly instead
    sed -r \
    -e 's@(IMPORT[{]program[}]=)"([a-z0-9A-Z])@\1"/sbin/\2@' \
    -e 's@"[-_/+.a-zA-Z0-9]*/(s?bin|lib/udev)/@"/init-tools/bin/@' \
    -e 's@, GROUP="[a-z0-9]*"@@' \
    -i "$out"/*.rules   
    # This comment appeases syntax highlighting for /* */

    # Default NixOS ships this fix to work around a Qemu misfeature
    substituteInPlace "$out"/*.rules \
      --replace ID_CDROM_MEDIA_TRACK_COUNT_DATA ID_CDROM_MEDIA
  '';

  storageUdevRules = [
    "${eudev}/lib/udev/rules.d/50-*.rules"
    "${eudev}/lib/udev/rules.d/60-cdrom*.rules"
    "${eudev}/lib/udev/rules.d/60-persistent-storage*.rules"
    "${lvm2}/lib/udev/rules.d/*.rules"
  ];

  makeUdevInitrd = { extraToolset ? [], extraToolsetWithTargets ? []
    , extraToolsetCommands ? ""
    , initScript ? "sh", extraUdevRules ? [], extraUdevRuleCommands ? ""
    , modulesTree ? [linux_latest]
    , modulesAvailable ? [], modules ? []}: makeBasicInitrd {
    modulesTree = modulesTree;
    extraContents = [ 
      { object = makeUdevRules { 
          ruleFiles = storageUdevRules ++ extraUdevRules;
          extraCommands = extraUdevRuleCommands;
        }; 
        symlink = "/etc/udev/rules.d";
      } 
    ];
    extraToolset = straceForToolset ++ eudevForToolset
      ++ lvmForToolset ++ mdadmForToolset ++ blkidForToolset
      ++ extraToolset
      ;
    extraToolsetWithTargets = kmodForToolset ++ eudevForToolsetTargeted
      ++ extraToolsetWithTargets;
    extraToolsetCommands = extraToolsetCommands;
    initScript = ''
      #!/init-tools/bin/sh

      # paths in the initrd
      export PATH=/init-tools/bin
      export MODULE_DIR=/lib/modules/

      if test -L "$0" ; then
        cp -L "$0" /init-file
        exec /init-file
      fi
      
      mkdir -p /var/log

      {
      # mount just /proc and /dev to load kernel modules earlier
      mount procfs /proc -t proc
      mount devtmpfs /dev -t devtmpfs -o size=32M

      echo "/init-tools/bin/modprobe" > /proc/sys/kernel/modprobe

      # load modules
      ${toString (
      map (x: 
          let
            modName = if builtins.isString x then x else x.name;
            modArgs = if builtins.isString x then [] else x.args;
          in
          ''
            modprobe "${modName}" ${toString modArgs} ;
          ''
      ) (modules ++ ["atkbd"])
      )}

      # mount kernel filesystems
      mkdir -m 0755 /run /tmp
      
      mount sysfs /sys -t sysfs
      mount run-tmpfs /run -t tmpfs --size=16M
      mount tmpfs /tmp -t tmpfs
      mkdir -m 0755 /dev/pts
      mount devpts /dev/pts -t devpts -o ptmxmode=0666

      # add some dummies just in case
      mkdir -p /etc
      touch /etc/fstab # to shut up mount
      touch /etc/mtab # to shut up mke2fs

      modprobe "unix"

      test -d "/sys/firmware/efi" && {
        modprobe fbcon
      }

      # make device nodes
      mkdir -p /run/udev/rules.d
      cp /etc/udev/rules.d/*.rules /run/udev/rules.d/
      udevd --daemon
      udevadm control --reload
      udevadm trigger -c add
      udevadm trigger
      udevadm settle || true

      targetSystem=$(cat /proc/cmdline | tr ' ' '\n' | grep targetSystem= | tr '=' '\n' | tail -n 1)
      if [ -n "$targetSystem" ]; then
        export PATH="$PATH:$targetSystem/sw/bin:$targetSystem/bin:/new-root/$targetSystem/sw/bin:/new-root/$targetSystem/bin"
      fi

      mkdir /new-root
      } 2>/var/log/early-boot-stderr.log >/var/log/early-boot-stdout.log 

      ${initScript}

      while true; do
        echo init script ended for some reason

        sh
      done
    '';
    modules = ["atkbd" "unix"] ++ modules ++ modulesAvailable;
  };

  qemuLauncherFun = makeOverridable ({initrd}: 
    writeScript "qemu-launcher" ''
      "$@" -kernel ${initrd.kernel}/bzImage \
           -initrd ${initrd}/initrd \
           -m 512
    '');

  typicalNotebookModules = [
    # USB
    "xhci-hcd" "ehci-hcd" "ohci-hcd" "uhci-hcd"

    "usb-storage" "usbhid" "hid-generic" "evdev"

    # loop devices
    {name = "loop"; args = ["max_loop=12"];}

    # for FS
    "nls-iso8859-1"
  ];
  typicalNotebookAvailableModules = [
    # Just for debugging
    "configs"

    # LVM
    "dm-mod"

    # FS
    "ext4" "ext3" "ext2" "btrfs" "vfat" "isofs" "squashfs" "loop"
    
    # ATA etc.
    "ahci" "sd-mod" "sr-mod" "cdrom" "ata-piix"

    # graphics
    "i915" "nouveau" "radeon" "fbcon"

    # FS dependencies
    "crc32c-generic" "nls-cp437"
  ];
  setuidWrapper = runCommand "setuid-wrapper" {} ''
    mkdir -p "$out/bin"
    gcc ${<nixpkgs>}/nixos/modules/security/setuid-wrapper.c \
      -Wall -O2 -DWRAPPER_DIR='"/var/setuid-wrappers"' \
      -o "$out"/bin/setuid-wrapper 
  '';

  cpioStatic = lib.overrideDerivation cpio (x: {LDFLAGS=["-static"];});
  gzipStatic = lib.overrideDerivation gzip (x: {LDFLAGS=["-static"];});

}
