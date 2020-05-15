{
    pkgs ? import <nixpkgs> {}
  , nixos ? import <nixpkgs/nixos>
}:
pkgs.lib.makeExtensible (self: with self; {
  inherit pkgs nixos;

  makeSquashfs = pkgs.callPackage <nixpkgs/nixos/lib/make-squashfs.nix>;

  base_kernel = pkgs.linux;
  nixpkgs_kernel_lib = (import <nixpkgs/lib/kernel.nix> { inherit (pkgs) lib; });
  large_kernel = 
    base_kernel.override {
      structuredExtraConfig = with nixpkgs_kernel_lib; {
         VIRTIO_PCI = yes;
         VIRTIO_BLK = yes;
         DEVTMPFS_MOUNT = yes;
         SQUASHFS = yes;
      };
    };

  tool_paths = with pkgs; [
        (lowPrio busybox)
        kmod bashInteractive coreutils
        gnugrep gnused eudev strace utillinux e2fsprogs
  ];

  initTools = pkgs.buildEnv {
    name = "init-tools";
    paths = tool_paths;
    ignoreCollisions = true;
    pathsToLink = ["/"];
    extraOutputsToInstall = ["bin" "out"];
  };

  initScript = ''
    export PATH="${initTools}/bin"
    mkdir -p /proc
    mount procfs -t proc /proc
    cmdline="$(cat /proc/cmdline)"
    cmd="''${cmdline#*!}"
    test "$cmd" != "$cmdline" && eval "$cmd"
    bash -i
  '';

  init = pkgs.writeScript "init" ("#!/bin/sh\n" + initScript);

  udevPackages = with pkgs; [ eudev lvm2 libinput ];

  blacklistUdevRules = [];

  udevRules = pkgs.runCommand "udev-rules" {} ''
    mkdir -p "$out"/etc/udev/{rules.d,hwdb.d}
    for i in ${toString udevPackages}; do
      test -d "$i"/etc/udev/rules.d/ && ln -sf "$i"/etc/udev/rules.d/* "$out"/etc/udev/rules.d
      test -d "$i"/var/lib/udev/rules.d/ && ln -sf "$i"/var/lib/udev/rules.d/* "$out"/etc/udev/rules.d
      test -d "$i"/lib/udev/rules.d/ && ln -sf "$i"/lib/udev/rules.d/* "$out"/etc/udev/rules.d
      test -d "$i"/etc/udev/hwdb.d/ && ln -sf "$i"/etc/udev/hwdb.d/* "$out"/etc/udev/hwdb.d
      test -d "$i"/var/lib/udev/hwdb.d/ && ln -sf "$i"/var/lib/udev/hwdb.d/* "$out"/etc/udev/hwdb.d
      test -d "$i"/lib/udev/hwdb.d/ && ln -sf "$i"/lib/udev/hwdb.d/* "$out"/etc/udev/hwdb.d
    done
    for i in ${toString blacklistUdevRules}; do
      rm "$out/etc/udev/rules.d/$i" || true
    done
    ${pkgs.eudev}/bin/udevadm hwdb -u -r "$out"
  '';

  kernelModulePackages = kernel: [ kernel ];
  kernelModules = kernel: (pkgs.aggregateModules (kernelModulePackages kernel));

  fsContents = kernel: [
      { object = init; symlink = "/init";}
      { object = (kernelModules kernel) + "/lib/modules"; symlink = "/lib/modules"; }
      { object = initTools; symlink = "/init-tools"; }
      { object = pkgs.busybox; symlink = "/busybox"; }
      { object = initTools + "/bin/bash"; symlink = "/bin/sh"; }
      { object = initTools + "/bin/env"; symlink = "/usr/bin/env"; }
      { object = udevRules + "/etc/udev"; symlink = "/etc/udev"; }
    ];

  extraDirs = ["/dev" "/tmp" "/sys" "/proc" "/mnt" "/media" "/etc"
    "/home" "/var/log" "/var/db" "/run"];

  initramfs = pkgs.makeInitrd {
    contents = fsContents base_kernel;
  };

  fsImage = pkgs.runCommand "bootfs" {} ''
    mkdir "$out"
    cd "$out"
    ${pkgs.lib.concatMapStringsSep "\n"
    (x: ''
          target="${x.symlink}";
          test -n "$target" || target="/extra-objects/$(basename "${x.object}")";
          mkdir -p "$(dirname "./$target")"
          ln -s ${x.object} "./$target"
        '')
    (fsContents large_kernel)}

    ${pkgs.lib.concatMapStringsSep "\n"
    (x: ''
          mkdir -p "./${x}"
        '')
    extraDirs}
  '';

  fsTarball = pkgs.runCommand "bootfs.tar.gz" {} ''
    cd "${fsImage}"
    grep -v "$(pwd)" "${pkgs.writeReferencesToFile fsImage}" |
    xargs tar -cP -z -f "$out" --owner root:0 --group root:0 --hard-dereference * 
  '';

  fsSquash = pkgs.runCommand "bootfs.squashfs" {} ''
    gunzip < "${fsTarball}" | ${pkgs.squashfs-tools-ng}/bin/tar2sqfs "$out"
  '';

  swPackages = with pkgs; [curl host iotop htop (hiPrio glibcLocales)
    screen git cacert];
  swEnv = pkgs.buildEnv {
    name = "main-tools";
    paths = swPackages;
    ignoreCollisions = true;
    pathsToLink = ["/"];
    extraOutputsToInstall = ["bin" "out"];
  };
})
