rec {
  stage1 = import ./fat-initramfs.nix {
    mountScript = ''
      while ! test -e /dev/sda2; do
        sleep 0.1
      done
      mount -t ext4 /dev/sda2 /new-root
      mount /dev/sda1 /new-root/boot
    '';
    blacklistUdevRules = ["80-net-name-slot.rules"];
    modprobeConfig = (builtins.readFile ./modprobe.conf);
    firmwarePackages = [];
  };

  stage2 = import ./stage-2-template.nix {
    inherit stage1;
  };

  stage2small = import ./stage-2-template.nix {
    inherit stage1;
    allOutputs = false;
  };
}
