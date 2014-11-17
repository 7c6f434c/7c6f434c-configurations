{...}:
[
  {
    mountPoint="/";
    label="SystemRoot";
    fsType="btrfs";
    neededForBoot=true;
    options="noatime";
  }
  {
    mountPoint="/root";
    label="Root";
    fsType="btrfs";
    neededForBoot=true;
    options="noatime";
  }
  {
    mountPoint="/boot";
    label="NIXOS_EFI";
    fsType="vfat";
    neededForBoot=true;
  }
  {
    mountPoint="/var";
    label="Var";
    fsType="btrfs";
    neededForBoot=true;
    options="noatime";
  }
  {
    mountPoint="/var/log";
    label="VarLog";
    fsType="btrfs";
    neededForBoot=true;
    options="noatime";
  }
  {
    mountPoint="/var/db";
    label="VarDB";
    fsType="btrfs";
    neededForBoot=true;
    options="noatime";
  }
  {
    mountPoint="/nix";
    label="Nix";
    fsType="btrfs";
    neededForBoot=true;
    options="noatime";
  }
  {
    mountPoint="/tmp";
    label="Tmp";
    fsType="ext4";
    neededForBoot=true;
    options="noatime";
  }
  {
    mountPoint="/home";
    label="Home";
    fsType="ext4";
    neededForBoot=true;
    options="noatime";
  }
    { mountPoint = "/sys/kernel/debug"; 
      device="debugfs";
      fsType = "debugfs";
    }
  ]
