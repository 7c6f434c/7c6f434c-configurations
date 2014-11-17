{...}:
[
  {
    mountPoint="/";
    label="root";
    fsType="btrfs";
    neededForBoot=true;
    options="noatime";
  }
  {
    mountPoint="/root";
    label="root-home";
    fsType="btrfs";
    neededForBoot=true;
    options="noatime";
  }
  {
    mountPoint="/boot";
    label="boot";
    fsType="ext3";
    neededForBoot=true;
    options="acl,user_xattr,noatime,data=ordered";
  }
  {
    mountPoint="/var";
    label="var";
    fsType="btrfs";
    neededForBoot=true;
    options="noatime";
  }
  {
    mountPoint="/var/log";
    label="var-log";
    fsType="btrfs";
    neededForBoot=true;
    options="noatime";
  }
  {
    mountPoint="/var/db";
    label="var-db";
    fsType="btrfs";
    neededForBoot=true;
    options="noatime";
  }
  {
    mountPoint="/nix";
    label="nix";
    fsType="btrfs";
    neededForBoot=true;
    options="noatime";
  }
  {
    mountPoint="/tmp";
    label="tmp";
    fsType="ext4";
    neededForBoot=true;
    options="noatime";
  }
  {
    mountPoint="/home";
    label="home";
    fsType="btrfs";
    neededForBoot=true;
    options="noatime";
  }
    { mountPoint = "/dev/shm";
      device = "memory";
      fsType = "tmpfs";
    }
    { mountPoint = "/sys/fs/fuse/connections"; 
      device="fuse";
      fsType = "fusectl";
    }
    { mountPoint = "/sys/kernel/debug"; 
      device="debugfs";
      fsType = "debugfs";
    }
  ]
