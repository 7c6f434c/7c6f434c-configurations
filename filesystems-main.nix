{...}:
[
    { mountPoint = "/";
      device = "/dev/sda5";
      fsType = "ext3";

	# Enable POSIX Acess Control Lists and user-set
	# extended attributes in user.* namespace
      options = "acl,user_xattr";
    }
    { mountPoint = "/boot";
      device = "/dev/sda1";
      neededForBoot = true;
    }
    { mountPoint = "/nix/store";
      device = "/dev/sda11";
      neededForBoot = true;
      fsType = "btrfs";
      options = "noatime";
    }
    { mountPoint = "/home";
      device = "/dev/sda7";
      options = "acl,user_xattr";
      neededForBoot = true;
    }
    { mountPoint = "/tmp";
      device = "/dev/sda10";
      fsType = "btrfs";
      neededForBoot = true;
      options = "noatime";
    }
    { mountPoint = "/dev/shm";
      device = "memory";
      fsType = "tmpfs";
    }
  ]
