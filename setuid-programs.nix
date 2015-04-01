{
  plainSetuidPrograms = [
    "fusermount"
    "mount" "umount" "sudo" "xlaunch"
    "lsof" "suid-chroot" "fbterm" "pmount"
    "pumount" "udisks" "su" "passwd"
    "lxc-execute" "init.lxc" "firejail"
  ];
  renamedSetuidPrograms = [
    {src="unix_chkpwd.orig";dst="unix_chkpwd";}
  ];
}
