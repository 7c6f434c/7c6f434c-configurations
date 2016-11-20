{
  pkgs, config, ...
}:
{
   services.xserver.enable = true;
   services.xserver.videoDrivers = ["nvidia"];
   nixpkgs.config.allowUnfree = true;

   boot = {
     loader.grub = {
       enable = true;
       version = 2;
       efiSupport = true;
       device = "nodev";
       copyKernels = true;
     };
   };
  
  fileSystems = [
    {
      mountPoint = "/";
      label = "NixOSRescue";
      fsType = "ext4";
    }
    {
      mountPoint = "/boot/";
      label = "RESCUE_EFI";
      fsType = "vfat";
    }
  ];
}
