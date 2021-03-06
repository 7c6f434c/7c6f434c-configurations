{pkgs, ...}: rec {
        #baseKernel = pkgs.kernel_2_6_33_zen1_bfs;
        #baseKernel = pkgs.kernel_2_6_36;
  baseKernel = rec{
    kernelPackages = pkgs.linuxPackagesFor pkgs.linux_latest;
    #kernelPackages = pkgs.linuxPackagesFor pkgs.linux_4_9;
    extraModulePackages = [kernelPackages.acpi_call /*kernelPackages.aufs */
      /*kernelPackages.sysdig*/
      kernelPackages.bbswitch
      /*kernelPackages.virtualbox*/
      ];
  };
}
