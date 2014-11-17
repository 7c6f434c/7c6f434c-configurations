{pkgs, config, ...}:

with pkgs.lib;

{
  options = {
  };

  config = {
    boot={
      kernelModules = [
        "fbcon" "i915"
        "acpi-cpufreq"
        "cpufreq-ondemand"
        "coretemp"
        "battery"
        "xhci-hcd"
        "uvcvideo"
        "snd-hda-intel power_save=5 index=1,0"
	"snd-pcm"
        "kvm-intel"
        "aesni-intel"
        "crc32c-intel"
        "intel-powerclamp"
        "ath9k"
	"asus-nb-wmi"
      ];
      extraModulePackages = [ config.boot.kernelPackages.bbswitch ];
      initrd = {
        kernelModules = [
          "fbcon" "i915"
        ];
      };
    };

    hardware.bluetooth.enable = mkDefault true;
    hardware.bumblebee.enable = mkDefault true;

    # Intel Core i7 -- 4500U
    nix.maxJobs = mkDefault 4;

    services.xserver = {
      videoDrivers = ["intel" "i965"];
      # TODO: find the right model or add one for it.
      xkbModel = "pc105";

      synaptics = mkDefault {
        enable = true;
        minSpeed = "0.7";
        maxSpeed = "3";
	accelFactor = "0.05";
      };

      # Intel Corporation Haswell-ULT Integrated Graphics Controller
      driSupport = true;
      resolutions = [{x = 1920; y=1080;}];

      defaultDepth = 24;
    };


    powerManagement = {
      enable = mkDefault true;
      powerUpCommands = ''
        ${pkgs.hdparm}/sbin/hdparm -B 255 /dev/sda
      '';
    };
  };
}


