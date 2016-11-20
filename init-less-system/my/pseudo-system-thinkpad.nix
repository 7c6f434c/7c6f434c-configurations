import ../generic/pseudo-system.nix {
  overrides = x: {
    initrdMountCommands = (builtins.readFile ./mount-partitions-thinkpad.sh) + x.initial.initrdMountCommands;
    nixBuildSandboxPaths = [ "/home/repos" ] ++ x.initial.nixBuildSandboxPaths;
    nixBuildUseSandbox = "true";
    nixBuildCores = "4";
    nixBuildMaxJobs = "8";
    nixGcKeepOutputs = "true";
    nixBinaryCaches = x.initial.nixBinaryCaches ++ [ "http://192.168.0.202:32062/" "http://127.0.0.1:32062/" ];
    systemPackages = x.initial.systemPackages ++ 
      (let z = 
      (import ../../package-groups.nix {
        pkgs = x.pkgs;
        myTexLive = import ../../texlive-set.nix x.pkgs;
        myKDE = x.pkgs.kde414;
        baseKernel = rec {
          kernel-to-use = import ../../kernel-options.nix {pkgs = x.pkgs;};
          kernel = kernel-to-use.baseKernel.kernelPackages.kernel;
          kernelPackages = x.pkgs.linuxPackagesFor kernel;
          extraModulePackages = kernel-to-use.baseKernel.extraModulePackages;
        };
        });
        zb = z.bootstrap;
        zcu = z.constantly_used;
        in 
          (x.lib.concatMap (y: (builtins.getAttr y zb)) (builtins.attrNames zb))
          ++
          (x.lib.concatMap (y: (builtins.getAttr y zcu)) (builtins.attrNames zcu))
          )
       ;
    extraOutputsToInstall = x.allOutputNames;
    initrdModules = [
      "btrfs" "vfat" "ext4" "nls-cp437" "nls-iso8859-1" "crc32c-generic"
      "dm-mod" "ahci" "sd-mod" "usb-storage" 
      "xhci-hcd" "xhci-pci" "ehci-hcd" "ohci-hcd" "uhci-hcd"
      "usbhid" "hid-generic" "evdev"
      "psmouse" "mousedev" "usbhid" "hid_generic" "atkbd" "xtkbd"
      {name = "loop"; args = ["max_loop=16"];}
      "configs"
      "fbcon" "i915"
    ] ++ x.initial.initrdModules;
    firmwarePackages = [
      x.pkgs.firmwareLinuxNonfree
    ];
    linuxLateModules = [
      "iwlwifi swcrypto=1" "iwldvm" "arc4" "ctr" "ccm" "hmac" "md4" "af-packet"
      "e1000e" "uvcvideo" "tpacpi" "thinkpad-acpi"
      "ac" "battery"
      "fuse" "tun"
      "coretemp" "button" "acpi-cpufreq" "thermal"
      "snd-usb-audio" "snd-hda-intel index=1"
      "aesni-intel" "kvm-intel"
    ] ++ x.initial.linuxLateModules;
    fontPackages = (import ../../fonts.nix {inherit (x) pkgs;}).fonts;
    gettyConsoles = ["tty2" "tty3" "tty4" "tty5" "tty6"]; 
    bindNixConfig = import ../../bind.nix {inherit (x) pkgs; config.networking.hostName = x.hostnameNix.hostname ;};
    hostnameNix = (import /root/nix-sysconfig/hostname.nix);
    hostName = "${x.hostnameNix.hostname}.${x.hostnameNix.domain}";
    initrdExtraToolset = [
      "${x.pkgs.e2fsprogs}/bin/mkfs.ext*" 
      "${x.pkgs.e2fsprogs}/bin/mke2fs" 
      "${x.pkgs.e2fsprogs.out}/lib/libcom_err.*so*"
      "${x.pkgs.e2fsprogs.out}/lib/libe2p.*so*"
      "${x.pkgs.e2fsprogs.out}/lib/libext2fs.*so*"
      "${x.pkgs.utillinux.out}/lib/libblkid.*so*"
      "${x.pkgs.utillinux.out}/lib/libuuid.*so*"
      "${x.pkgs.libunwind}/lib/libunwind-ptrace.so*"
      "${x.pkgs.libunwind}/lib/libunwind-x86*.so*"
      "${x.pkgs.libunwind}/lib/libunwind.so*"

      "${x.pkgs.hdparm}/bin/hdparm"
    ] ++ x.initial.initrdExtraToolset;
    extraEtcBuildCommands = ''
      ln -s ${../../sudoers} "$out/sudoers"
      ln -s /root/sudo-scripts "$out"/sudo-scripts
    '' + x.initial.extraEtcBuildCommands;
    profileText = x.initial.profileText + ''
      export EDITOR=vim

          # Provide a nice prompt.
          PROMPT_COLOR="1;31m"
          let $UID && PROMPT_COLOR="1;32m"
          PS1="\n\[\033[$PROMPT_COLOR\][\u@\h:\w]\\$\[\033[0m\] "
          if test "$TERM" = "xterm"; then
            PS1="\[\033]2;\h:\u:\w\007\]$PS1"
          fi
    '';
    setuidProgramList = x.initial.setuidProgramList ++ [
      "ping" "fbterm" "lsof" "pmount" "pumount" "udisks"
    ];
    systemActivationExtraCommands = ''
      mountpoint /sys/fs/fuse/connections || mount fuse -t fusectl /sys/fs/fuse/connections
    '';
    initrdPrePivotRootCommands = ''
      grep boot.debug.pivot-root /proc/cmdline && ash -i
    '';
    services = [
      (x.serviceDefinitions.openssh {enable = true;})
      (x.serviceDefinitions.nix-serve {enable = true; port = 32062;})
      (x.serviceDefinitions.postgresql {enable = true;})
      (x.serviceDefinitions.bind x.bindNixConfig)
      (x.serviceDefinitions.cron
        {crontab=x.lib.concatStringsSep "\n" (import ../../services-main.nix 
          {pkgs=null; config=null;}).cron.systemCronJobs;})
      (x.serviceDefinitions.cups {
        enable = true; gutenprint = true; drivers = with x.pkgs; [
          hplip foo2zjs foomatic_filters ghostscript cups_filters samba
        ];})
      (x.serviceDefinitions.xorg {
        fonts.fonts = x.fontPackages;
        hardware.opengl.driSupport = true;
        hardware.opengl.driSupport32Bit = true;
        services.xserver = {
          enableTCP = true;

          layout = "us(altgr-intl),ru(common),gr(basic)";			
          xkbOptions = "grp:caps_toggle, grp_led:caps, lv3:lwin_switch, terminate:ctrl_alt_bksp";
          enableCtrlAltBackspace = true;
          
          synaptics.enable = true;

          serverLayoutSection = ''
            Option "AIGLX" "true"
          ''; 
        };
      })
      (x.serviceDefinitions.udev {})
      (x.serviceDefinitions.dbus {})
      (x.pkgs.writeScript "wpa_supplicant" ''wpa_supplicant -c "''${1:-/root/src/rc/wpa_supplicant.conf}" -i wlan0 -D nl80211'')
      (x.pkgs.writeScript "dhclient-wlan0" ''dhclient -r wlan0; dhclient wlan0 -d'')
      (x.pkgs.writeScript "wlan" ''/var/current-system/bin/run-service wpa_supplicant; /var/current-system/bin/run-service dhclient-wlan0;'')
    ];
    autoServices = x.initial.autoServices ++ [
       "udev" "bind" "cron" "cups" "nix-serve" "openssh" "postgresql"
    ];
    extraUdevRules = [
      (x.pkgs.writeText "30-local-touchpad.rules" ''
        ACTION=="remove", GOTO="local_touchpad_end"
        SUBSYSTEM!="input", GOTO="local_touchpad_end"
        KERNEL!="event*", GOTO="local_touchpad_end"
        IMPORT{builtin}="input_id"
        ENV{ID_INPUT_TOUCHPAD}=="?*", SYMLINK+="input/touchpad%n"
        LABEL="local_touchpad_end"
      '')
    ] ++ [
      "${x.pkgs.eudev}/var/lib/udev/rules.d/60-evdev.rules"
      "${x.pkgs.eudev}/var/lib/udev/rules.d/60-persistent-input.rules"
    ];
  };
}
