(import ./test-system.nix {}).extend ( self: super: {
  stage1 = super.stage1.extend (s1self: s1super: {
    kernelPackages = pkgs: pkgs.linuxPackagesFor pkgs.linux_latest;

    mountScript = ''
      modprobe atkbd
      modprobe usbhid
      modprobe hid-generic
      modprobe hid-cherry
      modprobe mac-hid
      modprobe xhci-hcd
      modprobe ehci-hcd
      modprobe usb-storage
      modprobe uas
      
      sh ${./mount-partitions-usb-1.sh}
    '';
    modprobeConfig = ''
      blacklist nouveau
      blacklist iwlwifi
      
      ${builtins.readFile ./modprobe.conf}
    '';
  });

  swPackages = super.swPackages ++ (with self.pkgs; [
    zsh pypy2 pypy3 expect firmwareLinuxNonfree
    alsa-utils alsa-tools mplayer rxvt-unicode
    (mlterm.override (x: { enableGuis = { 
                 fb = true; 
                 xlib = true;
                 wayland = true;
                 sdl2 = true;
                 quartz = false;
               }; }))
    kdePackages.konsole
    androidenv.androidPkgs.platform-tools
    adb-sync
    powertop
  ]);

  systemFonts = (import ./fonts.nix { inherit (self) pkgs; }).fonts;

  fontconfigConfPackages = [ (self.pkgs.hiPrio (self.pkgs.runCommand
    "fontconfig-kill-conf" {} ''
      mkdir -p "$out/etc/fonts/conf.d"
      mkdir -p "$out/etc/fonts/2.11/conf.d"
      for f in ; do
        for d in "$out/etc/fonts"/{,2.11}/conf.d/; do
          touch "$d/$f.conf"
        done
      done
    '')) ];

  systemEtc = super.systemEtc.override (x: {
    paths = x.paths ++ [
      (super.etcPieces.deeplinkAttrset "etc-lvm" {
        "lvm" = "/var/etc/lvm";
      })
    ];
  });

  systemParts = super.systemParts // {
    services = self.etcPieces.deeplinkAttrset "systemServices"
    (super.systemParts.services.entries // {
      "from-nixos/gpm" = self.fromNixOS.serviceScript "gpm"
          { services.gpm.enable = true; services.gpm.protocol = "imps2"; };
    });
  };
  
  openglPackages = with self.pkgs; [ vaapiIntel libvdpau-va-gl vaapiVdpau ];
})

