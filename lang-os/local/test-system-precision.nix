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
      
      sh ${./mount-partitions-precision.sh}
    '';
    modprobeConfig = ''
      blacklist nouveau
      blacklist i915
      blacklist xe
      blacklist iwlwifi
      
      ${builtins.readFile ./modprobe.conf}
    '';

   tools = pkgs: with pkgs; [ btrfs-progs ];

   firmwarePackages = p: [ (p.runCommand "needed-firmware" {} ''
     mkdir -p "$out"
     ( cd "${p.linux-firmware}"; find . -type f ) | grep -E 'i915' | ( cd "$out" ; 
       while read f; do
         mkdir -p "$(dirname "$f")"
         cp "${p.linux-firmware}/$f" "$f"
       done
     )
   '') ];
  });

  swPackages = super.swPackages ++ (with self.pkgs; [
    zsh pypy2 pypy3 expect linux-firmware
    alsa-utils alsa-tools mplayer rxvt-unicode
    (mlterm.override (x: {
      enableGuis = { 
        fb = true; 
        xlib = false;
        wayland = false;
        sdl2 = false;
        quartz = false;
      };
      desktopBinary = "true";
    }))
    (mlterm.override {enableFeatures = mlterm.enableFeatures // {ssh2 = false;};})
    kdePackages.konsole
    android-tools
    (adb-sync.override { androidenv = { androidPkgs = { platform-tools = android-tools;};};})
    powertop cryptsetup
    (self.pkgs.runCommand "local-keymap" {} ''
      mkdir -p "$out/share/keymaps/local/"
      cp ${./ru-en.map} "$out/share/keymaps/local/ru-en.map"
    '')
    pciutils wakeonlan 
    self.stumpwmWithDepsRunnable
  ]);

  stumpwmConfigDir = <src/lsp/stumpwm-config>;
  stumpwmContrib = <src/lsp/stumpwm-contrib>;
  stumpwmLangOS = <src/lsp/stumpwm>;

  stumpwmWithConfig = self.pkgs.runCommand "stumpwm-with-config" {} ''
    mkdir -p "$out/bin"
    cp -r "${self.stumpwmContrib}" "contrib"
    chmod u+rwX -R contrib
    export HOME="$PWD"
    ${self.sbcl-for-stumpwm.withPackages (x: [self.stumpwmWithDeps])}/bin/sbcl \
      --eval '(require :asdf)' \
      --eval '(require :stumpwm)' --eval '(in-package :stumpwm)' \
      --eval '(defvar stumpwm::*local-module-dir* "'"$PWD"'/contrib/")' \
      --eval '(defvar stumpwm::*langos* "${self.stumpwmLangOS}/")' \
      --load "${self.stumpwmConfigDir}"/setup.lisp \
      --load "${self.stumpwmConfigDir}"/all-defs.lisp \
      --eval '(sb-ext:save-lisp-and-die "'"$out"'/bin/stumpwm-with-config" 
        :executable t )'
    test -x "$out/bin/stumpwm-with-config"
  ''; 

  systemFonts = (import ./fonts.nix { inherit (self) pkgs; }).fonts;

  postgresql-package = self.pkgs.postgresql_19;

  fontconfigConfPackages = [ (self.pkgs.lib.hiPrio (self.pkgs.runCommand
    "fontconfig-kill-conf" {} ''
      mkdir -p "$out/etc/fonts/conf.d"
      mkdir -p "$out/etc/fonts/2.11/conf.d"
      for f in ; do
        for d in "$out/etc/fonts"/{,2.11}/conf.d/; do
          touch "$d/$f.conf"
        done
      done
    '')) ];

  fontConfigQuasiFonts = super.fontConfigQuasiFonts // {
    "DejaFallSans" = [
      "DejaVu Sans"
      "Free Sans"
      "Noto Sans"
      "emoji"
      "Noto Color Emoji"
    ];
    "DejaFallSerif" = [
      "DejaVu Serif"
      "Free Serif"
      "Noto Serif"
      "emoji"
      "Noto Color Emoji"
    ];
    "DejaFallMono" = [
      "DejaVu Sans Mono"
      "Free Sans Mono"
      "Noto Sans Mono"
      "emoji"
      "Noto Color Emoji"
    ];
  };

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
  
  openglPackages = with self.pkgs; [ 
    intel-vaapi-driver 
    libvdpau-va-gl libva-vdpau-driver
    mesa.opencl vulkan-loader libcap.lib
  ];

  nixOptions = super.nixOptions // {
    extraOptions = super.nixOptions.extraOptions or "" + ''
          gc-keep-outputs = true
          gc-keep-derivations = false
          experimental-features = nix-command

          secret-key-files = /nix/var/nix/cache-key.secret
    '';
    settings.trusted-substituters = [ "http://morefine:8008/" "ssh-ng://morefine" ];
    settings.trusted-public-keys = [ "buildbox-morefine-s500plus-20250322:VYZrnhIxa7NvILYl2Lme0NfKbJrthYyhtGk5/D+O0LA=" "raskin-thinkpad-20250322:+rGdt5KlHiDySpO3fwuJtr3+MBw/C7QYvgcQrTA+1rY=" "buildbox-morefine-s500plus-20260413:XWsjdGWXcp2x/ZOou2l2Uuug8b5PJd+UbTKXrNV3mdc=" "raskin-precision-20260730:DHY9R6BvTfNlZ8ARAeFWlrb8S8iPvTzJrLbxaMmc+sM=" ];
  };

  hostname = "prec-401a0bf1";

  setupScript = super.setupScript + ''
    ln -sf /var/current-system/graphics-drivers/* /run/
  '';

  systemLispSettings = ./system-lisp-settings-precision.lisp;
})

