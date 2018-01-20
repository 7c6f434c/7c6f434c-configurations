rec {
  pkgs = import <nixpkgs> {};
  nixos = import <nixpkgs/nixos>;

  stage1 = import ./fat-initramfs.nix {
    mountScript = ''
      modprobe atkbd

      mount /dev/sda2 /new-root
      mount /dev/sda1 /new-root/boot
    '';
    firmwarePackages = pkgs: [];
    modprobeConfig = builtins.readFile ./modprobe.conf;
    blacklistUdevRules = ["80-net-name-slot.rules"];
  };

  swPieces = import ./system-sw-pieces.nix { inherit pkgs; };
  
  lispServerHelpers = with pkgs.lispPackages; buildLispPackage {
    inherit (pkgs) stdenv;
    inherit clwrapper;
    baseName = "server-helpers";
    buildSystems = ["server-helpers"];
    src = "" + ./lisp-server-helpers;
    description = "Local library for Common Lisp system server";
    deps = [iolib iterate local-time cl-ppcre bordeaux-threads];
    overrides = x: {
      postInstall = ''
        NIX_LISP_PRELAUNCH_HOOK='nix_lisp_run_single_form "(asdf:perform (quote asdf:monolithic-compile-bundle-op) :server-helpers)"' "$out"/bin/*-lisp-launcher.sh ""
      '';
    };
  };

  systemParts = {
    bin = import ./system-bin.nix {
      initScript = ''
        ${import ./system-lisp.nix { 
          deps = with pkgs.lispPackages; [
            lispServerHelpers
          ];
          code = ''(defvar *server-helpers-package* "${lispServerHelpers}") (load "${./system-lisp.lisp}")'';
        }} &
        ${import ./system-gerbil.nix { 
          deps = [];
        }} &
        while true; do /bin/sh -i; done
      '';
    };
    global = import ./system-global.nix {inherit systemEtc;};
    setuid = import ./system-setuid.nix {
      setuidPrograms = [
        { name = "su"; src="${pkgs.shadow}/bin/su"; setuid=true; }
        { name = "unix_chkpwd"; src="${pkgs.pam}/bin/unix_chkpwd.orig"; setuid=true; }
      ];
    };
    sw = pkgs.buildEnv rec {
      name = "system-path";
      paths = swPieces.corePackages ++ (with pkgs; [
        vim monotone screen
      ]) ++ (with stage1; [firmwareSet] ++ _kernelModulePackages);
      extraOutputsToInstall = swPieces.allOutputNames paths;
      ignoreCollisions = true;
      pathsToLink = ["/"];
    };
    services = etcPieces.deeplinkAttrset "system-services" {
      "from-nixos/openssh" = fromNixOS.serviceScript "sshd"
        {services.openssh.enable = true;};
      "udevd" = pkgs.writeScript "udevd" ''
          ${pkgs.eudev}/bin/udevd &
          ${pkgs.eudev}/bin/udevadm trigger --action add
          ${pkgs.eudev}/bin/udevadm settle
        '';
    };
  };
  systemInstance = import ./system-instance.nix {
    inherit pkgs;
    inherit stage1;
    inherit systemParts;
    kernelParameters = ["intel_pstate=disable"];
  };

  etcPieces = import ./system-etc-pieces.nix { inherit pkgs nixos; };

  inherit (etcPieces) fromNixOS;

  sessionVariables = {};

  systemEtc = pkgs.buildEnv {
    name = "system-etc";
    paths = [
      (etcPieces.timeEtc "UTC")
      (etcPieces.namesEtc "localhost")
      (etcPieces.authEtc {
        security.pam.services = {};
        environment.sessionVariables = sessionVariables;
      })
      (etcPieces.deeplinkAttrset "etc-udev" 
        (fromNixOS.etcSelectPrefix "udev/" (etcPieces.udevConf {})))
      etcPieces.mountEtc
      (etcPieces.deeplinkAttrset "etc-ssl"
        {
          "ssl/certs/ca-bundle.crt" = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
          "ssl/certs/ca-certificates.crt" = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
          "pki/tls/certs/ca-bundle.crt" =  "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
        })
      (etcPieces.deeplinkAttrset "etc-fonts"
       (fromNixOS.etcSelectPrefix "fonts/" {
          fonts = {
            enableDefaultFonts = true;
            fontconfig = {
              enable = true;
              hinting.autohint = true;
            };
          };
        }))
      (etcPieces.deeplinkAttrset "etc-nix"
        (fromNixOS.etcSelectPrefix "nix/" {}))
      (etcPieces.deeplinkAttrset "etc-cups"
        {"cups" = "/var/lib/cups";})
    ];
    pathsToLink = ["/"];
  };

  nixosTools = (import <nixpkgs/nixos/modules/installer/tools/tools.nix> 
    {inherit pkgs; config={}; modulesPath = null;}).config.system.build;
}
