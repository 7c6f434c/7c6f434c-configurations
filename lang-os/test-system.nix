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
  
  lispServerHelpers = import ./lisp-server-helpers.nix {
    inherit pkgs;
    src = "" + ./lisp-server-helpers;
    deps = with pkgs.lispPackages; [
      iolib iterate local-time cl-ppcre bordeaux-threads alexandria
      clsql clsql-sqlite3
    ];
  };

  systemLisp = import ./system-lisp.nix { 
          deps = with pkgs.lispPackages; [
            lispServerHelpers
          ];
          code = ''(defvar *server-helpers-package* "${lispServerHelpers}") (load "${./system-lisp.lisp}")'';
        };

  systemGerbil = import ./system-gerbil.nix { 
          deps = [];
	  code = ''
             (load "${./system-gerbil.scm}")
          '';
        };

  loopStarter = command: pkgs.writeScript "loop-starter" ''
          trap : 1 2 3 13 14 15
          while true; do
                ${command}
          done
  '';

  systemParts = {
    bin = import ./system-bin.nix {
      initScript = ''
        ${loopStarter "/run/current-system/services/language-daemon/system-lisp"} &
        ${loopStarter "/run/current-system/services/language-daemon/system-gerbil"} &
        chvt 2
      '';
      setupScript = ''
        mkdir -p /var/lib/cups /var/lib/ssh /var/empty
      '';
    };
    global = import ./system-global.nix {inherit systemEtc;};
    setuid = import ./system-setuid.nix {
      setuidPrograms = [
        { name = "su"; src="${pkgs.shadow.su}/bin/su"; setuid=true; }
        { name = "unix_chkpwd"; src="${pkgs.pam}/bin/unix_chkpwd.orig"; setuid=true; }
      ];
    };
    sw = pkgs.buildEnv rec {
      name = "system-path";
      paths = swPieces.corePackages ++ (with pkgs; [
        vim monotone screen
        sbcl gerbil
        (swPieces.cProgram "vtlock" ./c/vtlock.c [] [])
        (swPieces.cProgram "file-lock" ./c/file-lock.c [] [])
        (swPieces.cProgram "in-pty" ./c/in-pty.c [] [])
        (swPieces.cProgram "numeric-su" ./c/numeric-su.c [] [])
        lispServerHelpers
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
      "nix-daemon" = pkgs.writeScript "nix-daemon" ''
        ${pkgs.nix}/bin/nix-daemon
      '';
      "language-daemon/system-lisp" = systemLisp;
      "language-daemon/system-gerbil" = systemGerbil;
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
        security.pam.services = {
          sshd = {};
        };
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
      (etcPieces.deeplinkAttrset "etc-openssh"
        {"ssh" = "/var/lib/ssh";})
    ];
    pathsToLink = ["/"];
  };

  nixosTools = (import <nixpkgs/nixos/modules/installer/tools/tools.nix> 
    {inherit pkgs; config={}; modulesPath = null;}).config.system.build;
}
