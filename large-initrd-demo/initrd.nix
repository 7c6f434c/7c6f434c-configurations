###
###   Note: this doesn't work as expected…
###


{pkgs ? import ./packages.nix {}}:
with pkgs;
rec {
  init-hello = runCommand "init-hello" {} ''
    gcc ${./hello.c} -o "$out" -static
    strip "$out"
  '';
  initrd = makeOverridable (
    {packages, script, kernelPackages, compressor ? "gzip -2"}: 
    let 
      kernelModules = (symlinkJoin "kernel-modules" kernelPackages);
    in
    makeInitrd {
      contents = 
      [ 
        { object = writeScript "init" (''
            #!/bin/sh
            export MODULE_DIR=${kernelModules}/lib.modules
            for i in ${toString packages}; do 
              export PATH="$PATH:$i/bin"
              [ -d "$i/sbin" ] && export PATH="$PATH:$i/sbin"
            done
          '' + script);
          symlink = "/init";
        }
        { object = stdenv.shell;
          symlink = "/bin/sh";
        }
        #{ object = stdenv.shell;
        #  symlink = "/init";
        #}
        #{
        #  object = init-hello;
        #  symlink = "/init";
        #}
      ] 
      #++
      #[{ object=kernelModules; symlink="../";}]
      ++
      (map (x: {object = x; symlink="../";}) packages);
      inherit compressor;
      }) {
    packages = [
      coreutils
      kmod
      vimTiny
    ];
    script = ''
      echo Hello
      uname -a
      echo $PATH
      /bin/sh -i
      '';
    kernelPackages = [ linux_latest ];
  }; 
  qemuLauncher = makeOverridable ({kernelPackages ? [ linux_latest ]}@x: 
    writeScript "qemu-launcher" ''
      "$@" -kernel ${builtins.head kernelPackages}/bzImage \
           -initrd ${initrd.override x}/initrd \
           -m 512
    '') { };
}
