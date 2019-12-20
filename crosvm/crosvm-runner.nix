{
    pkgs ? import <nixpkgs> {}
  , spectrumPkgs ? import <nixpkgs-spectrum-os> {}
  , nixos ? import <nixpkgs/nixos>
  , lang-os ? ../lang-os
  , rootfs ? import ./rootfs.nix {
    inherit pkgs nixos;
  }
}:
pkgs.lib.makeExtensible (self: with self; {
  inherit pkgs nixos rootfs;

  rust-9p = pkgs.callPackage ./rust-9p.nix {};
  mktuntap = spectrumPkgs.mktuntap;
  crosvm = spectrumPkgs.crosvm;

  lispClientBin = pkgs.runCommand "lisp-request-client" {}
    ''
      export PATH="$PATH:${pkgs.sbcl}/bin:${pkgs.lispPackages.clwrapper}/bin"
      export USER_LISP_SHELL_RC=${./minimal-rc.lisp}
      export LISP_OS_HELPERS_PACKAGE=${(import "${lang-os}/local/test-system.nix" {}).lispOsHelpers}
      export HOME="$PWD"
      ${lang-os}/run-user-shell.sh --non-interactive \
        --eval "(build-shell \"$out\")"
    '';

  escapeLispString = s: ''"'' + (pkgs.lib.escape [''"'' ''\''] (builtins.toString s)) + ''"'';
  escapeLispStrings = pkgs.lib.concatMapStringsSep " " escapeLispString;
  inherit (pkgs.lib) escapeShellArg escapeShellArgs;

  lispRequest = code: ''
    ${lispClientBin} --non-interactive --eval ${escapeShellArg code}
  '';

  socket-9p-dir = ''tmp/9p-sockets'';
  socket-9p-dir-internal = ''/9p-sockets'';
  ensureSocketDir = ''
    mkdir -p "$HOME"/${escapeShellArg socket-9p-dir}
    ${lispRequest ''(ask-with-auth ()
         `(chown-subuser ,(~ ${escapeLispString socket-9p-dir}) "" t))''}
  '';

  store-9p-command = socket: path:
    ["${rust-9p}/bin/unpfs" "unix!${socket-9p-dir-internal}/${socket}!0" path];
  store-9p-export = ''
    ${ensureSocketDir}
    ${pkgs.socat}/bin/socat stdio \
       unix-connect:"$HOME"/${escapeShellArg socket-9p-dir}/'store\:0' \
       &> /dev/null < /dev/null ||
    {
    rm "$HOME"/${escapeShellArg socket-9p-dir}/'store:0'
    ${lispRequest ''
         (subuser-nsjail-x-application
            `(${escapeLispStrings (store-9p-command "store" "/nix/store")})
            :grant `(,(~ ${escapeLispString socket-9p-dir}))
            :mounts `((:b ,(~ ${escapeLispString socket-9p-dir}) ${escapeLispString socket-9p-dir-internal}))
            :wait nil
            )
    ''}
    }
    while ! test -e "$HOME"/${escapeShellArg socket-9p-dir}/store:0; do
      sleep 0.1;
    done
    ${ensureSocketDir}
  '';
  store-9p-exporter = pkgs.writeScript "store-9p-exporter" store-9p-export;

  host-address = "10.0.2.2";
  vm-address = "10.0.2.3";
  ip-netmask = "24";
  ip-version = "4";

  base-modules = [ "virtio-net" "9p" "9pnet" ];

  crosvm-base-script = ''
    for m in ${escapeShellArgs base-modules}; do
      modprobe $m
    done

    ip link set lo up
    ip link set eth0 up
    ip -${ip-version} address add ${vm-address}/${ip-netmask} dev eth0

    mount -t 9p -o version=9p2000.L,trans=tcp,cache=loose,ro ${host-address} /nix/store

    export PATH="$PATH:${rootfs.swEnv}/bin"

    source /dev/vdc
  '';

  pow2string = n: s: if n <= 0 then s else pow2string (n - 1) (s+s);

  crosvm-base-init-disk = pkgs.writeTextFile {
    name = "init";
    # Add enough spaces that rounding down to whole blocks is safe
    # Ideally, should add exactly the exact number of bytes needed
    text = crosvm-base-script + (pow2string 13 " ");
  };

  crosvm-init-disk-internal = "/crosvm-init";

  crosvm-extra-args = [];

  crosvmCommand =
  [
    "${mktuntap}/bin/mktuntap" "-i" "tap0" "-B" "-v" "-p" "3"
    "${crosvm}/bin/crosvm" "run"
    "--disk" rootfs.fsSquash
    "--disk" crosvm-base-init-disk
    "--disk" crosvm-init-disk-internal
    "--params" "root=/dev/vda init=/init ! source /dev/vdb"
    "--tap-fd" "3"
  ] ++ crosvm-extra-args ++ [
    "${rootfs.large_kernel}/bzImage"
  ];

  crosvm-launcher-code = ''
    ${store-9p-exporter}

    init_dir="''${TMPDIR:-/tmp}/crosvm-init-$USER/"
    mkdir -p "$init_dir"
    init_file="$(mktemp -p "$init_dir")"
    echo "$1" > "$init_file"
    yes "" | head -n 8192 >> "$init_file"
    
    chmod a+rX "$init_dir" "$init_file"
    
    ports="$2"

    export init_file ports

    ${lispRequest ''
         (subuser-nsjail-x-application
           `(${escapeLispStrings crosvmCommand})
           :pass-stdin t :pass-stderr t :pass-stdout t
           :dev-log-socket ""
           :x-optional t :display 999
           :mounts `(
                      (:b "/dev/kvm") (:r "/var/empty") (:b "/dev/net")
                      (:b ,(~ ${escapeLispString socket-9p-dir}) ${escapeLispString socket-9p-dir-internal})
                      (:r ,($ "init_file") ${escapeLispString crosvm-init-disk-internal})
                    )
           :grant-read `(
                          ,(~ ${escapeLispString socket-9p-dir})
                        )
           :grant      `(
                          ,(~ ${escapeLispString socket-9p-dir} "store:0")
                        )
           :network-ports `(
                             ((564 :tcp)${escapeLispString host-address}(${escapeLispString "${socket-9p-dir-internal}/store\\:0"} :unix))
                             ,@(loop for p in (cl-ppcre:split " " ($ "ports"))
                                     for proto := (if (cl-ppcre:scan ":" p) (cl-ppcre:regex-replace ".*:" p "") "tcp")
                                     for pnum := (parse-integer (cl-ppcre:regex-replace ":.*" p ""))
                                     collect `((,pnum ,proto) ${escapeLispString host-address} (,pnum ,proto) "127.0.0.1")
                                  )
                           )
           :netns-tuntap-devices `(("tap0" "${host-address}/${ip-netmask}" "tap" ("vnet_hdr" "${ip-version}")))
           )
    ''}
    ec="$?"
    rm "$init_file"
    exit "$ec"
  '';

  crosvm-launcher = pkgs.writeScript "crosvm-launcher" crosvm-launcher-code;
})
