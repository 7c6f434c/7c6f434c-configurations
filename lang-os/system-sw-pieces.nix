{
  pkgs ? import <nixpkgs> {}
}:
{
  corePackages = with pkgs; [
    coreutils utillinux grub2_efi bashInteractive nix shadow
    iproute openssh curl procps gnugrep gnused gptfdisk
    cpio dhcp less nettools iw wpa_supplicant findutils
    parted gzip bzip2 xz e2fsprogs dosfstools glibc gnutar 
    psmisc pam kbd lynx fuse fuse3 ncurses acl eudev kmod git
    strace efibootmgr gcc binutils socat rlwrap fbterm which
  ];
  allOutputNames = l: builtins.attrNames
      (pkgs.lib.fold
        (a: b: b //
          (builtins.listToAttrs (map (x: {name = x; value = x;}) a.outputs or ["out"])))
        {} l);
  cProgram = name: cfile: buildInputs: flags: pkgs.runCommandCC name { inherit buildInputs; } ''
    mkdir -p "$out/bin"
    cc "${cfile}" ${builtins.toString flags} -o "$out/bin/${name}" -Wall -Werror -Wpedantic
  '';
}
