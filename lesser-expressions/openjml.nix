let pkgs = (import <nixpkgs> {}); in
pkgs.lib.makeExtensible (self: with self; {
  inherit pkgs;
  openjml-binary-archive = pkgs.fetchurl {
    url = "https://github.com/OpenJML/OpenJML/releases/download/21-0.6/openjml-ubuntu-22.04-21-0.6.zip";
    hash = "sha256-C2nk9tcoJtyMtUzVTRrhMsafRuotHAtwfvo7q5Pi0IE=";
  };
  openjml-binary-set = pkgs.runCommandNoCC "openjml" {} ''
    mkdir -p "$out/openjml"
    cd "$out/openjml"
    ${pkgs.lib.getExe pkgs.unzip} "${openjml-binary-archive}"
    patchShebangs *
    mkdir "$out/bin"
    find "$out/openjml" -type f -perm /0100 -maxdepth 1 -exec ln -s '{}' "$out/bin" ';'
    ( grep -rl '/lib64/ld-linux-x86-64[.]so[.]2' "$out/openjml" ) | xargs -l1 "${pkgs.lib.getExe pkgs.patchelf}" --set-interpreter "${pkgs.glibc}/lib64/ld-linux-x86-64.so.2" --set-rpath "${pkgs.lib.makeLibraryPath [pkgs.zlib "${placeholder "out"}/openjml/jdk" pkgs.gcc.cc]}"
    (  find "$out/openjml" -name '*.so' -type f ) | xargs -l1 "${pkgs.lib.getExe pkgs.patchelf}" --set-rpath "${pkgs.lib.makeLibraryPath [pkgs.zlib "${placeholder "out"}/openjml/jdk" pkgs.gcc.cc]}"
  '';
})
