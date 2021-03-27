{
  stdenv, lib, fetchFromGitHub, rust
}:

rust.packages.stable.rustPlatform.buildRustPackage rec {
  pname = "rust-9p";
  version = "0.0.2019-05-17";

  src = fetchFromGitHub {
    owner = "pfpacket";
    repo = pname;
    rev = "01cf9c60bff0f35567d876db7be7fb86032b44eb";
    sha256 = "0mhmr1912z5nyfpcvhnlgb3v67a5n7i2n9l5abi05sfqffqssi79";
  };

  sourceRoot = "source/example/unpfs";

  cargoSha256 = "045fb0q5d8frfjq83a3v4mgck3398l23rhzwk8qkdk48xv3j1d5i";

  RUSTC_BOOTSTRAP=1;

  postInstall = ''
    install -D -m 0444 ../../README* -t "$out/share/doc/${pname}"
    install -D -m 0444 ../../LICEN* -t "$out/share/doc/${pname}"
  '';

  meta = with lib; {
    description = "9P2000.L server implementation in Rust";
    homepage = "https://github.com/pfpacket/rust-9p";
    license = with licenses; [ bsd3 ];
    maintainers = with maintainers; [ raskin ];

    # macOS build fails: https://github.com/pfpacket/rust-9p/issues/7
    platforms = with platforms; linux;
  };
}
