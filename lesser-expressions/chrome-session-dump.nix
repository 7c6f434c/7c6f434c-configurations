{ stdenv, fetchFromGitHub, go }:
stdenv.mkDerivation {
  pname = "chrome-session-dump";
  version = "0.pre-untagged-2022-07-15";
  src = fetchFromGitHub {
    owner = "lemnos";
    repo = "chrome-session-dump";
    rev = "22ec4169c97341cbf50355055020fe72271256ec";
    hash = "sha256-CMfW+n53P7RNmEWDLTKNG9YR4bvC6QyvOSwMUcPzqbM=";
  };
  buildInputs = [ go ];
  postPatch = ''
    sed -e '512d; 513s/^ */&fmt.Printf("Usage: /' -i chrome-session-dump.go
    sed -e 's@/usr/bin@/${placeholder "out"}/bin/@' -i Makefile
  '';
  preBuild = ''
    export HOME=$PWD
  '';
  preInstall = ''
    mkdir -p "$out/bin/"
  '';
}
