{ stdenv, fetchFromGitHub, curl }:
stdenv.mkDerivation (finalAttrs: {
  pname = "tptp4X";
  version = "0.0-untagged-20250226";
  src = fetchFromGitHub {
    owner = "TPTPWorld";
    repo = "TPTP4X";
    rev = "914d3fd62fe7d6c0c2fe6416b10f5f5e1ede853f";
    hash = "sha256-NfORXfeX1aTcuYkKgXI1aZB7DYwFzPo/wwCG4mIgDw8=";
  };
  jjparser = fetchFromGitHub {
    owner = "TPTPWorld";
    repo = "JJParser";
    rev = "75ebf8b149728c4794ba81308709b6ee16f1e31";
    hash = "sha256-rJYwueDGWkC9qfSPHg2SH7ed6z1OVP1lM3sMa7xGMy0=";
  };
  prePatch = ''
    chmod u+rwX -R .
    rmdir JJParser
    cp -r ${finalAttrs.jjparser} JJParser
    chmod u+rwX -R .
    sed -re 's/return[(]"sumo"[)];/& break; case smt2: return("smt2"); /' -i JJParser/PrintTSTP.c
    cat JJParser/PrintTSTP.c
  '';
  buildInputs = [ curl ];
  installPhase = ''
    mkdir -p "$out"/bin
    cp tptp4X "$out/bin"
  '';
})
