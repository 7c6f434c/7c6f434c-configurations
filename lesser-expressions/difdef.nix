{ stdenv, fetchFromGitHub }:
stdenv.mkDerivation (finalAttrs: {
  pname = "difdef";
  version = "0.0-untagged-2023-05-17";
  # src = fetchFromGitHub {
  #   owner = "QuuxPlusOne";
  #   repo = "difdef";
  #   #rev = "418586e73865f402c226794b8aeaced5324addc2";
  #   rev = "8259e871e3c5a4f5c7663a4584a7bfa8b86a93cd";
  #   hash = "sha256-PyI0DhZyODwwFfr6mT3CwaG2aTtLWIRaL0joWsCDdW8=";
  # };
  src = /home/repos/difdef;

  doCheck = true;

  makeFlags = [
    "PREFIX=${placeholder "out"}"
    "CXX:=$(CXX)"
  ];
})
