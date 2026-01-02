{ fetchFromGitHub, buildGoModule }:
# Has winner count issues
buildGoModule {
  name = "meek-stv";
  version = "0.pre-untagged-2022-11-29";
  src = fetchFromGitHub {
    owner = "blackgreen100";
    repo = "meek-stv";
    rev = "8cbb6461d5f160e0e6fc4a849163b842e6810581";
    hash = "sha256-REAT7mGIol37GiqJA/OWFB+4euJPIjwJWkwZvuGRJ/A=";
  };
  vendorHash = "sha256-r95PFkTywGiDIEnDfLpzt97SkuDeXo4xg2N7ikG0hs0=";
  postPatch = ''
    sed -e 's/os.Open(.*)/os.Open(os.Args[1])/' -i main.go
  '';
}
