{ stdenv, fetchFromGitHub, buildGoModule }:
buildGoModule (finalAttrs: {
  pname = "colorexp";
  version = "0.0-untagged-2023-05-04";

  src = fetchFromGitHub {
    owner = "EugenDueck";
    repo = "colorexp";
    rev = "2e608f95f710960c40125a623ed9306582f2006e";
    hash = "sha256-G8YwYG9lWEHwu9Xy/uj2991gNdqaQZt2ApIn9gUa9GA=";
  };

  vendorHash = "sha256-3PnXB8AfZtgmYEPJuh0fwvG38dtngoS/lxyx3H+rvFs=";

})
