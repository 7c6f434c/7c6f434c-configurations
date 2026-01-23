{ buildGoModule, fetchFromGitHub, go }:
buildGoModule (finalAttrs: {
  pname = "emm";
  version = "0.9.15-untagged-2025-12-09";
  src = fetchFromGitHub {
    owner = "etkecc";
    repo = "emm";
    rev = "4eff5721c609c4aeb6ce5573485d7239720b49b4";
    hash = "sha256-zuRM8zfpaipr5YAGIZRvb5ymzl08Xh3mUv7K3N78qyc=";
  };
  vendorHash = null;
})
