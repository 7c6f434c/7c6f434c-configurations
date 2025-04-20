{ ollama, fetchFromGitHub, vulkan-loader, vulkan-headers, shaderc, libcap }:
ollama.overrideAttrs (x: {
  pname = "ollama-vulkan";
  version = "0.6.2-untagged-2025-03-23";

  src = fetchFromGitHub {
    owner = "grinco";
    repo = "ollama-vulkan";
    rev = "45dbd1464542a197f3b0af54f0ccd68b39fb2c1e";
    hash = "sha256-2cTPSJ1V/tN0Hy87Y6KGj6X+Pm/RwbrjAVqqkvWTOiA=";
  };

  vendorHash = "sha256-Zpzn2YWpiDAl4cwgrrSpN8CFy4GqqhE1mWsRxtYwdDA=";

  nativeBuildInputs = x.nativeBuildInputs ++ [
    shaderc
  ];

  buildInputs = x.buildInputs ++ [
    vulkan-loader vulkan-headers libcap
  ];
})
