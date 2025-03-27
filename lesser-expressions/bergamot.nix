{ stdenv, lib, fetchFromGitHub, cmake, doxygen, gperftools, pcre2, openblas, forceDynamic ? true, enableHtmlMode ? true}:
let 
  tcmalloc = gperftools; 
in
stdenv.mkDerivation (finalAttrs: {
  pname = "bergamot-translator";
  version = "0.4.5-untagged-2024-05-12";

  nativeBuildInputs = [ cmake doxygen ];
  buildInputs = [ tcmalloc pcre2 openblas ];

  postPatch = ''
    mkdir -p 3rd_party/marian-dev/.git/logs
    echo 'nix-package-build' > 3rd_party/marian-dev/.git/logs/HEAD
    echo '#define GIT_REVISION "nix-package-build 1970-01-01T00:00:00Z"' > 3rd_party/marian-dev/src/common/git_revision.h
    chmod a-w 3rd_party/marian-dev/src/common/git_revision.h

    # Mkdir "$NIX_BUILD_TOP/fake-bin"
    # echo -e '#! /bin/sh\ntrue\n' > "$NIX_BUILD_TOP/fake-bin/git"
    # chmod a+x "$NIX_BUILD_TOP/fake-bin/git"
    # export PATH="$NIX_BUILD_TOP/fake-bin:$PATH"
  ''
  + (lib.optionalString enableHtmlMode
      ''
        cp app/bergamot.cpp app/bergamot-html.cpp
        sed -e '/ResponseOptions responseOptions;/a responseOptions.alignment = true\; responseOptions.HTML = true\; ' -i app/bergamot-html.cpp
        echo "
          add_executable(bergamot-html bergamot-html.cpp)
          target_link_libraries(bergamot-html PRIVATE bergamot-translator)
        " >> app/CMakeLists.txt
      '')
  + (lib.optionalString forceDynamic
      ''
        sed -e "s/STATIC//" -i src/translator/CMakeLists.txt
        sed -e "s/PRIVATE//" -i app/CMakeLists.txt
      '')
  ;

  cmakeFlags = [ 
    "-DBLAS_FOUND=1" 
    "-DBLAS_INCLUDE_DIR=${lib.getDev openblas}/include" 
    "-DBLAS_LIBRARIES=-lblas" 
    "-DCBLAS_LIBRARIES=-lcblas" 
  ];

  makeFlags = ["VERBOSE=1"];

  postInstall = ''
    mkdir -p "$out/bin"
    cp ./app/bergamot "$out/bin"
    cp ./app/bergamot-html "$out/bin" || true
    mkdir -p "$out/lib"
    cp ./src/translator/lib* "$out/lib"
    mkdir -p "$out/include/translator"
    cp ../src/translator/*.h "$out/include/translator"
  '';

  # env.dontStrip = 1;
  # env.NIX_CFLAGS_COMPILE = "-g";

  src = fetchFromGitHub {
    owner = "browsermt";
    repo = "bergamot-translator";
    rev = "9271618ebbdc5d21ac4dc4df9e72beb7ce644774";
    sha256 = "sha256-VWKFSxvCH7fVSABMieBWlA0knLYWLI61MyVqoZa6Pm4=";
    fetchSubmodules = true;
  };
})
