{ stdenv, lib, fetchFromGitHub, runCommandCC, runCommand,
strace,
cmake, doxygen, gperftools, pcre2, openblas, 
forceDynamic ? true}:
let 
  tcmalloc = gperftools; 
  bergamot = stdenv.mkDerivation (finalAttrs: {
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
 #+ (lib.optionalString enableHtmlMode
 #    ''
 #      cp app/bergamot.cpp app/bergamot-html.cpp
 #      sed -e '/ResponseOptions responseOptions;/a responseOptions.alignment = true\; responseOptions.HTML = true\; ' -i app/bergamot-html.cpp
 #      echo "
 #        add_executable(bergamot-html bergamot-html.cpp)
 #        target_link_libraries(bergamot-html PRIVATE bergamot-translator)
 #      " >> app/CMakeLists.txt
 #    '')
  + (lib.optionalString forceDynamic
      ''
        sed -e "s/STATIC/SHARED/" -i src/translator/CMakeLists.txt
      '')
  ;

  cmakeFlags = [ 
    "-DBLAS_FOUND=1" 
    "-DBLAS_INCLUDE_DIR=${lib.getDev openblas}/include" 
    "-DBLAS_LIBRARIES=-lblas" 
    "-DCBLAS_LIBRARIES=-lcblas" 
    "-DUSE_STATIC_LIBS=0"
    "-DCMAKE_TARGET_ARCHITECTURES=universal"
    "-DCMAKE_TARGET_ARCHITECTURE_UNIVERSAL=1"
    "-DBUILD_ARCH=x86-64"
  ];

  postInstall = ''
    mkdir -p "$out/lib"
    cp ./src/translator/lib* "$out/lib"
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

  passthru = {
    dev = runCommand "bergamot-dev" {} ''
      cd "${bergamot.src}"
    mkdir -p "$out/include/bergamot-translator"
    cp ./src/translator/*.h "$out/include/bergamot-translator"
    for dep in marian-dev ssplit-cpp; do
      ( 
        cd ./3rd_party/$dep/;
        find src -name '*.h*' -o -name '*.inl' | while read f; do
          mkdir -p "$out/include/bergamot-translator/3rd_party/$dep/$(dirname "$f")"
          cp "$f" "$out/include/bergamot-translator/3rd_party/$dep/$(dirname "$f")"
        done
      )
    done
      cp ./3rd_party/marian-dev/src/3rd_party/spdlog/details/format.cc "$out/include/bergamot-translator/3rd_party/marian-dev/src/3rd_party/spdlog/details/"
      cp ./src/translator/threadsafe_batching_pool.cpp "$out/include/bergamot-translator"

      ln -s "$out/include/bergamot-translator/3rd_party/marian-dev/src"/{*.*,data,common,training,graph,tensors,functional,optimizers,translator/*.h,models,layers,rnn} "$out/include/bergamot-translator/3rd_party/marian-dev/src/3rd_party"/sentencepiece "$out/include/bergamot-translator/3rd_party/marian-dev/src/3rd_party"/{spdlog,yaml-cpp,CLI,half_float} "$out/include/bergamot-translator/3rd_party/marian-dev/src/3rd_party/spdlog/details"/format.* "$out/include/bergamot-translator/3rd_party/ssplit-cpp/src/ssplit"/*.* "$out/include/bergamot-translator"
      ln -s "$out/include/bergamot-translator/3rd_party/marian-dev/src/3rd_party"/{pathie-cpp,zstr,zlib,yaml-cpp,CLI,any_type.h,phf,threadpool.h,sse_mathfun.h,mio} "$out/include/bergamot-translator/3rd_party"
      ln -s "$out/include/bergamot-translator" "$out/include/bergamot-translator/translator"
      '';
    translator = runCommandCC "bergamot-translator" {
      buildInputs = [ bergamot ];
      nativeBuildInputs = [ strace ];
    } ''
      mkdir -p "$out/bin"
      cp ${bergamot.src}/app/bergamot.cpp .
      "$CXX" bergamot.cpp -lbergamot-translator \
      -I"${bergamot.dev}/include/bergamot-translator" -L"${bergamot}/lib" \
      -o "$out/bin/bergamot"
      '';
    translator-html = runCommandCC "bergamot-translator" {
      buildInputs = [ bergamot ];
      nativeBuildInputs = [ strace ];
    } ''
      mkdir -p "$out/bin"
      cp ${bergamot.src}/app/bergamot.cpp .
      sed -e '
          /ResponseOptions responseOptions;/a responseOptions.alignment = true\; responseOptions.HTML = true\; 
      ' -i bergamot.cpp
      "$CXX" bergamot.cpp -lbergamot-translator \
      -I"${bergamot.dev}/include/bergamot-translator" -L"${bergamot}/lib" \
      -o "$out/bin/bergamot-html"
      '';
  };

  meta = {
    description = "Local CPU-friendly translation neural network library";
    homepage = "https://browser.mt/";
    license = lib.licenses.mpl20;
    maintainers = with lib.maintainers; [ raskin ];
    platforms = lib.platforms.all;
    };
  });
in 
  bergamot
