{ jdk25, fetchurl, writeScriptBin }:
let 
  version = "6.3.0";
  hash = "sha256-Xywh6LiO13E07/yzpaOrE9GI9qPhbUATh/dHnpLbmqI=";
  jar = fetchurl {
    url = "https://github.com/jplag/jplag/releases/download/v${version}/jplag-${version}-jar-with-dependencies.jar";
    inherit hash;
  };
in
  writeScriptBin "jplag" ''
    "${jdk25}"/bin/java -jar "${jar}" "$@"
  ''
