{ stdenv, lib, fetchurl, writeTextFile, writeScriptBin, runCommand, bergamot }:
let 
  getModel = { 
    lang, vocabLang ? lang,
    modelType ? "base-memory",
    baseUrl ? 
    "https://media.githubusercontent.com/media/mozilla/firefox-translations-models/318943910ed065517c1e26b8e0783efdc64976ab/models/${modelType}", 
    prefixes ? {
      model = "model.";
      vocab = "vocab."; 
      lex = "lex.50.50.";
    },
    suffixes ? {
      model = ".intgemm.alphas.bin";
      vocab = ".spm"; 
      lex = ".s2t.bin";
    },
    hashes ? {
      model = lib.fakeHash;
      vocab = lib.fakeHash;
      lex = lib.fakeHash;
    },
    gzipped ? true,
    langDirs ? true
  }: (lib.makeExtensible (self: {
    original = lib.mapAttrs
    (k: v: fetchurl {
      url = "${baseUrl}${if langDirs then "/${lang}/" else "/"}" + 
      "${prefixes.${k}}${if k == "vocab" then vocabLang else lang}${suffixes.${k}}" + 
      "${if gzipped then ".gz" else ""}";
      hash = v;
    }) hashes;
    unpacked = if gzipped then lib.mapAttrs (k : v : 
    runCommand "${lib.head ((lib.match "(.*)[.]gz" v.name)++[v.name])}" {} ''
      gunzip < "${v}" > "$out"
    '') self.original else self.original;
    inherit lang;
    config = writeTextFile {
      name = "bergamot-config-${lang}.yml";
      text = ''
        bergamot-mode: native
        models:
          - ${self.unpacked.model}
        vocabs:
          - ${self.unpacked.vocab}
          - ${self.unpacked.vocab}
        shortlist:
          - ${self.unpacked.lex}
          - false
        beam-size: 1
        normalize: 1.0
        word-penalty: 0
        max-length-break: 128
        mini-batch-words: 1024
        workspace: 128
        max-length-factor: 2.0
        skip-cost: true
        cpu-threads: 0
        quiet: false
        quiet-translation: false
        gemm-precision: int8shiftAlphaAll
        alignment: soft
        '';
    };

    wrapper = writeScriptBin "bergamot-wrapper-${lang}" ''
      "${bergamot.translator}/bin/bergamot" --model-config-paths "${self.config}" "$@"
    '';
    wrapperHTML = writeScriptBin "bergamot-wrapper-html-${lang}" ''
      "${bergamot.translator-html}/bin/bergamot-html" --model-config-paths "${self.config}" "$@"
    '';
  }));
in
  {
    inherit getModel;
    fren = (getModel {
      lang = "fren";
      hashes = {
        model = "sha256-BrHu7dOUQmDQCjk9EuxxkkkMW3yTC5chL69NX1yQou4=";
        vocab = "sha256-jRWyGf/TIye0yr8NlKBaT+y4kjpPOGutvL5ehu3kU6c=";
        lex = "sha256-OVqndnIg4bz8CF8rJ4f/mABdAHXlVFHx4IMohePZZCo=";
      };
    });
    enfr = (getModel {
      lang = "enfr";
      hashes = {
        model = "sha256-Is2vPcmh2m5YGLVXZzcYJGfzzZfm/W+U5Cc+t7VtHj8=";
        vocab = "sha256-XcrarYGR+063Z3vOBwmKICId7JWXak23lvOhfMgrRUE=";
        lex = "sha256-BVmH8LWFthxJFPtEaeI5L9oLJdP3eEJ1l0+SDntjGZc=";
      };
    });
    ruen = (getModel {
      lang = "ruen";
      modelType = "tiny";
      hashes = {
        model = "sha256-Sop7mwfJ4GoWfsW/JUJSiBcyFRbbTt9hT9pFAR+o5dE=";
        vocab = "sha256-zXC4KOmeTQx51IzVbYV51lbIfB2yC/iIg9owhdy/73U=";
        lex = "sha256-ZST1yJjx/vUpkr0lZabUrPr7ak6NzWrvI3vYiCOUGKA=";
      };
    });
    enru = (getModel {
      lang = "enru";
      modelType = "base";
      hashes = {
        model = "sha256-PG4//SdclqIgrijdtVuMK4a0T/7MHutsgZXBU23krHQ=";
        vocab = "sha256-B+2QVTGfKtxQoWvH5jb+FUerdF65huQLY4SVbcH8bP0=";
        lex = "sha256-bwD12VW4slnLSnjlut8JWOX+884VP9d6beoDJFGLsbg=";
      };
    });
    deen = (getModel {
      lang = "deen";
      hashes = {
        model = "sha256-OLJFvPwj9tYf2YoYdDbqEyCjj0GJbfQRsQPfPoP1FoE=";
        vocab = "sha256-iNzX/b8g3GCDfOg3JE0NVbIPcHnyy9K6jmNTBjICgxA=";
        lex = "sha256-Ix6cr2Lg6Gd3XV9MH0z64h43xw2b8r1ij6RorusrqGk=";
      };
    });
    ende = (getModel {
      lang = "ende";
      hashes = {
        model = "sha256-46dEWPq6OrJ1Ltg1XF45efEzlSecj8G9P1SjwjFuSXU=";
        vocab = "sha256-Oy3b+sc/mU8VGomURH1rPUGrDTQ12lb3nCM/NNA+RcI=";
        lex = "sha256-A3DXnNpv7V36j5sqAAlFm9J2aJOCW7W4tRFBadGNpsQ=";
      };
    });
  }
