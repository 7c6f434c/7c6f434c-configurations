{ stdenv, lib, fetchurl, writeTextFile, writeScriptBin, runCommand, bergamot }:
let 
  getModel = { 
    lang, vocabLang ? lang,
    baseUrl ? 
    "https://media.githubusercontent.com/media/mozilla/firefox-translations-models/main/models/prod", 
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
        model = "sha256-T1YYuT0IvYK9r8CWtc3+RZ4ksmPwpaI9z2QaBw67YLU=";
        vocab = "sha256-6Q4OY1I0RF303vr96or/I/ig1oxzdERiwCG12/825V8=";
        lex = "sha256-F7kz8rQlFu0NMl3YYu58UKzHT/xdKmYFmzV7kx94jfc=";
      };
    });
    enfr = (getModel {
      lang = "enfr";
      vocabLang = "fren";
      hashes = {
        model = "sha256-8577tqsVSWfidi98S6oP8vL6CPMr/l9tKbeHcmR26Cg=";
        vocab = "sha256-poIqFyRU/PasugySbN5ZAi5NUlsAUt2Mift7x2oVQuA=";
        lex = "sha256-7V6uvxmLeHtxi4GUjdsYR4ClWebu/bdBb99AXvPlBXY=";
      };
    });
    ruen = (getModel {
      lang = "ruen";
      hashes = {
        model = "sha256-Sop7mwfJ4GoWfsW/JUJSiBcyFRbbTt9hT9pFAR+o5dE=";
        vocab = "sha256-zXC4KOmeTQx51IzVbYV51lbIfB2yC/iIg9owhdy/73U=";
        lex = "sha256-ZST1yJjx/vUpkr0lZabUrPr7ak6NzWrvI3vYiCOUGKA=";
      };
    });
    enru = (getModel {
      lang = "enru";
      hashes = {
        model = "sha256-PG4//SdclqIgrijdtVuMK4a0T/7MHutsgZXBU23krHQ=";
        vocab = "sha256-B+2QVTGfKtxQoWvH5jb+FUerdF65huQLY4SVbcH8bP0=";
        lex = "sha256-bwD12VW4slnLSnjlut8JWOX+884VP9d6beoDJFGLsbg=";
      };
    });
    deen = (getModel {
      lang = "deen";
      hashes = {
        model = "sha256-DCeYpqQohYd7APM3lBbkDnx0iXW55LindlurnLbStvY=";
        vocab = "sha256-rX1BgOuYyuV2h1UYchJU6fqYh88XAuOFxj92RFpQJPo=";
        lex = "sha256-piJfg6zKaZlKrOf0spWzHpjFebsM6S5BSG3smIHJBkc=";
      };
    });
    ende = (getModel {
      lang = "ende";
      vocabLang = "deen";
      hashes = {
        model = "sha256-2iH82QpsqkCgskbSM8jl9a16qaGqmO7OCWgoINNbsqY=";
        vocab = "sha256-mxn+WtecJZMU8du3ucvYd7qG17H7YeP6FkzJUvbPw4E=";
        lex = "sha256-1mBq7NR2JOhnIe1HYrpHvgeusLLcAE/M0SHy6x+Yvdw=";
      };
    });
  }
