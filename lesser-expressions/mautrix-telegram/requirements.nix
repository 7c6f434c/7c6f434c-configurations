# generated using pypi2nix tool (version: 2.0.1)
# See more at: https://github.com/nix-community/pypi2nix
#
# COMMAND:
#   pypi2nix -e mautrix-telegram -e pytest-runner -e libmagic -e file -E file -E libffi
#

{ pkgs ? import <nixpkgs> {},
  overrides ? ({ pkgs, python }: self: super: {})
}:

let

  inherit (pkgs) makeWrapper;
  inherit (pkgs.stdenv.lib) fix' extends inNixShell;

  pythonPackages =
  import "${toString pkgs.path}/pkgs/top-level/python-packages.nix" {
    inherit pkgs;
    inherit (pkgs) stdenv;
    python = pkgs.python3;
  };

  commonBuildInputs = with pkgs; [ file libffi ];
  commonDoCheck = false;

  withPackages = pkgs':
    let
      pkgs = builtins.removeAttrs pkgs' ["__unfix__"];
      interpreterWithPackages = selectPkgsFn: pythonPackages.buildPythonPackage {
        name = "python3-interpreter";
        buildInputs = [ makeWrapper ] ++ (selectPkgsFn pkgs);
        buildCommand = ''
          mkdir -p $out/bin
          ln -s ${pythonPackages.python.interpreter} \
              $out/bin/${pythonPackages.python.executable}
          for dep in ${builtins.concatStringsSep " "
              (selectPkgsFn pkgs)}; do
            if [ -d "$dep/bin" ]; then
              for prog in "$dep/bin/"*; do
                if [ -x "$prog" ] && [ -f "$prog" ]; then
                  ln -s $prog $out/bin/`basename $prog`
                fi
              done
            fi
          done
          for prog in "$out/bin/"*; do
            wrapProgram "$prog" --prefix PYTHONPATH : "$PYTHONPATH"
          done
          pushd $out/bin
          ln -s ${pythonPackages.python.executable} python
          ln -s ${pythonPackages.python.executable} \
              python3
          popd
        '';
        passthru.interpreter = pythonPackages.python;
      };

      interpreter = interpreterWithPackages builtins.attrValues;
    in {
      __old = pythonPackages;
      inherit interpreter;
      inherit interpreterWithPackages;
      mkDerivation = args: pythonPackages.buildPythonPackage (args // {
        nativeBuildInputs = (args.nativeBuildInputs or []) ++ args.buildInputs;
      });
      packages = pkgs;
      overrideDerivation = drv: f:
        pythonPackages.buildPythonPackage (
          drv.drvAttrs // f drv.drvAttrs // { meta = drv.meta; }
        );
      withPackages = pkgs'':
        withPackages (pkgs // pkgs'');
    };

  python = withPackages {};

  generated = self: {
    "aiohttp" = python.mkDerivation {
      name = "aiohttp-3.6.2";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/00/94/f9fa18e8d7124d7850a5715a0b9c0584f7b9375d331d35e157cee50f27cc/aiohttp-3.6.2.tar.gz";
        sha256 = "259ab809ff0727d0e834ac5e8a283dc5e3e0ecc30c4d80b3cd17a4139ce1f326";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [
        self."async-timeout"
        self."attrs"
        self."chardet"
        self."multidict"
        self."yarl"
      ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/aio-libs/aiohttp";
        license = licenses.asl20;
        description = "Async http client/server framework (asyncio)";
      };
    };

    "alembic" = python.mkDerivation {
      name = "alembic-1.3.1";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/84/64/493c45119dce700a4b9eeecc436ef9e8835ab67bae6414f040cdc7b58f4b/alembic-1.3.1.tar.gz";
        sha256 = "49277bb7242192bbb9eac58fed4fe02ec6c3a2a4b4345d2171197459266482b2";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [
        self."mako"
        self."python-dateutil"
        self."python-editor"
        self."sqlalchemy"
      ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://alembic.sqlalchemy.org";
        license = licenses.mit;
        description = "A database migration tool for SQLAlchemy.";
      };
    };

    "async-timeout" = python.mkDerivation {
      name = "async-timeout-3.0.1";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/a1/78/aae1545aba6e87e23ecab8d212b58bb70e72164b67eb090b81bb17ad38e3/async-timeout-3.0.1.tar.gz";
        sha256 = "0c3c816a028d47f659d6ff5c745cb2acf1f966da1fe5c19c77a70282b25f4c5f";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/aio-libs/async_timeout/";
        license = licenses.asl20;
        description = "Timeout context manager for asyncio programs";
      };
    };

    "attrs" = python.mkDerivation {
      name = "attrs-19.3.0";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/98/c3/2c227e66b5e896e15ccdae2e00bbc69aa46e9a8ce8869cc5fa96310bf612/attrs-19.3.0.tar.gz";
        sha256 = "f7b7ce16570fe9965acd6d30101a28f62fb4a7f9e926b3bbc9b61f8b04247e72";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [
        self."setuptools"
        self."wheel"
      ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://www.attrs.org/";
        license = licenses.mit;
        description = "Classes Without Boilerplate";
      };
    };

    "cffi" = python.mkDerivation {
      name = "cffi-1.13.2";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/2d/bf/960e5a422db3ac1a5e612cb35ca436c3fc985ed4b7ed13a1b4879006f450/cffi-1.13.2.tar.gz";
        sha256 = "599a1e8ff057ac530c9ad1778293c665cb81a791421f46922d80a86473c13346";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [
        self."pycparser"
      ];
      meta = with pkgs.stdenv.lib; {
        homepage = "http://cffi.readthedocs.org";
        license = licenses.mit;
        description = "Foreign Function Interface for Python calling C code.";
      };
    };

    "chardet" = python.mkDerivation {
      name = "chardet-3.0.4";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/fc/bb/a5768c230f9ddb03acc9ef3f0d4a3cf93462473795d18e9535498c8f929d/chardet-3.0.4.tar.gz";
        sha256 = "84ab92ed1c4d4f16916e05906b6b75a6c0fb5db821cc65e70cbd64a3e2a5eaae";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/chardet/chardet";
        license = licenses.lgpl2;
        description = "Universal encoding detector for Python 2 and 3";
      };
    };

    "commonmark" = python.mkDerivation {
      name = "commonmark-0.9.1";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/60/48/a60f593447e8f0894ebb7f6e6c1f25dafc5e89c5879fdc9360ae93ff83f0/commonmark-0.9.1.tar.gz";
        sha256 = "452f9dc859be7f06631ddcb328b6919c67984aca654e5fefb3914d54691aed60";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/rtfd/commonmark.py";
        license = licenses.bsdOriginal;
        description = "Python parser for the CommonMark Markdown spec";
      };
    };

    "file" = python.mkDerivation {
      name = "file-0.3.0";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/0a/49/317ac8ed10afb25c4c24972ea4de4c5507d6b0ab13bca4941c9a0bdc64ea/file-0.3.0.tar.gz";
        sha256 = "02735d18e92a13dd8335aee2373ffbe41444b2c42091d03b75dd2633121c46f4";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [
        self."cffi"
      ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/ionelmc/python-file";
        license = licenses.mit;
        description = "CFFI bindings for libmagic.";
      };
    };

    "future-fstrings" = python.mkDerivation {
      name = "future-fstrings-1.2.0";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/5d/e2/3874574cce18a2e3608abfe5b4b5b3c9765653c464f5da18df8971cf501d/future_fstrings-1.2.0.tar.gz";
        sha256 = "6cf41cbe97c398ab5a81168ce0dbb8ad95862d3caf23c21e4430627b90844089";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/asottile/future-fstrings";
        license = licenses.mit;
        description = "A backport of fstrings to python<3.6";
      };
    };

    "idna" = python.mkDerivation {
      name = "idna-2.8";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/ad/13/eb56951b6f7950cadb579ca166e448ba77f9d24efc03edd7e55fa57d04b7/idna-2.8.tar.gz";
        sha256 = "c357b3f628cf53ae2c4c05627ecc484553142ca23264e593d327bcde5e9c3407";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/kjd/idna";
        license = licenses.bsdOriginal;
        description = "Internationalized Domain Names in Applications (IDNA)";
      };
    };

    "libmagic" = python.mkDerivation {
      name = "libmagic-1.0";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/83/86/419ddfc3879b4565a60e0c75b6d19baec48428cbc2f15aca5320b3d136f6/libmagic-1.0.tar.gz";
        sha256 = "649f1ce7fb7c92796badbb812555e4a926351da4f5cdf82e810b5cd371aedf8d";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "http://bitbucket.org/xmonader/pymagic-dev";
        license = licenses.gpl1;
        description = "libmagic bindings";
      };
    };

    "mako" = python.mkDerivation {
      name = "mako-1.1.0";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/b0/3c/8dcd6883d009f7cae0f3157fb53e9afb05a0d3d33b3db1268ec2e6f4a56b/Mako-1.1.0.tar.gz";
        sha256 = "a36919599a9b7dc5d86a7a8988f23a9a3a3d083070023bab23d64f7f1d1e0a4b";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [
        self."markupsafe"
      ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://www.makotemplates.org/";
        license = licenses.mit;
        description = "A super-fast templating language that borrows the  best ideas from the existing templating languages.";
      };
    };

    "markupsafe" = python.mkDerivation {
      name = "markupsafe-1.1.1";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/b9/2e/64db92e53b86efccfaea71321f597fa2e1b2bd3853d8ce658568f7a13094/MarkupSafe-1.1.1.tar.gz";
        sha256 = "29872e92839765e546828bb7754a68c418d927cd064fd4708fab9fe9c8bb116b";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://palletsprojects.com/p/markupsafe/";
        license = licenses.bsdOriginal;
        description = "Safely add untrusted strings to HTML/XML markup.";
      };
    };

    "mautrix-appservice" = python.mkDerivation {
      name = "mautrix-appservice-0.3.11";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/ba/31/594f902a147fb60eb0f9ab6cd17122c4839d6d7ac7b1af5030a02be6108e/mautrix-appservice-0.3.11.tar.gz";
        sha256 = "60192920cff75afdd096eea3a43276e33ec15f4f00bd04d2d1dda616c84f22a5";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [
        self."aiohttp"
        self."future-fstrings"
      ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/tulir/mautrix-appservice-python";
        license = licenses.mit;
        description = "A Python 3 asyncio-based Matrix application service framework.";
      };
    };

    "mautrix-telegram" = python.mkDerivation {
      name = "mautrix-telegram-0.6.1";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/c7/f9/19692e42d925e7d49d48930360344d0516b105332a450ed9e2327878d011/mautrix-telegram-0.6.1.tar.gz";
        sha256 = "2ddcd404a94918accfa3409c7cb3d12677cb4d633507a8bbf4c9a1ec4b3751d3";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [
        self."aiohttp"
        self."alembic"
        self."commonmark"
        self."future-fstrings"
        self."mautrix-appservice"
        self."python-magic"
        self."ruamel-yaml"
        self."sqlalchemy"
        self."telethon"
        self."telethon-session-sqlalchemy"
      ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/tulir/mautrix-telegram";
        license = licenses.agpl3Plus;
        description = "A Matrix-Telegram hybrid puppeting/relaybot bridge.";
      };
    };

    "multidict" = python.mkDerivation {
      name = "multidict-4.6.1";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/8a/74/61547af55c077b8d2e3648c2af74c08fa1e382665b290468db7ba54db2ea/multidict-4.6.1.tar.gz";
        sha256 = "5159c4975931a1a78bf6602bbebaa366747fce0a56cb2111f44789d2c45e379f";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/aio-libs/multidict";
        license = licenses.asl20;
        description = "multidict implementation";
      };
    };

    "pyaes" = python.mkDerivation {
      name = "pyaes-1.6.1";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/44/66/2c17bae31c906613795711fc78045c285048168919ace2220daa372c7d72/pyaes-1.6.1.tar.gz";
        sha256 = "02c1b1405c38d3c370b085fb952dd8bea3fadcee6411ad99f312cc129c536d8f";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/ricmoo/pyaes";
        license = licenses.mit;
        description = "Pure-Python Implementation of the AES block-cipher and common modes of operation";
      };
    };

    "pyasn1" = python.mkDerivation {
      name = "pyasn1-0.4.8";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/a4/db/fffec68299e6d7bad3d504147f9094830b704527a7fc098b721d38cc7fa7/pyasn1-0.4.8.tar.gz";
        sha256 = "aef77c9fb94a3ac588e87841208bdec464471d9871bd5050a287cc9a475cd0ba";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/etingof/pyasn1";
        license = licenses.bsdOriginal;
        description = "ASN.1 types and codecs";
      };
    };

    "pycparser" = python.mkDerivation {
      name = "pycparser-2.19";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/68/9e/49196946aee219aead1290e00d1e7fdeab8567783e83e1b9ab5585e6206a/pycparser-2.19.tar.gz";
        sha256 = "a988718abfad80b6b157acce7bf130a30876d27603738ac39f140993246b25b3";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/eliben/pycparser";
        license = licenses.bsdOriginal;
        description = "C parser in Python";
      };
    };

    "pytest-runner" = python.mkDerivation {
      name = "pytest-runner-5.2";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/5b/82/1462f86e6c3600f2471d5f552fcc31e39f17717023df4bab712b4a9db1b3/pytest-runner-5.2.tar.gz";
        sha256 = "96c7e73ead7b93e388c5d614770d2bae6526efd997757d3543fe17b557a0942b";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [
        self."setuptools"
        self."setuptools-scm"
        self."wheel"
      ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/pytest-dev/pytest-runner/";
        license = licenses.mit;
        description = "Invoke py.test as distutils command with dependency resolution";
      };
    };

    "python-dateutil" = python.mkDerivation {
      name = "python-dateutil-2.8.1";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/be/ed/5bbc91f03fa4c839c4c7360375da77f9659af5f7086b7a7bdda65771c8e0/python-dateutil-2.8.1.tar.gz";
        sha256 = "73ebfe9dbf22e832286dafa60473e4cd239f8592f699aa5adaf10050e6e1823c";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [
        self."setuptools"
        self."setuptools-scm"
        self."wheel"
      ];
      propagatedBuildInputs = [
        self."six"
      ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://dateutil.readthedocs.io";
        license = licenses.bsdOriginal;
        description = "Extensions to the standard Python datetime module";
      };
    };

    "python-editor" = python.mkDerivation {
      name = "python-editor-1.0.4";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/0a/85/78f4a216d28343a67b7397c99825cff336330893f00601443f7c7b2f2234/python-editor-1.0.4.tar.gz";
        sha256 = "51fda6bcc5ddbbb7063b2af7509e43bd84bfc32a4ff71349ec7847713882327b";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/fmoo/python-editor";
        license = licenses.asl20;
        description = "Programmatically open an editor, capture the result.";
      };
    };

    "python-magic" = python.mkDerivation {
      name = "python-magic-0.4.15";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/84/30/80932401906eaf787f2e9bd86dc458f1d2e75b064b4c187341f29516945c/python-magic-0.4.15.tar.gz";
        sha256 = "f3765c0f582d2dfc72c15f3b5a82aecfae9498bd29ca840d72f37d7bd38bfcd5";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "http://github.com/ahupp/python-magic";
        license = licenses.mit;
        description = "File type identification using libmagic";
      };
    };

    "rsa" = python.mkDerivation {
      name = "rsa-4.0";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/cb/d0/8f99b91432a60ca4b1cd478fd0bdf28c1901c58e3a9f14f4ba3dba86b57f/rsa-4.0.tar.gz";
        sha256 = "1a836406405730121ae9823e19c6e806c62bbad73f890574fff50efa4122c487";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [
        self."pyasn1"
      ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://stuvel.eu/rsa";
        license = licenses.asl20;
        description = "Pure-Python RSA implementation";
      };
    };

    "ruamel-yaml" = python.mkDerivation {
      name = "ruamel-yaml-0.15.100";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/9a/ee/55cd64bbff971c181e2d9e1c13aba9a27fd4cd2bee545dbe90c44427c757/ruamel.yaml-0.15.100.tar.gz";
        sha256 = "8e42f3067a59e819935a2926e247170ed93c8f0b2ab64526f888e026854db2e4";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://bitbucket.org/ruamel/yaml";
        license = licenses.mit;
        description = "ruamel.yaml is a YAML parser/emitter that supports roundtrip preservation of comments, seq/map flow style, and map key order";
      };
    };

    "setuptools" = python.mkDerivation {
      name = "setuptools-42.0.2";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/f7/b6/5b98441b6749ea1db1e41e5e6e7a93cbdd7ffd45e11fe1b22d45884bc777/setuptools-42.0.2.zip";
        sha256 = "c5b372090d7c8709ce79a6a66872a91e518f7d65af97fca78135e1cb10d4b940";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/pypa/setuptools";
        license = licenses.mit;
        description = "Easily download, build, install, upgrade, and uninstall Python packages";
      };
    };

    "setuptools-scm" = python.mkDerivation {
      name = "setuptools-scm-3.3.3";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/83/44/53cad68ce686585d12222e6769682c4bdb9686808d2739671f9175e2938b/setuptools_scm-3.3.3.tar.gz";
        sha256 = "bd25e1fb5e4d603dcf490f1fde40fb4c595b357795674c3e5cb7f6217ab39ea5";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/pypa/setuptools_scm/";
        license = licenses.mit;
        description = "the blessed package to manage your versions by scm tags";
      };
    };

    "six" = python.mkDerivation {
      name = "six-1.13.0";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/94/3e/edcf6fef41d89187df7e38e868b2dd2182677922b600e880baad7749c865/six-1.13.0.tar.gz";
        sha256 = "30f610279e8b2578cab6db20741130331735c781b56053c59c4076da27f06b66";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/benjaminp/six";
        license = licenses.mit;
        description = "Python 2 and 3 compatibility utilities";
      };
    };

    "sqlalchemy" = python.mkDerivation {
      name = "sqlalchemy-1.3.11";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/34/5c/0e1d7ad0ca52544bb12f9cb8d5cc454af45821c92160ffedd38db0a317f6/SQLAlchemy-1.3.11.tar.gz";
        sha256 = "afa5541e9dea8ad0014251bc9d56171ca3d8b130c9627c6cb3681cff30be3f8a";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "http://www.sqlalchemy.org";
        license = licenses.mit;
        description = "Database Abstraction Library";
      };
    };

    "telethon" = python.mkDerivation {
      name = "telethon-1.9.0";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/6c/9b/8668c74decf4c5e8cec3d73794f89947842a39b9e127bf7e7dfeddd8ca5b/Telethon-1.9.0.tar.gz";
        sha256 = "a8797ad5bfee2b350cfc9b73cbb30fc19c8f73c0db42471e0df1371b1a269edc";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [
        self."pyaes"
        self."rsa"
      ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/LonamiWebs/Telethon";
        license = licenses.mit;
        description = "Full-featured Telegram client library for Python 3";
      };
    };

    "telethon-session-sqlalchemy" = python.mkDerivation {
      name = "telethon-session-sqlalchemy-0.2.15";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/5a/cf/e558fe7af77ba06251916e1c5450e156e96709e47df4c5c782a2ac077ae7/telethon-session-sqlalchemy-0.2.15.tar.gz";
        sha256 = "2ba603d95d5be6ddecd8ecaeaffba00b75b49dd80eb77f6228dd7b793ca67fd2";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [
        self."sqlalchemy"
      ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/tulir/telethon-session-sqlalchemy";
        license = licenses.mit;
        description = "SQLAlchemy backend for Telethon session storage";
      };
    };

    "wheel" = python.mkDerivation {
      name = "wheel-0.33.6";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/59/b0/11710a598e1e148fb7cbf9220fd2a0b82c98e94efbdecb299cb25e7f0b39/wheel-0.33.6.tar.gz";
        sha256 = "10c9da68765315ed98850f8e048347c3eb06dd81822dc2ab1d4fde9dc9702646";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/pypa/wheel";
        license = licenses.mit;
        description = "A built-package format for Python.";
      };
    };

    "yarl" = python.mkDerivation {
      name = "yarl-1.4.1";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/5f/9d/7aced56856e64bf1b6c14a3701ebdfc7942574d8a5487ee7cccf4cd13ffd/yarl-1.4.1.tar.gz";
        sha256 = "6076bce2ecc6ebf6c92919d77762f80f4c9c6ecc9c1fbaa16567ec59ad7d6f1d";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [
        self."idna"
        self."multidict"
      ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/aio-libs/yarl/";
        license = licenses.asl20;
        description = "Yet another URL library";
      };
    };
  };
  localOverridesFile = ./requirements_override.nix;
  localOverrides = import localOverridesFile { inherit pkgs python; };
  commonOverrides = [
        (let src = pkgs.fetchFromGitHub { owner = "nix-community"; repo = "pypi2nix-overrides"; rev = "db87933d87d9b3943cf636a49f16b76c9ea66db7"; sha256 = "1phiqh72dyg7qhkv15kdg4gjkx8rkywvs41j7liz5faj66ijlpv6"; } ; in import "${src}/overrides.nix" { inherit pkgs python; })
  ];
  paramOverrides = [
    (overrides { inherit pkgs python; })
  ];
  allOverrides =
    (if (builtins.pathExists localOverridesFile)
     then [localOverrides] else [] ) ++ commonOverrides ++ paramOverrides;

in python.withPackages
   (fix' (pkgs.lib.fold
            extends
            generated
            allOverrides
         )
   )
