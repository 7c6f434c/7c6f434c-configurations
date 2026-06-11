{ runCommand, fetchurl }:
[
    (runCommand "ruffle-rs-xpi" 
    {
      extid = "{b5501fd1-7084-45c5-9aa6-567c2fcf5dc6}";
    }
    ''
      mkdir -p "$out"
      cp -v "${(fetchurl {
        url = "https://addons.mozilla.org/firefox/downloads/file/4833742/ruffle_rs-0.3.0.26154.xpi";
        hash = "sha256-qFZTjWmaVuErBJuAFKBL9NfEvHoZ77OmKWuzn/Waif8=";
      })}" "$out/$extid.xpi"
    '')
    (runCommand "ublock-origin-xpi" 
    {
      extid = "uBlock0@raymondhill.net";
    }
    ''
      mkdir -p "$out"
      cp -v "${(fetchurl {
        url = "https://addons.mozilla.org/firefox/downloads/file/4814095/ublock_origin-1.71.0.xpi";
        hash = "sha256-R/eIofwsAUgwswuw75WIYVcBuYxSZfsZuM9Lp3mEn+s=";
      })}" "$out/$extid.xpi"
    '')
    (runCommand "down-them-all-xpi" 
    {
      extid = "{DDC359D1-844A-42a7-9AA1-88A850A938A8}";
    }
    ''
      mkdir -p "$out"
      cp -v "${(fetchurl {
        url = "https://addons.mozilla.org/firefox/downloads/file/4628327/downthemall-4.14.2.xpi";
        hash = "sha256-e7g/cpk0+ypY8sZSSAYIMLyjN+Hop8Jym1YINA3CnfM=";
      })}" "$out/_$(basename $out | sed -e 's/[^-]*-//')_$extid.xpi"
    '')
    (runCommand "youtube-no-translation-xpi" 
    {
      extid = "{9a3104a2-02c2-464c-b069-82344e5ed4ec}";
    }
    ''
      mkdir -p "$out"
      cp -v "${(fetchurl {
        url = "https://addons.mozilla.org/firefox/downloads/file/4822978/youtube_no_translation-2.24.1.xpi";
        hash = "sha256-fhryVVlutn04ud8eNgsyZBwgBzDUCk+c6wBCFS7qn1Q=";
      })}" "$out/_$(basename $out | sed -e 's/[^-]*-//')_$extid.xpi"
    '')
]
