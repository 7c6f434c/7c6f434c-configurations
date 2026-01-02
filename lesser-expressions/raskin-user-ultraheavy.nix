with import ./env-defs.nix;
with pkgs;

linkFarm "raskin-ultraheavy-packages" ([
  { name = "main-ultraheavy-package-set";
    path = (fullEnv "main-ultraheavy-package-set"
      [
        /* libreoffice */ /*chromium*/ /* qutebrowser */ winePackages.unstable 
        /*sage*/ /*midori*/ /* scilab-bin */ /* clasp-common-lisp */
        /* eolie */
        epiphany
        ffmpeg-full obs-studio audacity /* pitivi */
        diff-pdf
      ]);}
])

