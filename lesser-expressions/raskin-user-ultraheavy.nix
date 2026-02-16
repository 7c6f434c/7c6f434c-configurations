with import ./env-defs.nix;
with pkgs;

linkFarm "raskin-ultraheavy-packages" ([
  { name = "main-ultraheavy-package-set";
    path = (fullEnv "main-ultraheavy-package-set"
      [
        winePackages.unstable 
        /*midori*/ /* scilab-bin */
        /* eolie */
        epiphany
        ffmpeg-full obs-studio audacity pitivi
        diff-pdf
      ]);}
])

