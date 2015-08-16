{pkgs,...}:
{
   fetchmtn = {
     cacheDB = "/var/cache/monotone/mtn.db";
     # defaultDBMirrors = ["file:///home/raskin/.mtn-journal-dir/.mtn.db"];
   };

   ikiwiki = {
     git = true;
     monotone = true;
   };

   packageOverrides = p: 
     let
       pp = import ./private-packages.nix {pkgs=p;}; 
     in
   {
     glibcLocales = p.glibcLocales.override {
       locales = ["en_US.UTF-8/UTF-8" "ru_RU.UTF-8/UTF-8" "ru_RU.KOI8-R/KOI8-R" "ru_RU.CP1251/CP1251"];
       allLocales = true;
     };
     #patchelf06 = p.lib.overrideDerivation p.patchelf06 (x: {
     #  src = "" + /home/repos/patchelf + ""; 
     #  preConfigure = "./bootstrap.sh"; 
     #  buildInputs = x.buildInputs ++ [p.autoconf p.automake p.libtool];
     #});
     # grub2 = pp.grub2Bzr;
     # kexectools="/var/empty/";
   };

   allowUnfreePredicate = (x:
     (pkgs.lib.hasPrefix "nvidia-x11-" x.name) ||
     false
   );

  allowTexliveBuilds = true;
}
