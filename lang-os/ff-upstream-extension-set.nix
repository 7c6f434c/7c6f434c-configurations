{ symlinkJoin, callPackage }:
symlinkJoin {
  name = "firefox-extensions";
  paths = (callPackage ./user/upstream-extensions.nix {});
}
