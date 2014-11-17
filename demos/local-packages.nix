let
testPackage = {bc, writeScript}: {
  name = "echoPi";
  builder = writeScript "echoPi-builder" ''
    #! /bin/sh
    echo -e 'scale=1\n 4 * a(1)\n quit' | ${bc}/bin/bc -l;
  '';
};
localPkgs = pkgs: with pkgs; {
  testPackage = testPackage {
    inherit bc writeScript;
  };
};
pkgs = (import /etc/nixos/nixpkgs) {}; 

in
pkgs // (localPkgs pkgs)
