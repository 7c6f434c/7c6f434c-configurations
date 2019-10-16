{ pkgs, python }:

self: super: {
  plastex = super.plastex.override (x: {
    sourceRoot = "plasTeX-1.0.0";
  });
}
