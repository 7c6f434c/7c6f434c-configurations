{ pkgs, python }:

self: super: {
  mautrix-telegram = super.mautrix-telegram.override (x: {
    propagatedBuildInputs = x.propagatedBuildInputs ++ [
      self.pytest-runner
      self.libmagic
    ];
  });
  python-magic = super.python-magic.override (x: {
    patchPhase = ''
      sed -e "s@dll = .*@dll = '${pkgs.file}/lib/libmagic${pkgs.stdenv.hostPlatform.extensions.sharedLibrary}'@" -i magic.py
    '';
  });
}
