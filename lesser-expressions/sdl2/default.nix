{ newScope, callPackage }:
let self = {
  SDL2 = callPackage ./sdl2.nix {};
  SDL2_classic = self.SDL2;
  SDL2_ttf = callPackage ./sdl2-ttf.nix { inherit(self) SDL2_classic; };
  SDL2_image = callPackage ./sdl2-image.nix { inherit(self) SDL2_classic; };
  SDL2_mixer = callPackage ./sdl2-mixer.nix { inherit(self) SDL2_classic; };
}; in self
