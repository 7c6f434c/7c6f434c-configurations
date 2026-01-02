with import ./env-defs.nix;
with pkgs;

linkFarm "raskin-toy-packages" ([
  { name = "main-toy-package-set";
    path = (fullEnv "main-heavy-package-set"
      [
        fsg kobodeluxe extremetuxracer 
        golly
        lincity construo
        sgt-puzzles xconq pysolfc xaos _2048-in-terminal blobby
        xpilot-ng liquidwar
        quantumminigolf liquidwar5 xmoto
        ruffle
        /*(renpy.override (x: {
          ffmpeg = ffmpeg_6;
          SDL2 = SDL2_classic;
          python3 = x.python3 // {
            pkgs = x.python3.pkgs // {
              pygame-sdl2 = x.python3.pkgs.pygame-sdl2.override {
                SDL2 = SDL2_classic;
                SDL2_mixer = SDL2_mixer.override {
                  SDL2 = SDL2_classic;
                };
                SDL2_ttf = SDL2_ttf.override {
                  SDL2 = SDL2_classic;
                };
                SDL2_image = SDL2_image.override {
                  SDL2 = SDL2_classic;
                };
              };
            };
          };
          }))*/
        (let 
          py=python312; 
          sdl2pkgs = callPackage ./sdl2 {};
        in renpy.override {
          inherit (sdl2pkgs) SDL2;
          python3 = py // {
            pkgs = py.pkgs // {
              pygame-sdl2 = py.pkgs.pygame-sdl2.override {
                inherit (sdl2pkgs) SDL2 SDL2_mixer SDL2_ttf SDL2_image;
              };
            };
          };
        })
        (let py=python312;
             rp = renpy.override { python3 = py; };
        in runCommand "renpy-sdl2-compat" {} ''
          mkdir "$out/bin" -p
          ln -s "${rp}/bin/renpy" "$out/bin/renpy-sdl2-compat"
            '')
        (runCommand "rpatool" {} ''
          mkdir -p "$out"/bin
          cp "${(fetchFromGitHub {
            owner = "shizmob";
            repo = "rpatool";
            rev = "74f26d5dfdd645483e02552aa766ca447ad6b191";
            sha256 = "sha256-S4aX+bDy3Gu5so1V2tq2kRhfmjT888hCI6g2MNVCtYE=";
          })}"/rpatool "$out/bin"
          chmod a+x "$out/bin/rpatool"
        '')
      ]);}
])

