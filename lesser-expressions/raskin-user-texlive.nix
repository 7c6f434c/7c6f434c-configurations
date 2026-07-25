with import ./env-defs.nix; 
with pkgs;

let konsole-profile = ./rc.private/konsole.profile; in
let konsole-colorscheme = ./rc.private/KonsoleMyLight.colorscheme; in
let tmux-profile = ./rc.private/tmux/tmux.conf; in
let tmux-profile-sh = ./rc.private/tmux/tmux.conf.sh; in

#let tmux-to-use = tmux.overrideAttrs (x: 
#{
#  patches = tmux.patches ++ [
#    ./tmux-sixel-fixes-issue-3839-at-2024-02-25.diff
#  ]; 
#});
###
let tmux-to-use = tmux;

in

fullEnv "main-light-package-set"
[
        (import ./texlive-set.nix pkgs)
]
