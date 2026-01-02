{ fetchFromGitHub, runCommand }:
let asy-circ-src = 
fetchFromGitHub {
  owner = "erusyd";
  repo = "asy-circ";
  rev = "477c857e402943e3e6cda907489220e94381167b";
  hash = "sha256-x9LHMf7Ma8OhaZO/0AgknQ9GIWLHKSGYgHHXZj3gJXA=";
};
asy-circ = runCommand "asy-circ" {} ''
  mkdir -p "$out/share/asymptote"
  ln -s "${asy-circ-src}" "$out/share/asymptote/asy-circ"
'';
in
  asy-circ
