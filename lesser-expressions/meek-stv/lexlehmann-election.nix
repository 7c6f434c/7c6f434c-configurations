{ python3Packages, fetchFromGitHub }:
python3Packages.buildPythonApplication {
  # Not yet working as packaged
  name = "election";
  version = "0.pre-untagged-2020-01-08";
  src = fetchFromGitHub {
    owner = "LexLehmann";
    repo = "Election";
    rev = "01ea52bd7111fb3fee1895d808476440e01275d9";
    hash = "sha256-rgOYuGjR8tINp9IqK4MDtrpW5C4FIlxi4qF+2ua3Vcc=";
  };
}
