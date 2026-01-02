{ stdenv, perl, fetchFromGitHub }:
stdenv.mkDerivation {
  pname = "civs";
  version = "2.26-untagged-2025-06-27";

  src = fetchFromGitHub {
    owner = "andrewcmyers";
    repo = "civs";
    rev = "6f1fd8264a7f8e322a3c95ed847058386e81a39d";
    hash = "sha256-YIao7RzP5R5EEdsxgoYn0FvXAU7VjBKGIErb5XnEEbg=";
    fetchSubmodules = true;
  };
}
