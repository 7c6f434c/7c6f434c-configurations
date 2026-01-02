with import ./env-defs.nix;

linkFarm "raskin-packages" (
  (map justUse [
    "sbcl"
    "ccl" "ecl" "clisp" /*"gcl"*/ "abcl" "mkcl"
    "asdf"
    "racket"
  ])
  )
