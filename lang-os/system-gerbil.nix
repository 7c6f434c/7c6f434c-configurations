{ 
  pkgs ? import <nixpkgs> {}
  , tty ? "tty12"
  , code ? ''(begin (print "Hello from Gerbil. Time is ") (print (time->seconds (current-time))) (print "\n") (thread-sleep! 10.2))''
  , deps ? []
}:
pkgs.writeScript "system-gerbil-launcher" ''
  trap : 1 2 3 13 14 15
  while true; do
    (
      ${
        pkgs.lib.concatMapStrings 
        (s: ''export GERBIL_LOADPATH="$GERBIL_LOADPATH''${GERBIL_LOADPATH:+:}${s}"; '')
        deps
      }
      "${pkgs.gerbil}/bin/gxi" -e '(load "${pkgs.writeText "system-gerbil-script.scm" code}")' < /dev/null &>/dev/${tty}
    )
  done
''
