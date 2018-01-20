{ pkgs, fromNixOS }:
rec {
  pamEnvironmentText = ''
    export PATH="/var/current-system/setuid/wrappers:/var/current-system/sw/bin:/var/current-system/sw/sbin:$PATH"
    export MODULE_DIR=/run/booted-system/boot/kernel-modules/lib/modules
    export LD_LIBRARY_PATH=/run/opengl-driver/lib:/run/opengl-driver-32/lib
    export LOCALE_ARCHIVE=/var/current-system/sw/lib/locale/locale-archive
    export NIX_CONF_DIR=/etc/nix
    export NIX_CURRENT_LOAD=/run/nix/current-load
    export OPENSSL_X509_CERT_FILE=/etc/ssl/certs/ca-bundle.crt
    export SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt
    export TERMINFO_DIRS=/var/current-system/sw/share/terminfo
    export TZDIR=/etc/zoneinfo
  '';
  pamEnvironment = pkgs.writeText "pam-environment" pamEnvironmentText;
  pamLoginText = ''
    # Account management.
    account sufficient pam_unix.so
    # Authentication management.
    auth sufficient pam_unix.so nullok likeauth
    auth required   pam_deny.so
    # Password management.
    password requisite pam_unix.so nullok sha512
    # Session management.
    session required pam_env.so envfile=${pamEnvironment}
    session required pam_unix.so
  '';
  pamLoginConf = pkgs.writeText "pam-login" pamLoginText;
  pamSuConf = pkgs.writeText ''pam-su'' pamSuText;
  pamSuText = ''
    # Account management.
    account sufficient pam_unix.so
    # Authentication management.
    auth sufficient pam_rootok.so
    auth required pam_tally.so
    auth sufficient pam_unix.so  likeauth
    auth required   pam_deny.so
    # Password management.
    password requisite pam_unix.so nullok sha512
    # Session management.
    session required pam_unix.so
  '';
  pamPasswdConf = pkgs.writeText ''passwd'' pamPasswdText;
  pamPasswdText = ''
    # Account management.
    account sufficient pam_unix.so
    # Authentication management.
    auth sufficient pam_unix.so  likeauth
    auth required   pam_deny.so
    # Password management.
    password requisite pam_unix.so nullok sha512
    # Session management.
    session required pam_env.so envfile=${pamEnvironment}
    session required pam_unix.so
  '';
  pamSshdConf = pkgs.writeText ''passwd'' pamSshdText;
  pamSshdText = ''
    # Account management.
    account sufficient pam_unix.so
    # Authentication management.
    auth sufficient pam_unix.so  likeauth
    auth required   pam_deny.so
    # Password management.
    password requisite pam_unix.so sha512
    # Session management.
    session required pam_env.so envfile=${pamEnvironment}
    session required pam_unix.so
  '';
  pamPieces = {
    login = pamLoginConf;
    su = pamSuConf;
    sshd = pamSshdConf;
    passwd = pamPasswdConf;
    other = pamLoginConf;
  };
  pamD = { programs ? {} }: pkgs.runCommand "pam.d" {} ''
    mkdir "$out"

    ${
      pkgs.lib.concatMapStrings 
         (x: ''ln -s "${builtins.getAttr x pamPieces}" "$out/${x}"'')
         (builtins.attrNames pamPieces)
    }
    ${
      pkgs.lib.concatMapStrings 
         (x: ''ln -s "${builtins.getAttr (builtins.getAttr x programs) pamPieces}" "$out/${x}"'')
         (builtins.attrNames programs)
    }
  '';
  nixosPamD = { services ? {}, sessionVariables ? {} }: pkgs.runCommand "pam.d" {} ''
    mkdir -p "$out"
    ln -s "$out" pam.d
    ${
      pkgs.lib.concatStrings (pkgs.lib.mapAttrsToList (k: v: ''
        ln -s "${v}" "${k}"
      '')
      (fromNixOS.etcSelectPrefix "pam.d/" {
        security.pam.services = services;
        environment.sessionVariables = sessionVariables;
      }))
    }
  '';
}
