self: {
  pamEnvironmentText = ''
    export PATH="$PATH:/var/current-system/setuid/wrappers:/var/current-system/sw/bin:/var/current-system/sw/sbin"
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
  pamEnvironment = self.pkgs.writeText "pam-environment" self.pamEnvironmentText;
  pamLoginText = ''
    # Account management.
    account sufficient pam_unix.so
    # Authentication management.
    auth sufficient pam_unix.so nullok likeauth
    auth required   pam_deny.so
    # Password management.
    password requisite pam_unix.so nullok sha512
    # Session management.
    session required pam_env.so envfile=${self.pamEnvironment}
    session required pam_unix.so
  '';
  pamLoginConf = self.pkgs.writeText "pam-login" self.pamLoginText;
  pamSuConf = self.pkgs.writeText ''pam-su'' self.pamSuText;
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
  pamPasswdConf = self.pkgs.writeText ''passwd'' self.pamPasswdText;
  pamPasswdText = ''
    # Account management.
    account sufficient pam_unix.so
    # Authentication management.
    auth sufficient pam_unix.so  likeauth
    auth required   pam_deny.so
    # Password management.
    password requisite pam_unix.so nullok sha512
    # Session management.
    session required pam_env.so envfile=${self.pamEnvironment}
    session required pam_unix.so
  '';
  pamSshdConf = self.pkgs.writeText ''passwd'' self.pamSshdText;
  pamSshdText = ''
    # Account management.
    account sufficient pam_unix.so
    # Authentication management.
    auth sufficient pam_unix.so  likeauth
    auth required   pam_deny.so
    # Password management.
    password requisite pam_unix.so sha512
    # Session management.
    session required pam_env.so envfile=${self.pamEnvironment}
    session required pam_unix.so
  '';
  pamD = self.pkgs.runCommand "pam.d" {} self.pamDBuildCommands;
  pamDBuildCommands = ''
    mkdir "$out"

    ln -s ${self.pamLoginConf}  "$out"/login
    ln -s ${self.pamLoginConf}  "$out"/xscreensaver
    ln -s ${self.pamLoginConf}  "$out"/cups
    ln -s ${self.pamLoginConf}  "$out"/xlock
    ln -s ${self.pamLoginConf}  "$out"/vlock
    ln -s ${self.pamLoginConf}  "$out"/screen
    ln -s ${self.pamLoginConf}  "$out"/other
    ln -s ${self.pamSuConf}     "$out"/su
    ln -s ${self.pamSuConf}     "$out"/sudo
    ln -s ${self.pamSshdConf}   "$out"/sshd
    ln -s ${self.pamPasswdConf} "$out"/passwd
    ln -s ${self.pamSuConf} "$out"/groupadd
    ln -s ${self.pamSuConf} "$out"/useradd
    ln -s ${self.pamSuConf} "$out"/userdel
    ln -s ${self.pamSuConf} "$out"/usermod
    ln -s ${self.pamSuConf} "$out"/chsh
  '';
}
