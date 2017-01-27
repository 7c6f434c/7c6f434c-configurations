{ pkgs ? import <nixpkgs> {}
, tools ? import ./tools.nix {inherit pkgs;}
}:
with pkgs // tools;
rec {
  bindNixConfig = import ../bind.nix {inherit pkgs; config.networking.hostName =(import /root/nix-sysconfig/hostname.nix).hostname ;};
  bindConfigText =
  (let 
    concatMapStrings = lib.concatMapStrings;
    cfg = bindNixConfig;
  in
  ''
      acl cachenetworks { ${concatMapStrings (entry: " ${entry}; ") (cfg.cacheNetworks or [])} };
      acl badnetworks { ${concatMapStrings (entry: " ${entry}; ") (cfg.blockedNetworks or [])} };

      options {
        listen-on {any;};
        listen-on-v6 {any;};
        allow-query { cachenetworks; };
        blackhole { badnetworks; };
        forward first;
        forwarders { ${concatMapStrings (entry: " ${entry}; ") (cfg.forwarders or [])} };
        directory "/var/run/named";
        pid-file "/var/run/named/named.pid";
      };

      ${ concatMapStrings
          ({ name, file, master ? true, slaves ? [], masters ? [] }:
            ''
              zone "${name}" {
                type ${if master then "master" else "slave"};
                file "${file}";
                ${ if master then
                   ''
                     allow-transfer {
                       ${concatMapStrings (ip: "${ip};\n") slaves}
                     };
                   ''
                   else
                   ''
                     masters {
                       ${concatMapStrings (ip: "${ip};\n") masters}
                     };
                   ''
                }
                allow-query { any; };
              };
            '')
          (cfg.zones or []) }
  '');
  bindConfig = writeText "named.conf" bindConfigText;
  bindScript = writeScript "bind-start" ''
    ${coreutils}/bin/mkdir -p /run/named
    ${gnugrep}/bin/grep '^named:' /etc/passwd || ${shadow}/bin/useradd named
    ${coreutils}/bin/chown named -R /run/named
    ${bind}/sbin/named -u named ${lib.optionalString (bindNixConfig.ipv4Only or false) "-4"} -c ${bindConfig} -f -g
  '';

  postgresqlNixConfig = import ../postgresql.nix {inherit pkgs;};
  postgresqlConfigText = let cfg = postgresqlNixConfig; in ''
      hba_file = '${postgresqlConfigHBA}'
      ident_file = '${postgresqlConfigIdent}'
      log_destination = 'stderr'
      port = ${toString (cfg.port or 5432)}
      ${cfg.extraConfig or ""}
  '';
  postgresqlConfigHBA = writeText "pg_hba.conf"
     (''
        # Generated file; do not edit!
        local all all              ident
        host  all all 127.0.0.1/32 md5
        host  all all ::1/128      md5
      '' +
      (postgresqlNixConfig.authentication or ""));
  postgresqlConfigIdent = pkgs.writeText "pg_ident.conf" (postgresqlNixConfig.identMap or "");
  postgresqlConfig = writeText "postgresql.conf" postgresqlConfigText;
  postgresqlScript = writeScript "postgresql-start" (let
    cfg = postgresqlNixConfig;
    dataDir = cfg.dataDir or "/var/db/postgresql";
    flags = lib.optional cfg.enableTCPIP "-i";
  in ''
    test -e ${dataDir} || {
      mkdir -m 0700 -p ${dataDir}
      if [ "$(id -u)" = 0 ]; then
        chown -R postgres ${dataDir}
        su -s ${stdenv.shell} postgres -c 'initdb -U root -D ${dataDir}'
      else
        # For non-root operation.
        initdb ${dataDir}
      fi
    }
    rm -f ${dataDir}/*.conf
    ln -sfn "${postgresqlConfig}" "${dataDir}/postgresql.conf"

    export PGDATA="${dataDir}"
    if [ "$(id -u)" = 0 ]; then
      su -s ${stdenv.shell} postgres -c '${cfg.package or postgresql}/bin/postgres ${toString flags}'
    else
      ${cfg.package or postgresql}/bin/postgres ${toString flags}
    fi;
  '');

  nixServeScript = writeScript "nix-serve-start" ''
    ${nix-serve}/bin/nix-serve --port 32062
  '';

  gpmScript = writeScript "gpm-start" ''
    ${gpm}/bin/gpm -m /dev/input/mouse0 -D -t imps2
  '';

  udevScript = writeScript "udev-start" ''
    mkdir -p /etc/udev/rules.d/
    while umount /etc/udev/rules.d; do true; done
    mount --bind ${udevConfigs}/etc/udev/rules.d/ /etc/udev/rules.d/
    mount /etc/udev/rules.d/ -o remount,ro,bind
    { ${coreutils}/bin/sleep 5;  ${eudev}/bin/udevadm trigger -c add;  ${eudev}/bin/udevadm trigger -c change;} &
    ${eudev}/bin/udevd
  '';
  udevConfigs = runCommand "udev-configs" {} ''
    mkdir -p "$out/etc/udev/rules.d"
    cd "$out/etc/udev/rules.d/"
    for i in ${toString storageUdevRules}       \
             ${fuse}/etc/udev/rules.d/*.rules   \
          ; do
      ln -sf "$i" .
    done
    ln -sf ${writeText "30-local-touchpad.rules" ''
      ACTION=="remove", GOTO="local_touchpad_end"
      SUBSYSTEM!="input", GOTO="local_touchpad_end"
      KERNEL!="event*", GOTO="local_touchpad_end"
      IMPORT{builtin}="input_id"
      ENV{ID_INPUT_TOUCHPAD}=="?*", SYMLINK+="input/touchpad%n"
      LABEL="local_touchpad_end"
    ''} 30-local-touchpad.rules
  '';

  fontsNixConfig = import ../fonts.nix {inherit pkgs;};

  XorgModules = with xorg; lib.concatLists (map (x: [x x.out]) 
    [ xf86videointel xorgserver.out xf86inputevdev xf86inputsynaptics ]);
  XNixConfig = import ../xserver-intel.nix {inherit pkgs;};
  XorgConfig = let
    cfg = XNixConfig; 
  in
  runCommand "xorg.conf" {} ''
        echo 'Section "Files"' >> $out
        for i in ${toString fontsNixConfig.fonts}; do
          if test "''${i:0:''${#NIX_STORE}}" == "$NIX_STORE"; then
            for j in $(find $i -name fonts.dir); do
              echo "  FontPath \"$(dirname $j)\"" >> $out
            done
          fi
        done

        for i in $(find ${toString XorgModules} -type d); do
          if test $(echo $i/*.so* | wc -w) -ne 0; then
            echo "  ModulePath \"$i\"" >> $out
          fi
        done

        echo 'EndSection' >> $out

        echo 'Section "ServerFlags"' >> $out
        echo '  Option "AllowMouseOpenFail" "on"' >> $out
        echo '  Option "AutoAddDevices" "on"' >> $out
        echo 'EndSection' >> $out

        echo "$config" >> $out
        echo 'Section "InputClass"' >> $out
        echo '  Identifier "Keyboard catchall"' >> $out
        echo '  MatchIsKeyboard "on"' >> $out
        echo '  Option "XkbRules" "base"' >> $out
        echo '  Option "XkbModel" "pc105"' >> $out
        echo '  Option "XkbLayout" "${XNixConfig.layout or "us"}"' >> $out
        echo '  Option "XkbOptions" "${XNixConfig.xkbOptions or ""}"' >> $out
        echo '  Option "XkbVariant" ""' >> $out
        echo '  Driver "evdev"' >> $out
        echo 'EndSection' >> $out
        echo 'Section "InputClass"' >> $out
        echo '  Identifier "Mouse catchall"' >> $out
        echo '  MatchIsPointer "on"' >> $out
        echo '  Driver "evdev"' >> $out
        echo 'EndSection' >> $out
        echo 'Section "ServerLayout"' >> $out
        echo '  Identifier "Layout[all]"' >> $out
        echo '  # Reference the Screen sections for each driver.  This will' >> $out
        echo '  # cause the X server to try each in turn.' >> $out
        echo '  Screen "Screen-intel[0]"' >> $out
        echo '  InputDevice "Main touchpad @7"' >> $out
        echo '  InputDevice "Main touchpad @8"' >> $out
        echo 'EndSection' >> $out
        echo '# For each supported driver, add a "Device" and "Screen"' >> $out
        echo '# section.' >> $out
        echo 'Section "Device"' >> $out
        echo '  Identifier "Device-intel[0]"' >> $out
        echo '  Driver "intel"' >> $out
        echo 'EndSection' >> $out
        echo 'Section "Screen"' >> $out
        echo '  Identifier "Screen-intel[0]"' >> $out
        echo '  Device "Device-intel[0]"' >> $out
        echo '  DefaultDepth 24' >> $out
        for i in 8 16 24 ; do 
        echo 'SubSection "Display"' >> $out
        echo "  Depth $i" >> $out
        echo '  Modes "1920x1080"' >> $out
        echo '  Virtual ${toString XNixConfig.virtualScreen.x} ${toString XNixConfig.virtualScreen.y}' >> $out
        echo 'EndSubSection' >> $out
        done;
        echo 'EndSection' >> $out
        echo '# Automatically enable the synaptics driver for all touchpads.' >> $out
        echo 'Section "InputClass"' >> $out
        echo '  Identifier "synaptics touchpad catchall"' >> $out
        echo '  MatchIsTouchpad "on"' >> $out
        echo '  Driver "synaptics"' >> $out
        echo '  Option "MinSpeed" "0.7"' >> $out
        echo '  Option "MaxSpeed" "3"' >> $out
        echo '  Option "AccelFactor" "0.05"' >> $out
        echo '  Option "MaxTapTime" "180"' >> $out
        echo 'Option "MaxTapMove" "220"' >> $out
        echo 'Option "TapButton1" "1"' >> $out
        echo 'Option "TapButton2" "2"' >> $out
        echo 'Option "TapButton3" "3"' >> $out
        echo '  Option "ClickFinger1" "1"' >> $out
        echo '  Option "ClickFinger2" "2"' >> $out
        echo '  Option "ClickFinger3" "3"' >> $out
        echo '  Option "VertTwoFingerScroll" "0"' >> $out
        echo '  Option "HorizTwoFingerScroll" "0"' >> $out
        echo '  Option "VertEdgeScroll" "1"' >> $out
        echo 'EndSection' >> $out
        echo 'Section "InputDevice"' >> $out
        echo '  Identifier "Main touchpad @7"' >> $out
        echo '  Driver "synaptics"' >> $out
        echo '  Option "Device" "/dev/input/touchpad7"' >> $out
        echo '  Option "SendCoreEvents" "on"' >> $out
        echo 'EndSection' >> $out
        echo 'Section "InputDevice"' >> $out
        echo '  Identifier "Main touchpad @8"' >> $out
        echo '  Driver "synaptics"' >> $out
        echo '  Option "Device" "/dev/input/touchpad8"' >> $out
        echo '  Option "SendCoreEvents" "on"' >> $out
        echo 'EndSection' >> $out
  '';
  XorgMesaDrivers = buildEnv {
    name = "opengl-drivers";
    paths = [ mesa_drivers mesa ];
  };
  XorgMesaDrivers32 = buildEnv {
    name = "opengl-drivers";
    paths = with pkgsi686Linux; [ mesa_drivers mesa ];
  };
  XorgScript = writeScript "xorg-start" 
  ''#!${stdenv.shell}
    if [ "$1" == "start" ]; then
      shift
      DISPLAY="''${1:-:0}"
      vt="$((7+''${DISPLAY#:}))"
      [ "$DISPLAY" != "''${DISPLAY#:}" ] || exit
      shift
      export XKB_BINDIR=${xorg.xkbcomp}/bin
      ln -sf ${XorgMesaDrivers} /run/opengl-driver
      ln -sf ${XorgMesaDrivers32} /run/opengl-driver-32
      LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/run/opengl-driver/lib:/run/opengl-driver-32/lib"
      ${xorg.xorgserver.out}/bin/Xorg -ac -logverbose -verbose -logfile "/var/log/X.''${DISPLAY#:}.log" \
        -terminate -config "${XorgConfig}" -xkbdir "${xkeyboard_config}/etc/X11/xkb"           \
        ${lib.optionalString (! (XNixConfig.enableTCPIP or false)) "-nolisten tcp"} $DISPLAY vt$vt \
        "$@"
    fi
  '';
  nixConfNix={
    maxJobs = 4;
    buildCores = 1;
    useChroot = true;
    chrootDirs = ["/home/repos"];
    binaryCaches = ["https://cache.nixos.org" "http://192.168.0.202:32062/"];
    trustedBinaryCaches = ["http://hydra.nixos.org" "http://192.168.0.202:32062/" "https://cache.nixos.org/"];
    gcKeepOutputs = true;
    gcKeepDerivations = true;
    binaryCachePublicKeys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "hydra.nixos.org-1:CNHJZBh9K4tP3EKF6FkkgeVYsS3ohTl+oS0Qa8bezVs="
      ];
  };
  nixConfText = ''
    build-users-group = nixbld
    build-max-jobs = ${toString (nixConfNix.maxJobs)}
    build-cores = ${toString (nixConfNix.buildCores)}
    build-use-chroot = ${if nixConfNix.useChroot then "true" else "false"}
    build-chroot-dirs = ${toString nixConfNix.chrootDirs} /bin/sh=${bash}/bin/sh ${glibc} ${bash}
    binary-caches = ${toString nixConfNix.binaryCaches}
    trusted-binary-caches = ${toString nixConfNix.trustedBinaryCaches}
    gc-keep-outputs = ${if nixConfNix.gcKeepOutputs then "true" else "false"}
    gc-keep-derivations = ${if nixConfNix.gcKeepDerivations then
      "true" else "false"} 
    binary-cache-public-keys = ${toString nixConfNix.binaryCachePublicKeys}
  '';
  nixConf = writeText "nix.conf" nixConfText;
  nixMachinesConf = writeText "nix-machines" (lib.concatMapStrings
      (machine:
       "${machine.sshUser}@${machine.hostName} "
       + (if machine ? system then machine.system else lib.concatStringsSep "," machine.systems)
       + " ${machine.sshKey} ${toString machine.maxJobs} "
       + (if machine ? speedFactor then toString machine.speedFactor else "1" )
       + " "
       + (if machine ? supportedFeatures then lib.concatStringsSep "," machine.supportedFeatures else "" )
       + " "
       + (if machine ? mandatoryFeatures then lib.concatStringsSep "," machine.mandatoryFeatures else "" )
       + "\n"
      )
      (with import ../nix-build-machines.nix; [gb-bxi7-4770r-1 gb-bxi7-4770r-1-i686]));

  fontsConf = makeFontsConf {fontDirectories = fontsNixConfig.fonts;};

  promptInit = ''
          # Provide a nice prompt.
          PROMPT_COLOR="1;31m"
          let $UID && PROMPT_COLOR="1;32m"
          PS1="\n\[\033[$PROMPT_COLOR\][\u@\h:\w]\\$\[\033[0m\] "
          if test "$TERM" = "xterm"; then
            PS1="\[\033]2;\h:\u:\w\007\]$PS1"
          fi
  '';
  profileScript = writeScript "profile" ''
    export PATH="$PATH:/run/current-system/sw/bin"
    export MODULE_DIR=/run/current-system/boot/kernel-modules/lib/modules
    export GIT_SSL_CAINFO=/etc/ssl/certs/ca-bundle.crt
    export INFOPATH=/run/current-system/sw/share/info
    export LD_LIBRARY_PATH=/run/opengl-driver/lib:/run/opengl-driver-32/lib
    export LOCALE_ARCHIVE=/run/current-system/sw/lib/locale/locale-archive
    export LOCATE_PATH=/var/cache/locatedb
    export NIXPKGS_CONFIG=/etc/nix/nixpkgs-config.nix
    export NIX_BUILD_HOOK=${nix.bin or nix.out or nix}/libexec/nix/build-remote.pl
    export NIX_CONF_DIR=/etc/nix
    export NIX_CURRENT_LOAD=/run/nix/current-load
    export NIX_PATH=/home/repos
    export NIX_REMOTE_SYSTEMS=/etc/nix/machines
    export OPENSSL_X509_CERT_FILE=/etc/ssl/certs/ca-bundle.crt
    export SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt
    export TERMINFO_DIRS=/run/current-system/sw/share/terminfo
    export TZDIR=/etc/zoneinfo

    ${promptInit}
  '';
  pamEnvironment=writeText "pam-environment" ''
    export PATH="$PATH:/run/current-system/sw/bin"
    export MODULE_DIR=/run/current-system/boot/kernel-modules/lib/modules
    export GIT_SSL_CAINFO=/etc/ssl/certs/ca-bundle.crt
    export INFOPATH=/run/current-system/sw/share/info
    export LD_LIBRARY_PATH=/run/opengl-driver/lib:/run/opengl-driver-32/lib
    export LOCALE_ARCHIVE=/run/current-system/sw/lib/locale/locale-archive
    export LOCATE_PATH=/var/cache/locatedb
    export NIXPKGS_CONFIG=/etc/nix/nixpkgs-config.nix
    export NIX_BUILD_HOOK=${nixUnstable}/libexec/nix/build-remote.pl
    export NIX_CONF_DIR=/etc/nix
    export NIX_CURRENT_LOAD=/run/nix/current-load
    export NIX_PATH=/home/repos
    export NIX_REMOTE_SYSTEMS=/etc/nix/machines
    export OPENSSL_X509_CERT_FILE=/etc/ssl/certs/ca-bundle.crt
    export SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt
    export TERMINFO_DIRS=/run/current-system/sw/share/terminfo
    export TZDIR=/etc/zoneinfo
  '';
  pamLoginConf = writeText "pam-login" ''
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
  pamSuConf = writeText ''pam-su'' ''
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
  pamPasswdConf = writeText ''passwd'' ''
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
  pamSshdConf = writeText ''passwd'' ''
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

  sshdConfig = writeText "sshd_config" ''
    GatewayPorts clientspecified
    PermitRootLogin no
    UseDNS no
    UsePAM yes
    VersionAddendum nixos@401a0bf1
    X11Forwarding yes
  '';
  sshdScript = writeScript "sshd-start" ''
    if [ "$1" = start ]; then
      ${openssh}/bin/sshd -D -e -f ${sshdConfig} -h /var/cert/ssh/ssh_host_rsa_key -h /var/cert/ssh/ssh_host_dsa_key -h /var/cert/ssh/ssh_host_ecdsa_key
    fi
  '';
  cronScript = writeScript "cron-start" ''
    if [ "$1" = start ]; then
      mkdir -p -m 0710 /var/cron/ 
      ${cron}/sbin/cron -n
    fi
  '';
  gogocScript = writeScript "gogoc-start" 
  (let
    serviceConf = (import ../services-main.nix 
      {inherit pkgs; config = {};}).gogoclient;
  in ''
    if [ "$1" = start ]; then
      mkdir -m 0600 -p /run/gogoc/
      cat ${gogoclient}/share/${gogoclient.name}/gogoc.conf.sample |
        sed -re "
          s|^userid=|&${serviceConf.username}|;
          s|^passwd=|&${lib.optionalString (serviceConf.password != "") 
            "$(cat ${serviceConf.password})"}|
          s|^server=.*|server=${serviceConf.server}|
          s|^auth_method=.*|auth_method=${serviceConf.authMethod or "any"}|
        " > /run/gogoc/gogoc.conf
      ${gogoclient}/bin/gogoc -y -n -f /run/gogoc/gogoc.conf
    fi
  '');
  cupsBindir = pkgs.buildEnv {
    name = "cups-progs";
    paths = 
      [ cups.out pkgs.ghostscript pkgs.cups_filters 
        pkgs.perl pkgs.coreutils pkgs.gnused pkgs.bc pkgs.gawk pkgs.gnugrep
        pkgs.hplip pkgs.foo2zjs pkgs.foomatic_filters 
        pkgs.samba
      ];
    pathsToLink = [ "/lib/cups" "/share/cups" "/bin" "/etc/cups" ];
    postBuild = "";
    ignoreCollisions = true;
  };
  cupsCupsdConfig = writeText "cupsd.conf" ''
        LogLevel info

        ${pkgs.lib.concatMapStrings (addr: ''
          Listen ${addr}
        '') ["127.0.0.1:631"]}
        Listen /var/run/cups/cups.sock

        SetEnv PATH ${cupsBindir}/lib/cups/filter:${cupsBindir}/bin:${cupsBindir}/sbin

        Browsing On
        BrowseOrder allow,deny
        BrowseAllow @LOCAL

        DefaultAuthType Basic

        <Location />
          Order allow,deny
          Allow localhost
        </Location>

        <Location /admin>
          Order allow,deny
          Allow localhost
        </Location>

        <Location /admin/conf>
          AuthType Basic
          Require user @SYSTEM
          Order allow,deny
          Allow localhost
        </Location>

        <Policy default>
          <Limit Send-Document Send-URI Hold-Job Release-Job Restart-Job Purge-Jobs Set-Job-Attributes Create-Job-Subscription Renew-Subscription Cancel-Subscription Get-Notifications Reprocess-Job Cancel-Current-Job Suspend-Current-Job Resume-Job CUPS-Move-Job>
            Require user @OWNER @SYSTEM
            Order deny,allow
          </Limit>

          <Limit Pause-Printer Resume-Printer Set-Printer-Attributes Enable-Printer Disable-Printer Pause-Printer-After-Current-Job Hold-New-Jobs Release-Held-New-Jobs Deactivate-Printer Activate-Printer Restart-Printer Shutdown-Printer Startup-Printer Promote-Job Schedule-Job-After CUPS-Add-Printer CUPS-Delete-Printer CUPS-Add-Class CUPS-Delete-Class CUPS-Accept-Jobs CUPS-Reject-Jobs CUPS-Set-Default>
            AuthType Basic
            Require user @SYSTEM
            Order deny,allow
          </Limit>

          <Limit Cancel-Job CUPS-Authenticate-Job>
            Require user @OWNER @SYSTEM
            Order deny,allow
          </Limit>

          <Limit All>
            Order deny,allow
          </Limit>
        </Policy>

  '';
  cupsFilesConfig = writeText "files.conf" ''

        SystemGroup root wheel

        ServerBin ${cupsBindir}/lib/cups
        DataDir ${cupsBindir}/share/cups

        AccessLog stderr
        ErrorLog stderr
        PageLog stderr

        TempDir /tmp/cups

        # User and group used to run external programs, including
        # those that actually send the job to the printer.  Note that
        # Udev sets the group of printer devices to `lp', so we want
        # these programs to run as `lp' as well.
        User cups
        Group lp
  '';
  cupsClientConfig = writeText "client.conf" "";
  cupsScript = writeScript "cups-start" ''
            mkdir -m 0755 -p /etc/cups
            mkdir -m 0700 -p /var/cache/cups
            mkdir -m 0700 -p /var/spool/cups
            mkdir -m 0755 -p /tmp/cups
            chown cups:lp /tmp/cups

            ln -sf ${cupsCupsdConfig} /etc/cups/cupsd.conf
            ln -sf ${cupsFilesConfig} /etc/cups/cups-files.conf
            ln -sf ${cupsClientConfig} /etc/cups/client.conf

            rm /etc/cups/*.{types,convs}
            ln -sf ${cupsBindir}/share/cups/mime/*.{types,convs} /etc/cups

            ${pkgs.cups.out}/sbin/cupsd
  '';
}
