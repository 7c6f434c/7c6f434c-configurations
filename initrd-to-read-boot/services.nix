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
      mkdir -m 0701 -p ${dataDir}
      if [ "$(id -u)" = 0 ]; then
        chown -R postgres ${dataDir}
      su -s ${stdenv.shell} postgres -c 'initdb -U root'
      else
        # For non-root operation.
        initdb
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

  nixBinaryCacheScript = writeScript "nix-binary-cache-start" ''
    export PATH="$PATH:${coreutils}/bin"
    ${nix-binary-cache}/bin/nix-binary-cache-start --port 32062
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

  XorgModules = with xorg; [xf86videointel xorgserver xf86inputevdev xf86inputsynaptics];
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
  XorgScript = writeScript "xorg-start" 
  ''#!${stdenv.shell}
    if [ "$1" == "start" ]; then
      shift
      DISPLAY="''${1:-:0}"
      vt="$((7+''${DISPLAY#:}))"
      [ "$DISPLAY" != "''${DISPLAY#:}" ] || exit
      shift
      export XKB_BINDIR=${xorg.xkbcomp}/bin
      ln -sf ${mesa_drivers} /run/opengl-driver
      mkdir -p /etc/fonts
      mount --bind ${fontsConf} /etc/fonts || {
        touch /etc/fonts/fonts.conf
        mount --bind ${fontsConf} /etc/fonts/fonts.conf
      }
      LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/run/opengl-driver/lib"
      ${xorg.xorgserver}/bin/Xorg -ac -logverbose -verbose -logfile "/var/log/X.''${DISPLAY#:}.log" \
        -terminate -config "${XorgConfig}" -xkbdir "${xkeyboard_config}/etc/X11/xkb"           \
        ${lib.optionalString (! (XNixConfig.enableTCPIP or false)) "-nolisten tcp"} $DISPLAY vt$vt \
        "$@"
    fi
  '';

  fontsConf = makeFontsConf {fontDirectories = fontsNixConfig.fonts;};
}
