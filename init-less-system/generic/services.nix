self: {
  openssh = cfg: {
    name = "openssh";
    script = (self.tools.nixosServiceScript "sshd" {services.openssh = cfg;});
  };
  nix-serve = cfg: {
    name = "nix-serve";
    scriptCode = ''#! ${self.pkgs.stdenv.shell}
      grep '^nix-serve:' /etc/passwd || ${self.pkgs.shadow}/bin/useradd -r -s /run/current-system/sw/sbin/nologin -g nogroup nix-serve
      su -s ${self.pkgs.stdenv.shell} nix-serve -c "NIX_REMOTE=daemon NIX_SECRET_KEY_FILE=${cfg.secretKeyFile} '${self.pkgs.nix-serve}/bin/nix-serve' --port ${toString cfg.port}"
    '';
  };
  postgresql = cfg: {
    name = "postgresql";
    scriptCode = "#! ${self.pkgs.stdenv.shell}\n" + 
    ''
      grep '^postgres:' /etc/passwd || ${self.pkgs.shadow}/bin/useradd -r -s /run/current-system/sw/sbin/nologin -g nogroup postgres
      su postgres -s "${self.pkgs.stdenv.shell}" -c "${
        (self.tools.nixosServiceScript "postgresql" 
          {services.postgresql = cfg;})}"
    '';
  };
  bind = cfg: (rec {
    name = "bind";
    config = (self.nixos {configuration={services.bind = cfg;};}).config.services.bind.configFile;
    scriptCode = "#! ${self.pkgs.stdenv.shell}\n" + ''
      ${self.pkgs.coreutils}/bin/mkdir -p /run/named
      ${self.pkgs.gnugrep}/bin/grep '^named:' /etc/passwd || ${self.pkgs.shadow}/bin/useradd -r -s /run/current-system/sw/sbin/nologin -g nogroup named
      ${self.pkgs.coreutils}/bin/chown named -R /run/named
      ${self.pkgs.bind}/bin/named -u named ${self.lib.optionalString (cfg.ipv4Only or false) "-4"} -c ${config} -f -g
    '';
  });
  cron = cfg: (rec {
    name = "cron";
    config = self.pkgs.writeText "crontab" cfg.crontab;
    scriptCode = ''#! ${self.pkgs.stdenv.shell}
      cp "${config}" /etc/crontab
      chmod 600 /etc/crontab
      ${self.pkgs.cron}/bin/cron -n
    '';
  });
  cups = cfg: let
    nixosWithCups = self.nixos {configuration.services.printing = cfg;};
  in
  {
    name = "cups";
    scriptCode = self.tools.shebang
      + ''
        grep '^lp:' /etc/group || ${self.pkgs.shadow}/bin/groupadd -r lp
        grep '^cups:' /etc/passwd || ${self.pkgs.shadow}/bin/useradd -g lp -r -s /var/current-system/sw/bin/nologin cups
      ''
      + nixosWithCups.config.systemd.services.cups.preStart 
      + ''
        ${self.pkgs.cups.out}/bin/cupsd -f
      ''
      ;
  };
  xorg = cfg: let
    nixosWithX = self.nixos {configuration = self.lib.recursiveUpdate cfg {
      services.xserver = {
        enable = true;
        exportConfiguration = true;
        inputClassSections = [
          ''
            Identifier "Keyboard catchall evdev"
            MatchIsKeyboard "on"
            Option "XkbRules" "base"
            Option "XkbModel" "${cfg.services.xserver.xkbModel or "pc105"}"
            Option "XkbLayout" "${cfg.services.xserver.layout or "us"}"
            Option "XkbOptions" "${cfg.services.xserver.xkbOptions or ""}"
            Option "XkbVariant" "${cfg.services.xserver.xkbVariant or ""}"
            Driver "evdev"
          ''
          ''
            Identifier "Mouse catchall evdev"
            MatchIsPointer "on"
            Driver "evdev"
          ''
        ];
      };
      hardware.opengl.enable = true;
    };};
  in
  (rec {
    name = "xorg";
    inherit nixosWithX;
    scriptName = "xorg";
    configName = "xorg.conf";
    config = self.tools.nixosEtcSelect nixosWithX "X11/xorg.conf";
    scriptCode = ''
      udevadm hwdb --update
      udevadm trigger
      udevadm settle
      export DISPLAY="''${1:-:0}"
      vt="$((7+''${DISPLAY#:}))"
      [ "$DISPLAY" != "''${DISPLAY#:}" ] || exit
      shift
      export XKB_BINDIR=${self.pkgs.xorg.xkbcomp}/bin
      ln -fs "${nixosWithX.config.hardware.opengl.package}" /run/opengl-driver
      ln -fs "${nixosWithX.config.hardware.opengl.package32}" /run/opengl-driver-32
      export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/run/opengl-driver/lib:/run/opengl-driver-32/lib"

      { while ! fuser /tmp/.X11-unix/X''${DISPLAY#:}; do sleep 1; done; udevadm trigger ; } & 

      ${self.pkgs.xorg.xorgserver.out}/bin/Xorg -ac -logverbose -verbose -logfile "/var/log/X.''${DISPLAY#:}.log" \
        -config "${config}" -xkbdir "${self.pkgs.xkeyboard_config}/etc/X11/xkb"           \
        ${if (! (cfg.services.xserver.enableTCP or false)) then
          "-nolisten tcp" else "-listen tcp"} $DISPLAY vt$vt \
        "$@"
    '';
  });
  udev = cfg: {
    name = "udev";
    scriptCode = ''
      udevadm trigger -c add
      udevadm trigger -c change
      udevadm trigger
      udevadm settle || true
      { sleep 5; udevadm trigger; } &
      udevd
    '';
  };
  dbus = cfg: {
    name = "dbus";
    scriptCode = ''
      mkdir /run/dbus
      kill $(cat /run/dbus/pid)
      rm /run/dbus/pid
      dbus-launch --config-file "${self.pkgs.dbus}"/share/dbus-1/system.conf > /run/dbus.env &
    '';
  };
}
