{pkgs ? import <nixpkgs> {} }:
with pkgs;
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
      postgres -c ${cfg.package or postgresql}/bin/postgres ${toString flags}
    fi;
  '');
}
