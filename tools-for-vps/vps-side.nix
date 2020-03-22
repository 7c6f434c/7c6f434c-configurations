with import <nixpkgs> {};
with rec {
        globalLinks = pkgs.runCommand "global-links" {} ''
          ncp () {
                mkdir -p "$2"
                cp "$1" "$2"/"$(basename "$1" | sed -re 's/^[a-z0-9]+-//')"
          }
          mkdir -p "$out/global/"
          cd "$out/global/"
          ncp ${./ii-starter} ./home/raskin
          ncp ${./ii-summarise} ./home/raskin
          ncp ${./openvpn.private/server.conf} ./etc/openvpn
          ncp ${./openvpn.private/server-tcp.conf} ./etc/openvpn
          ncp ${./ii.service} ./lib/systemd/system
          ncp ${./openvpn.service} ./lib/systemd/system
          ncp ${./openvpn-tcp.service} ./lib/systemd/system
          ncp ${./nat} ./etc/network/if-up.d
          ncp ${./env} ./etc
          ncp ${./nginx.conf} ./var/nginx/conf
          ncp ${./nginx.service} ./lib/systemd/system
          ncp ${./dehydrated.conf} ./var/dehydrated
          ncp ${./domains.txt.private} ./var/dehydrated
          ncp ${./dehydrated.service} ./lib/systemd/system
          ncp ${./dehydrated.timer} ./lib/systemd/system
          ncp ${./postfix/main.cf} ./etc/postfix
          ncp ${./postfix/master.cf} ./etc/postfix
          ncp ${./postfix.service} ./lib/systemd/system

          sed -e 's/@@@/'"$(cat ${./domains.txt.private} | xargs)"'/' -i ./etc/postfix/main.cf
          for i in $(cat ${./domains.txt.private}); do
                sed -e "s/@@@/$i/g" < ${./nginx.ssl.conf} >> ./var/nginx/conf/nginx.ssl.conf
          done

          mkdir -p ./var/log/nginx ./var/www ./var/www/.well-known ./var/nginx/logs \
            ./var/dehydrated/conf.d ./var/dehydrated/acme-challenge \
            ./var/lib/postfix/queue ./var/spool/mail ./var/mail ./var/spool/postfix
        '';
        remoteDeploy = pkgs.writeScriptBin "remote-deploy" ''
          for i in root raskin matrix; do
            su - $i sh -c 'grep /etc/env .bashrc || echo . /etc/env >> .bashrc'
          done
          (
            cd ./tools/global;
            find . -type d | while read i; do mkdir -vp /$i; done
            find . -type f | while read i; do ln -vsfT $(readlink -f $i) /$i; done
          )
          for i in ii openvpn openvpn-tcp nginx dehydrated.timer postfix; do systemctl enable $i; done;
          (
            cd ./tools/global/lib/systemd/system
            for i in *; do
              systemctl reenable /lib/systemd/system/$i
            done
          )
          for i in nginx; do systemctl restart $i; done

          for u in www-data postfix; do
            grep "^$u:" /etc/passwd || useradd -s /sbin/nologin -r $u
          done
          for g in postfix postdrop mail; do
            grep "^$g:" /etc/group || groupadd -r $g
          done

          ln -sfT /var/dehydrated/acme-challenge /var/www/.well-known/acme-challenge

          usermod -G mail postfix
          chgrp mail /var/mail /var/spool/mail /var/spool/postfix /var/lib/postfix/queue
          chmod g+ws /var/mail /var/spool/mail /var/spool/postfix
          chown root /var/lib/postfix/queue
          chmod u=rwx,og=rx /var/lib/postfix/queue

          touch /etc/aliases
          /root/tools/bin/newaliases
        '';
        toolset = pkgs.buildEnv {
          name = "tools-for-vps";
          paths = [ openvpn matrix-synapse ii screen nginx less
                rsync git monotone fossil mercurial vim strace transmission
                glibcLocales host dnsutils mtr htop iotop hping socat iftop
                curl wget youtube-dl jemalloc nix dehydrated netcat tcpdump
                alpine postfix dovecot shared_mime_info
                globalLinks remoteDeploy
          ];
        };
};
toolset

