with import <nixpkgs> {};
with rec {
        fontsConf = makeFontsConf {
          fontDirectories = [ dejavu_fonts ];
        };
        globalLinks = pkgs.runCommand "global-links" {} ''
          ncp () {
                mkdir -p "$2"
                local tgt="$2"/"$(basename "$1" | sed -re 's/^[a-z0-9]+-//')"
                cp "$1" "$tgt"
                chmod u+w "$tgt"
          }
          mkdir -p "$out/global/"
          cd "$out/global/"
          ncp ${./ii-starter-libera-chat} ./home/raskin
          ncp ${./ii-starter-oftc} ./home/raskin
          ncp ${./ii-summarise} ./home/raskin
          ncp ${./openvpn.private/server.conf} ./etc/openvpn
          ncp ${./openvpn.private/server-tcp.conf} ./etc/openvpn
          ncp ${./ii-libera-chat.service} ./lib/systemd/system
          ncp ${./ii-oftc.service} ./lib/systemd/system
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
          ncp ${./postfix-key-concat.service} ./lib/systemd/system
          ncp ${./postfix-key-concat.timer} ./lib/systemd/system
          ncp ${./postfix/main.cf} ./etc/postfix
          ncp ${./postfix/master.cf} ./etc/postfix
          ncp ${./postfix.private/pointless-postfix.crt} ./etc/postfix
          ncp ${./postfix.service} ./lib/systemd/system
          ncp ${./dovecot.conf} ./etc/dovecot
          ncp ${./pam/dovecot} ./etc/pam.d
          ncp ${./dovecot.service} ./lib/systemd/system
          ncp ${./dovecot.private/pointless-dovecot.key} ./etc/dovecot
          ncp ${./dovecot.private/pointless-dovecot.crt} ./etc/dovecot
          ncp ${fontsConf} ./etc/fonts
          ncp ${./screenrc} ./etc
          ncp ${./shadowsocks.service} ./lib/systemd/system
          ncp ${./grab-ntp-time.service} ./lib/systemd/system

          sed -e 's/@@@/'"$(cat ${./domains.txt.private} | xargs)"'/' -i ./etc/postfix/main.cf
          sed -e 's^@pam@^${pam}^' -i ./etc/pam.d/dovecot
          for i in $(cat ${./domains.txt.private}); do
                sed -e "s/@@@/$i/g" < ${./nginx.ssl.conf} >> ./var/nginx/conf/nginx.ssl.conf
                sed -e "s/@@@/$i/g" < ${./dovecot.conf.per-domain} >> ./etc/dovecot/dovecot.conf
                sed -e "s/@@@/$i/g" < ${./postfix/sni} >> ./etc/postfix/sni
          done
          echo "mail_plugin_dir = ${dovecot}/lib/dovecot" >> ./etc/dovecot/dovecot.conf

          mkdir -p ./var/log/nginx ./var/www ./var/www/.well-known ./var/nginx/logs \
            ./var/dehydrated/conf.d ./var/dehydrated/acme-challenge \
            ./var/lib/postfix/queue ./var/spool/mail ./var/mail ./var/spool/postfix \
            ./var/cache/nginx
          ln -sfT ${dovecot}/lib/dovecot ./etc/dovecot/modules

          sed -e "s/@@@addr@@@/$(cat ${./target.private})/g; s/@@@password@@@/$(cat ${./shadowsocks.private/password} | tr -d '\n' | base64)/g" -i ./lib/systemd/system/shadowsocks.service
        '';
        remoteDeploy = pkgs.writeScriptBin "remote-deploy" ''
          for u in raskin matrix; do
            useradd -m -s /bin/bash $u
          done
          for i in root raskin matrix; do
            su - $i sh -c 'grep /etc/env .bashrc || echo . /etc/env >> .bashrc'
          done
          (
            cd ./tools/global;
            find . -type d | while read i; do mkdir -vp /$i; done
            find . -type f | while read i; do ln -vsfT $(readlink -f $i) /$i; done
            find . -type l | while read i; do ln -vsfT $(readlink -f $i) /$i; done
          )
          for u in www-data postfix dovenull dovecot; do
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
          ${postfix}/bin/postmap -c /etc/postfix -F /etc/postfix/sni

          for i in nginx dovecot; do
            systemctl restart $i;
          done
          for i in ii ii-libera-chat ii-oftc \
                   openvpn openvpn-tcp \
                   nginx dehydrated.timer postfix-key-concat.timer postfix dovecot \
                   shadowsocks grab-ntp-time \
                   ; do 
            systemctl enable $i; systemctl start $i; systemctl reload $i;
          done;
          (
            cd ./tools/global/lib/systemd/system
            for i in *; do
              rm -v /etc/systemd/$i /etc/systemd/*/$i
              systemctl enable /lib/systemd/system/$i
            done
          )

        '';
        toolset = pkgs.buildEnv {
          name = "tools-for-vps";
          paths = [ openvpn matrix-synapse ii screen nginx less
                rsync git monotone fossil mercurial vim strace transmission
                glibcLocales host dnsutils mtr htop iotop hping socat iftop
                curl wget youtube-dl jemalloc nix dehydrated netcat tcpdump
                yt-dlp
                alpine postfix dovecot shared-mime-info
                tigervnc
                scite
                gsettings-desktop-schemas gtk3
                xorg.xinit xorg.twm icewm rxvt_unicode
                ncdu
                globalLinks remoteDeploy
                shadowsocks-rust ntp
          ];
        };
};
toolset

