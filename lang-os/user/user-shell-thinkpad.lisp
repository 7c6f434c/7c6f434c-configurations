; vim:filetype=lisp

(defvar *rc-path*
 (or
  (uiop:getenv "USER_LISP_SHELL_RC")
  *compile-file-pathname*
  *load-pathname*))

(load (format nil
              "~a/src/nix/lang-os/user/user-shell.lisp"
              (uiop:getenv "HOME")))

(defun im-browsers ()
  (firefox (list "https://web.telegram.org/")
           :pass-stderr nil :pass-stdout nil :wait nil
           :no-close t :stumpwm-tags "cat/e-im im telegram no-auto-tags"
           :javascript t
           :socks-proxy 1080)
  (firefox (list "https://web.skype.com/")
           :pass-stderr nil :pass-stdout nil :wait nil
           :no-close t :stumpwm-tags "cat/e-im im skype no-auto-tags"
           :javascript t
           :socks-proxy 1080)
  )

(defun email-browsers ()
  (firefox (list "https://email.mccme.ru/")
           :pass-stderr nil :pass-stdout nil :wait nil
           :no-close t :stumpwm-tags "cat/em-email email mail mccme no-auto-tags"
           :data "/home/raskin/fallout/"
           :javascript t
           :socks-proxy 1080)
  (firefox (list "https://github.com/notifications/")
           :pass-stderr nil :pass-stdout nil :wait nil
           :no-close t :stumpwm-tags "cat/em-email email mail github no-auto-tags"
           :javascript t
           :socks-proxy 1080)
  )

(defun matrix-term ()
  (& urxvt 
     -name "weechat:matrix:dev" -title "Weechat: Matrix: dev.mccme.ru"
     -e sh -c "https_proxy= weechat -d ~/.weechat-dev"))

(defun vps-term ()
  (& sh -c "ssh-window \"$(cat ~/.vps-ssh)\""))

(defun communication-windows ()
  (vps-term)
  (matrix-term)
  (im-browsers)
  (email-browsers))

(defun enter-labri (&rest args &key (brightness 400) (extra-ips `())
                          (location "LaBRI.fr")
                          &allow-other-keys)
  (apply
    'enter-location
    :brightness brightness
    :extra-ips extra-ips
    :location location
    args)
  (! x-options))

(defun enter-ratmino (&rest args &key (brightness 20) (extra-ips `())
                          (location "ratmino")
                          &allow-other-keys)
  (apply
    'enter-location
    :brightness brightness
    :extra-ips extra-ips
    :location location
    args)
  (! x-options))

(defun enter-mccme (&rest args &key (brightness 20) (extra-ips `())
                          (location "mccme")
                          &allow-other-keys)
  (apply
    'enter-location
    :brightness brightness
    :extra-ips extra-ips
    :location location
    args)
  (! x-options))

(defun enter-tum (&rest args &key (brightness 400) (extra-ips `())
                        (location "in.tum.de")
                        &allow-other-keys)
  (! x-options)
  (apply
    'enter-location
    :brightness brightness
    :extra-ips extra-ips
    :location location
    :extra-requests `((activate-interface "eth0")
                      (activate-interface "eth1")
                      (dhcp-resolv-conf))
    args)
  (! x-options)
  (when
    (or
      (ethernet-attached "eth0")
      (ethernet-attached "eth1")
      )
    (ask-with-auth
      (:presence t)
      `(progn
         (kill-wifi "wlan0")
         ,(if (ethernet-attached "eth0") `(dhclient "eth0" t) `(progn))
         ,(if (ethernet-attached "eth1") `(dhclient "eth1" t) `(progn))
         ))
    (! proxy-restart (format nil "~a/src/rc/squid/direct.squid" ($ :home))))
  (! x-options))

(defun enter-home-poing (&rest args)
  (apply 'enter-home
         (append args
                 (list :location "home@Poing"
                       :extra-requests `((local-resolv-conf))))))

(defun disconnect ()
  (alexandria:write-string-into-file
    "10" (format nil "~a/.watchperiod" (uiop:getenv "HOME"))
    :if-exists :supersede)
  (! web-stream-updater-starter quit)
  (ask-with-auth 
    (:presence t)
    `(list 
       (set-cpu-frequency "min")
       (set-brightness 1)))
  (! x-options)
  )

(defun xrandr-home-poing ()
  (! xrandr --output "VGA-1-2" --above "LVDS-1")
  (! xrandr --fb 4000x3000)
  (! pkill compton)
  (sleep 0.1)
  (! pkill -abrt compton)
  (sleep 0.1)
  (! pkill -kill compton)
  (sleep 0.5)
  (& compton --dbe))
