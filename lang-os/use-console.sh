#! /bin/sh
    if test "$1" = "--"; then shift; fi
    test -n "$CONSOLE_DEVICE" && chown "$1" "$CONSOLE_DEVICE"
    chown "$1" "$(tty)"
    mkdir -p /run/user/$(id -u "$1")
    chown "$1" -R /run/user/$(id -u "$1")
    chmod u+rwx /run/user/$(id -u "$1")
    unset "CONSOLE_DEVICE"
    /run/current-system/sw/bin/su nobody -s /bin/sh -c "/run/wrappers/bin/su - '$1' ${2:+-c $2}"
