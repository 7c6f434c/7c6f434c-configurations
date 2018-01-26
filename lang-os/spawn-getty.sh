#! /bin/sh

    export CONSOLE_DEVICE="/dev/tty$1"
    chmod 0600 "$CONSOLE_DEVICE"
    setfacl -b "$CONSOLE_DEVICE"
    chown root "$CONSOLE_DEVICE"
    setsid -w agetty tty$1 -l /run/current-system/bin/use-console -o "$2"
    chmod 0600 "$CONSOLE_DEVICE"
    setfacl -b "$CONSOLE_DEVICE"
    chown root "$CONSOLE_DEVICE"
