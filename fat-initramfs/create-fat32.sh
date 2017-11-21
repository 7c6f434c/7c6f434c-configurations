#! /bin/sh

dd if=/dev/zero bs=1M count="${2:-1024}" of="$1"
mkdosfs -F32 "$1"
