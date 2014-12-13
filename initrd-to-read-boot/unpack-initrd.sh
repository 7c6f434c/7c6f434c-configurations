#! /bin/sh

cat "$1" | ( chmod u+w -R "$2"; rm -rf "$2"; mkdir "$2"; cd "$2"; gunzip | cpio -i ) 
