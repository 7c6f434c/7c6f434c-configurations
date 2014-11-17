#! /bin/sh

echo "If you have global IPv6 address correctly set up, this should show the HTTP header from SixXS site" | fmt -w 72
echo

curl -I -6 https://www.sixxs.net/
