#! /bin/sh
curl https://addons.mozilla.org/en-US/firefox/addon/downthemall/ | tr '"' '\n' | grep '^https://.*[.]xpi' | head -n 1 
