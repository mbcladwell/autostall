#!/bin/bash
export LC_ALL="C"
mkdir /tmp/myapp/tmp/cache
cd PATH_INTO_STORE/share/guile/site/3.0/myapp
art work -h0.0.0.0 --config=$HOME/.config/myapp/artanis.conf
