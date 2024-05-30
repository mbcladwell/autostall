#! /bin/bash

mkdir -p /tmp/myapp/tmp/cache '/home/'$USER'/.config/myapp'
cp PATH_INTO_STORE/share/guile/site/3.0/myapp/conf/artanis.conf $HOME/.config/myapp
sudo chmod u+w $HOME/.config/myapp/artanis.conf
