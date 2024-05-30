#! /usr/bin/bash

guix package --install-from-file=limsn.scm
var_1=$(guix package --list-installed | grep limsn | cut -f 4)
$var_1/share/guile/site/3.0/limsn/bin/start-limsn.sh 
