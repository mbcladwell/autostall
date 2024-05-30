#!/bin/bash
rm ./ChangeLog
rm -Rf  ./build-aux
rm ./configure.ac
rm ./Makefile.am
rm ./pre-inst-env.in
rm ./guix.scm
rm ./hall.scm
rm ./scripts/*.conf
rm ./scripts/*.sql
rm ./scripts/guix-install-mod.sh
rm ./scripts/install-lnpg.sh
rm ./scripts/prep-for-lnpg.sh
rm ./scripts/install-pg.sh
rm ./*.go
rm ./labsolns/*.go
rm ./labsolns-0.1.tar.gz
hall init --convert --author "mbc" labsolns --execute
hall scan -x
hall build -x
cp /home/mbc/temp/testfiles/guix.scm .
cp /home/mbc/temp/psqlfiles/*.* ./scripts

