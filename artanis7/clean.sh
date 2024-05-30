#!/bin/bash
rm ./ChangeLog
rm -Rf  ./build-aux
rm ./configure.ac
rm ./Makefile.am
rm ./pre-inst-env.in
rm ./guix.scm
rm ./hall.scm
rm ./*.go
rm ./artanis/*.go
rm ./artanis/*.go
rm ./artanis/commands/*.go
rm ./artanis/mvc/*.go
rm ./artanis/security/*.go
rm ./artanis/server/*.go
rm ./artanis/sql-mapping/*.go
rm ./artanis/third-party/*.go
rm ./artanis/third-party/json/upstream/*.go
rm ./artanis/third-party/redis/upstream/*.go
rm ./artanis/tpl/*.go
rm ./artanis/webapi/*.go
rm ./artanis/websocket/*.go
rm ./artanis/test-suite/test-suite/*.go
rm ./artanis-0.5.3.tar.gz
export ACLOCAL_PATH=/usr/share/aclocal
hall init --convert --author "mbc" artanis --execute
hall scan -x
hall build -x
cp /home/mbc/syncd/tobedeleted/artanis52/guix.scm .
cp /home/mbc/syncd/tobedeleted/artanis52/hall.scm .
cp /home/mbc/syncd/tobedeleted/artanis52/Makefile.am .

autoreconf -vif && ./configure && make

make dist

##git add .
##git commit -a -S -m "changes for server"
##git push

##scp -i /home/mbc/labsolns.pem ./shinyln-0.1.tar.gz admin@ec2-18-189-31-114.us-east-2.compute.amazonaws.com:.
##scp -i /home/mbc/labsolns.pem /home/mbc/syncd/tobedeleted/shinyln/guix.scm admin@ec2-18-189-31-114.us-east-2.compute.amazonaws.com:.
##guix package --install-from-file=guix.scm
##source /home/mbc/.guix-profile/etc/profile
