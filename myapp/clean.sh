#!/bin/bash
rm ./ChangeLog
rm -Rf  ./build-aux
rm ./configure.ac
rm ./Makefile.am
rm ./pre-inst-env.in
rm ./guix.scm
rm ./hall.scm
rm ./*.go
rm ./myapp/*.go
rm ./scripts/*.*
rm ./myapp-0.1.tar.gz
rm ./myapp/lib/*.go
rm ./myapp/app/api/*.go
rm ./myapp/app/controllers/*.go
hall init --convert --author "mbc" myapp --execute
hall scan -x
hall build -x
cp /home/mbc/syncd/tobedeleted/myapp/guix.scm .
cp /home/mbc/syncd/tobedeleted/myapp/hall.scm .
cp /home/mbc/syncd/tobedeleted/myapp/Makefile.am .
autoreconf -vif && ./configure && make


cp /home/mbc/syncd/tobedeleted/myapp/scripts/*.sh ./scripts


make dist



scp -i /home/mbc/labsolns.pem /home/mbc/projects/myapp/myapp-0.1.tar.gz admin@ec2-3-140-251-71.us-east-2.compute.amazonaws.com:.
scp -i /home/mbc/labsolns.pem /home/mbc/syncd/tobedeleted/myapp/guix.scm admin@ec2-3-140-251-71.us-east-2.compute.amazonaws.com:.
##guix package --install-from-file=guix.scm
##source /home/mbc/.guix-profile/etc/profile
