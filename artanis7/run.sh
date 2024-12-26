#! /bin/bash

cd /home/mbc/projects/artanis
git add .
git commit -a -S -m "autocommit"
git push
     
COMMITID=$(git log -1 --pretty=format:"%H")
cd /home/mbc/projects/autostall/artanis7

sed -i "s/[ ]*(let ((commit \"[a-z0-9]*\")/             (let ((commit \"$COMMITID\")/" ./guix.scm
sed -i "s/[ ]*(let ((commit \"[a-z0-9]*\")/             (let ((commit \"$COMMITID\")/" ./artanis-07.scm

COMMITHASH=$(guix hash -x --serializer=nar /home/mbc/projects/artanis)
sed -i "s/[ ]*(base32 \"[a-z0-9]*\"))))/             (base32 \"$COMMITHASH\"))))/" ./guix.scm
sed -i "s/[ ]*(base32 \"[a-z0-9]*\"))))/             (base32 \"$COMMITHASH\"))))/" ./artanis-07.scm


##guix package --install-from-file=guix.scm

cp ./artanis-07.scm ~/projects/labsolns/labsolns/artanis-07.scm

cd /home/mbc/projects/labsolns
git add .
git commit -m "autocommit"
git push

cd /home/mbc
guix pull
guix package -i artanis
