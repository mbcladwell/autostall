#! /bin/bash

cd /home/mbc/projects/conmanv4
git add .
git commit -a -S -m "autocommit"
git push
     
COMMITID=$(git log -1 --pretty=format:"%H")
cd /home/mbc/syncd/tobedeleted/conman

sed -i "s/[ ]*(let ((commit \"[a-z0-9]*\")/             (let ((commit \"$COMMITID\")/" ./guix.scm

COMMITHASH=$(guix hash -x --serializer=nar /home/mbc/projects/conmanv4)
sed -i "s/[ ]*(base32 \"[a-z0-9]*\"))))/             (base32 \"$COMMITHASH\"))))/" ./guix.scm


cp ./guix.scm /home/mbc/projects/labsolns/labsolns/conmanv4.scm

cd /home/mbc/projects/labsolns
git add .
git commit -a -S -m "autocommit"
git push

guix pull
guix package -i conmanv4
