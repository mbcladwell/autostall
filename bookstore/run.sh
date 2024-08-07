#! /bin/bash

cd /home/mbc/projects/bookstore
git add .
git commit -a -S -m "autocommit"
git push
     
COMMITID=$(git log -1 --pretty=format:"%H")
cd /home/mbc/syncd/tobedeleted/bookstore

sed -i "s/[ ]*(let ((commit \"[a-z0-9]*\")/             (let ((commit \"$COMMITID\")/" ./guix.scm

COMMITHASH=$(guix hash -x --serializer=nar /home/mbc/projects/bookmunger)
sed -i "s/[ ]*(base32 \"[a-z0-9]*\"))))/             (base32 \"$COMMITHASH\"))))/" ./guix.scm


cp ./guix.scm /home/mbc/projects/labsolns/labsolns/bookstore.scm

cd /home/mbc/projects/labsolns
git add .
git commit -a -S -m "autocommit"
git push


guix pull
guix package -i bookstore
