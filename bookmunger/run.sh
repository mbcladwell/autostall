#! /bin/bash

cd /home/mbc/projects/bookmunger
git add .
git commit -a -S -m "autocommit"
git push
     
COMMITID=$(git log -1 --pretty=format:"%H")
cd /home/mbc/syncd/tobedeleted/bookmunger

sed -i "s/[ ]*(let ((commit \"[a-z0-9]*\")/             (let ((commit \"$COMMITID\")/" ./guix.scm

COMMITHASH=$(guix hash -x --serializer=nar /home/mbc/projects/bookmunger)
sed -i "s/[ ]*(base32 \"[a-z0-9]*\"))))/             (base32 \"$COMMITHASH\"))))/" ./guix.scm

guix package -L /gnu/store/3f0lv3m4vlzqc86750025arbskfrq05p-guile-dbi-2.1.8/lib --install-from-file=guix.scm
