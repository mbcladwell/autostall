#! /bin/bash

cd /home/mbc/projects/guile-ares-rs
git add .
git commit -a -S -m "autocommit"
git push
     
COMMITID=$(git log -1 --pretty=format:"%H")
cd /home/mbc/projects/autostall/guile-ares-rs

sed -i "s/[ ]*(let ((commit \"[a-z0-9]*\")/             (let ((commit \"$COMMITID\")/" ./guix.scm

COMMITHASH=$(guix hash -x --serializer=nar /home/mbc/projects/guile-ares-rs)
sed -i "s/[ ]*(base32 \"[a-z0-9]*\"))))/             (base32 \"$COMMITHASH\"))))/" ./guix.scm

guix package -L . --install-from-file=guix.scm
