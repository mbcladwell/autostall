#! /bin/bash

cd /home/mbc/projects/babweb
git add .
git commit -a -S -m "autocommit"
git push
     
COMMITID=$(git log -1 --pretty=format:"%H")
cd /home/mbc/projects/autostall/babweb

sed -i "s/[ ]*(let ((commit \"[a-z0-9]*\")/             (let ((commit \"$COMMITID\")/" ./guix.scm
sed -i "s/[ ]*(let ((commit \"[a-z0-9]*\")/             (let ((commit \"$COMMITID\")/" ./babweb.scm

COMMITHASH=$(guix hash -x --serializer=nar /home/mbc/projects/babweb)
sed -i "s/[ ]*(base32 \"[a-z0-9]*\"))))/             (base32 \"$COMMITHASH\"))))/" ./guix.scm
sed -i "s/[ ]*(base32 \"[a-z0-9]*\"))))/             (base32 \"$COMMITHASH\"))))/" ./babweb.scm


##guix package --install-from-file=guix.scm

cp ./babweb.scm ~/projects/labsolns/labsolns/babweb.scm

cd /home/mbc/projects/labsolns
git add .
git commit -m "autocommit"
git push

cd /home/mbc/projects/autostall/babweb
