#! /bin/bash

cd /home/mbc/projects/myapp3
git add .
git commit -a -S -m "autocommit"
git push
     
COMMITID=$(git log -1 --pretty=format:"%H")
cd /home/mbc/syncd/tobedeleted/myapp3

sed -i "s/[ ]*(let ((commit \"[a-z0-9]*\")/             (let ((commit \"$COMMITID\")/" ./guix.scm

COMMITHASH=$(guix hash -x --serializer=nar /home/mbc/projects/myapp3)
sed -i "s/[ ]*(base32 \"[a-z0-9]*\"))))/             (base32 \"$COMMITHASH\"))))/" ./guix.scm

export GUIX_PROFILE="/gnu/store/46333pg3nqikkwa4qwf40szxr540dnhm-guile-dbi-2.1.8/share/guile/site/2.2${GUIX_PROFILE:+:}$GUIX_PROFILE"

guix package --manifest=manifest.scm --install-from-file=guix.scm
