#! /bin/bash

cp ./ebbot.scm ~/projects/labsolns/labsolns/ebbot.scm

cd /home/mbc/projects/labsolns
git add .
git commit -m "autocommit"
git push

cd /home/mbc
guix pull
guix package -i ebbot
