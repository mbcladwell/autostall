#! /bin/bash

cp ./artanis-07.scm /home/mbc/projects/labsolns/labsolns/

cd /home/mbc/projects/labsolns
git add .
git commit -a -S -m "autocommit"
git push

cd /home/mbc
guix pull
guix package -i artanis

