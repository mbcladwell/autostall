#!/bin/sh


mkdir lndata

echo "export PGDATA=\"/home/admin/lndata\"" >> /home/admin/.bashrc
export PGDATA="/home/admin/lndata"


initdb -D /home/admin/lndata

cp /home/admin/lnpg/scripts/pg_hba.conf /home/admin/lndata
cp /home/admin/lnpg/scripts/pg_ident.conf /home/admin/lndata
cp /home/admin/lnpg/scripts/postgresql.conf /home/admin/lndata

pg_ctl -D /home/admin/lndata -l logfile start

psql -U admin -h 127.0.0.1 postgres -a -f /home/admin/lnpg/scripts/initdba.sql
psql -U admin -h 127.0.0.1 lndb -a -f /home/admin/lnpg/scripts/initdbb.sql
psql -U ln_admin -h 127.0.0.1 -d lndb -a -f /home/admin/lnpg/scripts/create-db.sql
psql -U ln_admin -h 127.0.0.1 -d lndb -a -f /home/admin/lnpg/scripts/example-data.sql

echo "LIMS*Nucleus database successfully installed."
echo "'pg_ctl -D /home/admin/lndata -l logfile start' to restart the database server after instance reboot."
