
(define-module (labsolns lnpg)
  #:export (
	    init-db
	    ))

(use-modules
	     (srfi srfi-1)  ;;list searching   
	     (ice-9 rdelim)
	     (ice-9 i18n)   ;; internationalization
	     (ice-9 popen)
	     (ice-9 regex) ;;list-matches
	     (ice-9 string-fun)  ;;string-replace-substring
	     (ice-9 pretty-print))


(define lnpg-store-dir "abcdefgh")

(define (get-pg-superuser-pgdb user pw ip port dbname)
  ;; needed to create users and the database
  ;; need: psql postgresql://admin@127.0.0.1:5432/postgres  -f /gnu/store/bahv45rxhv8d8mvr2rr3md2mx7gq899p-labsolns-0.1/scripts/initdba.sql
  (string-append "postgresql://admin@" ip ":" port "/postgres"  ))

(define (get-pg-superuser-lndb user pw ip port)
  ;; needed to create schema
  (string-append "postgresql://admin@" ip ":" port "/lndb" ))

(define (get-ln-superuser-cstring ip port dbname )
  ;;used to create  tables, functions, extensions, search path
(string-append "postgresql://ln_admin:welcome@" ip ":" port "/" dbname ))


(define (create-users-db pg-pgdb)
  ;; first step in db install
  (begin
   (pretty-print (string-append "psql " pg-pgdb " -f " lnpg-store-dir "/scripts/initdba.sql")) 
  (system (string-append "psql " pg-pgdb " -f " lnpg-store-dir "/scripts/initdba.sql"))))

(define (create-schema-search-path-roles pg-lndb)
  ;; second step in db install
  (system (string-append "psql " pg-lndb " -f " lnpg-store-dir "/scripts/initdbb.sql")))


(define (clean-db ln-cs)  (system (string-append "psql " ln-cs " -f " lnpg-store-dir "/scripts/drop-func-tables.sql") ))
(define (create-db ln-cs)  (system (string-append "psql " ln-cs " -f " lnpg-store-dir "/scripts/create-db.sql") ))
(define (load-eg-data ln-cs)  (system (string-append "psql " ln-cs " -f " lnpg-store-dir "/scripts/example-data.sql") ))


(define (init-fresh-local pg-pgdb pg-lndb)
  ;;used for local installation
  ;;config files used to provide unrestricted access to ln_admin
  (begin (system "mkdir $HOME/lndata" )
	 (system "echo \"export PGDATA=\\\"$HOME/lndata\\\"\" >> $HOME/.bashrc")
	 (system "export PGDATA=$HOME/lndata")
	 (system "initdb -D $HOME/lndata")
	 (system (string-append "cp " lnpg-store-dir "/scripts/pg_hba.conf $HOME/lndata"))
	 (system (string-append "cp " lnpg-store-dir "/scripts/pg_ident.conf $HOME/lndata"))
	 (system (string-append "cp " lnpg-store-dir "/scripts/postgresql.conf $HOME/lndata"))
	 (system "pg_ctl -D $HOME/lndata -l logfile start")
	 (create-users-db pg-pgdb)
	 (create-schema-search-path-roles pg-lndb)
	 ))


(define (refresh ln-cs)
  (begin
    (clean-db ln-cs)
    (create-db ln-cs)
    (load-eg-data ln-cs)))


(define (full-local-install pg-pgdb pg-lndb ln-cs)
  (begin
    (init-fresh-local pg-pgdb pg-lndb)
    (create-db ln-cs)
    (load-eg-data ln-cs)))

(define (init-db args)
  (let* (
	 (ip (cadr args))
	 (port (caddr args))
	 (user (cadddr args))
	 (pw (car (cddddr args)))
	 (dbname (cadr (cddddr args)))
	 (method (caddr (cddddr args)))
	 (pg-pgdb (get-pg-superuser-pgdb user pw ip port dbname))
	 (pg-lndb (get-pg-superuser-lndb user pw ip port))
	 (ln-cs (get-ln-superuser-cstring ip port dbname ))
	 )
    (if (equal? method "init" ) ;; expect init or refresh
	(full-local-install pg-pgdb pg-lndb ln-cs)
	(refresh ln-cs))))

		        
;; /gnu/store/bahv45rxhv8d8mvr2rr3md2mx7gq899p-labsolns-0.1/scripts/initdba.sql

;; psql postgresql://admin@127.0.0.1:5432/postgres  -f /gnu/store/bahv45rxhv8d8mvr2rr3md2mx7gq899p-labsolns-0.1/scripts/initdba.sql
