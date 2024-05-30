(hall-description
  (name "lnpg")
  (prefix "")
  (version "0.1")
  (author "demo")
  (copyright (2021))
  (synopsis "")
  (description "")
  (home-page "www.labsolns.com")
  (license gpl3+)
  (dependencies `())
  (skip ())
  (files (libraries
           ((directory "lnpg" ()) (scheme-file "lnpg")))
         (tests ((directory "tests" ())))
         (programs
           ((directory
              "scripts"
              ((shell-file "guix-install-mod")
               (configuration-file "pg_hba")
               (unknown-type "example-data.sql")
               (unknown-type "drop-func-tables.sql")
               (shell-file "install-lnpg")
               (shell-file "prep-for-lnpg")
               (configuration-file "postgresql")
               (unknown-type "initdba.sql")
               (configuration-file "pg_ident")
               (unknown-type "initdbb.sql")
               (unknown-type "create-db.sql")))))
         (documentation
           ((directory "doc" ((texi-file "lnpg")))
            (text-file "COPYING")
            (text-file "HACKING")
            (symlink "README" "README.org")
            (org-file "README")))
         (infrastructure
           ((scheme-file "hall")
            (text-file ".gitignore")
            (scheme-file "guix")))))
