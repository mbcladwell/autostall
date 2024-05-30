(hall-description
  (name "labsolns")
  (prefix "")
  (version "0.1")
  (author "mbc")
  (copyright (2021))
  (synopsis "")
  (description "")
  (home-page "www.labsolns.com")
  (license gpl3+)
  (dependencies `())
  (skip ())
  (files (libraries
           ((directory
              "labsolns"
              ((scheme-file "lnpg")
               (scheme-file "artass")
               (scheme-file "gplot")))
            (scheme-file "labsolns")))
         (tests ((directory "tests" ())))
         (programs ((directory "scripts" ())))
         (documentation
           ((text-file "AUTHORS")
            (text-file "NEWS")
            (directory
              "doc"
              ((texi-file "version")
               (info-file "version")
               (info-file "labsolns")
               (text-file ".dirstamp")
               (text-file "stamp-vti")
               (texi-file "labsolns")))
            (text-file "COPYING")
            (text-file "HACKING")
            (symlink "README" "README.org")
            (org-file "README")))
         (infrastructure
           ((scheme-file "hall")
            (text-file ".gitignore")
            (scheme-file "guix")))))
