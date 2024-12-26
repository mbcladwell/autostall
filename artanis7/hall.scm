(hall-description
  (name "artanis")
  (prefix "")
  (version "0.5.3")
  (author "mbc")
  (copyright (2022))
  (synopsis "")
  (description "")
  (home-page "www.labsolns.com")
  (license gpl3+)
  (dependencies `())
  (skip ())
  (files (libraries
           ((directory
              "artanis"
              ((directory
                 "sql-mapping"
                 ((scheme-file "built-in")
                  (scheme-file "mapping")
                  (scheme-file "fetcher")
                  (scheme-file "handlers")))
               (directory
                 "websocket"
                 ((scheme-file "named-pipe")
                  (scheme-file "protocols")
                  (scheme-file "handshake")
                  (scheme-file "frame")))
               (directory
                 "tpl"
                 ((scheme-file "lexer")
                  (scheme-file "parser")
                  (scheme-file "sxml")
                  (scheme-file "utils")))
               (directory
                 "commands"
                 ((scheme-file "migrate")
                  (scheme-file "help")
                  (scheme-file "api")
                  (scheme-file "version")
                  (scheme-file "create")
                  (scheme-file "work")
                  (scheme-file "draw")))
               (directory "webapi" ((scheme-file "restful")))
               (directory "security" ((scheme-file "nss")))
               (directory
                 "server"
                 ((scheme-file "http")
                  (scheme-file "aio")
                  (scheme-file "epoll")
                  (scheme-file "proxy")
                  (scheme-file "ragnarok")
                  (scheme-file "server-context")
                  (scheme-file "scheduler")))
               (directory
                 "mvc"
                 ((scheme-file "route")
                  (scheme-file "controller")
                  (scheme-file "model")
                  (scheme-file "migration")
                  (scheme-file "view")))
               (directory
                 "third-party"
                 ((directory
                    "redis"
                    ((directory
                       "upstream"
                       ((scheme-file "commands")
                        (scheme-file "main")
                        (scheme-file "connection")
                        (scheme-file "utils")))))
                  (directory
                    "json"
                    ((directory
                       "upstream"
                       ((text-file "AUTHORS")
                        (text-file "COPYING")
                        (scheme-file "builder")
                        (scheme-file "parser")
                        (unknown-type "COPYING.LESSER")
                        (text-file "README")
                        (scheme-file "record")
                        (org-file "README")))))
                  (scheme-file "csv")
                  (scheme-file "json")
                  (scheme-file "redis")))
               (scheme-file "page")
               (scheme-file "cache")
               (scheme-file "oht")
               (scheme-file "tpl")
               (scheme-file "sendmail")
               (scheme-file "cookie")
               (scheme-file "commands")
               (scheme-file "irregex")
               (scheme-file "lpc")
               (scheme-file "env")
               (scheme-file "db")
               (scheme-file "route")
               (scheme-file "config")
               (scheme-file "sql-mapping")
               (scheme-file "server")
               (scheme-file "fprm")
               (scheme-file "mime")
               (scheme-file "debug")
               (scheme-file "utils")
               (scheme-file "ffi")
               (scheme-file "ssql")
               (scheme-file "inotify")
               (scheme-file "upload")
               (scheme-file "session")
               (scheme-file "websocket")))
            (scheme-file "artanis")))
         (tests ((directory "tests" ())))
         (programs ((directory "scripts" ())))
         (documentation
           ((text-file "NEWS")
            (directory "doc" ((texi-file "artanis")))
            (text-file "COPYING")
            (text-file "HACKING")
            (symlink "README" "README.md")
            (org-file "README")))
         (infrastructure
           ((scheme-file "hall")
            (text-file ".gitignore")
            (scheme-file "guix")))))
