(define-module (port)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
    #:use-module (guix build-system gnu)

  #:use-module (guix build-system node)
  #:use-module (guix utils)
   #:use-module (gnu packages adns)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages node-xyz)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (ice-9 match)
#:use-module (srfi srfi-26)
#:use-module (gnu packages node)

  )

(define-public port
             (let ((commit "08ee5bb831ad1bd363e35f6d32ae1b2d61cfbde9")
        (revision "1"))
(package
  (name "port")
  (version (string-append "1.9.1" (string-take commit 7)))
  (source (origin
           (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/urbit/port.git")
                      (commit commit)))
                        (file-name (git-file-name name version))
                (sha256 
             (base32 "0c4ziya8srz4a95barr4ah9g5f39cyhnw9mhbyvslz4vgw5yjflq"))))
  (build-system node-build-system)
 
  (native-inputs
    `(("esbuild" ,esbuild)
       ("node" ,node-bootstrap) ))
  (propagated-inputs `( ))
  (synopsis "Auto tweeter for educational tweets concerning propaganda")
  (description "Auto tweeter for educational tweets concerning propaganda")
  (home-page "www.build-a-bot.biz")
  (license license:gpl3+))))

port

