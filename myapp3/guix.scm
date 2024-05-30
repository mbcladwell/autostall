(define-module (myapp3)
 #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system guile)
  #:use-module (gnu packages guile)
  #:use-module (guix utils)
  #:use-module ((guix build utils) #:select (alist-replace))
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (dbi dbi)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((srfi srfi-1) #:select (alist-delete)))

(define-public myapp3
             (let ((commit "7c74609013da613eddeb646dc5ab06d1cc0d2b96")
        (revision "4"))

  (package
    (name "myapp3")
    (version (string-append "0.1." (string-take commit 7)))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mbcladwell/myapp3.git")
		    (commit commit)
		    (file-name (git-file-name name version))
		    (sha256
		             (base32 "1c8bqbc96wh9yglizyk3zb230j7ijxwinkbacndyp358p9yn00wa"))))))
    (build-system guile-build-system)

    (inputs
     `(("guile" ,guile-3.0)
       ("guile-dbi" ,guile-dbi)
       ))
    (native-inputs
     `())
     (propagated-inputs
    `(
;;     ("dbd-postgresql" ,guile-dbd-mysql)
  		))
  
    (home-page "https://notabug.org/cwebber/guile-squee")
    (synopsis "Connect to PostgreSQL using Guile")
    (description
     "@code{squee} is a Guile library for connecting to PostgreSQL databases
using Guile's foreign function interface.")
    (license license:lgpl3+))))

myapp3
