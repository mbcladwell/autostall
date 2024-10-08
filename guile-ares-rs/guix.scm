(define-module (labsolns artanis-07)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils)
  #:use-module (gnu packages guile)
  #:use-module (guix build-system guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages pkg-config)
;;   #:use-module (json)
;;   #:use-module (gnu packages curl)
;;   #:use-module (gnu packages linux)
;; ;;   #:use-module (ice-9 readline)
;;    #:use-module (gnu packages nss) ;;;;;;;;;;
;;   #:use-module (gnu packages)
;;   #:use-module (gnu packages base)
;;   #:use-module (gnu packages bash)
;;   #:use-module (gnu packages compression)
;;   #:use-module (gnu packages crypto)
;;   #:use-module (gnu packages disk)
;;   #:use-module (gnu packages gettext)
;;   #:use-module (gnu packages gl)
;;   #:use-module (gnu packages glib)
;;   #:use-module (gnu packages gnupg)
  #:use-module (gnu packages guile)
  ;; #:use-module (gnu packages hurd)
  ;; #:use-module (gnu packages libffi)
  ;; #:use-module (gnu packages libunistring)
  ;; #:use-module (gnu packages mes)
  ;; #:use-module (gnu packages networking)
  ;; #:use-module (gnu packages noweb)
  ;;  #:use-module (gnu packages package-management)
  ;; #:use-module (gnu packages password-utils)
  ;; #:use-module (gnu packages readline)
  ;; #:use-module (gnu packages tls)
  ;;  #:use-module (gnu packages version-control)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix modules)
  #:use-module (guix monads)
   #:use-module (guix i18n)
  #:use-module (guix records)
  #:use-module (guix search-paths)
   #:use-module (guix derivations)
  #:use-module (guix store)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix utils)
  #:autoload   (srfi srfi-98) (get-environment-variables)
  #:use-module (guix build utils)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:use-module (guix build-system guile)
  #:use-module (gnu packages search)
  #:use-module (gnu packages serialization)
  ;; #:use-module (gnu packages slang)
  ;; #:use-module (gnu packages swig)
  ;; #:use-module (gnu packages webkit)
  ;; #:use-module (gnu packages xorg)
  ;; #:use-module ((srfi srfi-1) #:select (alist-delete))

  )


(define-public guile-ares-rs
             (let ((commit "530505954306bc586f2d85040fba6315b2087776")
	(revision "4"))
    (package
     (name "guile-ares-rs")
     (version (string-append "0.95." (string-take commit 7)))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mbcladwell/guile-ares-rs")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1c15n8sgch98ds06f8p5cvsv63p8wisxmryhafq1awv1x1qw96nn"))))
    (build-system guile-build-system)
    (arguments
     (list
      #:source-directory "src/guile"))
    ;; Remove guile-next dependency, when guile package get custom text port
    (inputs `(("guile" ,guile-next)))
    (propagated-inputs (list guile-fibers))
    (home-page "https://git.sr.ht/~abcdw/guile-ares-rs")
    (synopsis "Asyncronous Reliable Extensible Sleek RPC Server for Guile")
    (license (list license:gpl3+ license:lgpl3+))
    (description "Asynchronous Reliable Extensible Sleek RPC Server for
 Guile.  It's based on nREPL protocol and can be used for programmable
 interactions with a running guile processes, for implementing REPLs, IDEs,
 test runners or other tools."))))

    guile-ares-rs
    
