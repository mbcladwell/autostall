(use-modules
  (guix packages)
  ((guix licenses) #:prefix license:)
  (guix download)
  (guix build-system gnu)
  (gnu packages)
  (gnu packages readline)
  (gnu packages autotools)
  (gnu packages guile)
  (gnu packages guile-xyz)
  (gnu packages pkg-config)
  (gnu packages texinfo))

(package
  (name "postgresql-client")
  (version "13.4")
  (source "./postgresql-13_13.4.orig.tar.bz2")
  (build-system gnu-build-system)
  (arguments
    `())
  (native-inputs
    `(("autoconf" ,autoconf)
      ("automake" ,automake)
      ("pkg-config" ,pkg-config)
      ("texinfo" ,texinfo)
      ))
  (inputs `(("zlib" ,(@ (gnu packages compression) zlib))))
  (propagated-inputs
    `(("readline" ,readline)))
  (synopsis "Postgresql client")
  (description
    "")
  (home-page
    "https://packages.debian.org/bullseye/postgresql-client-13")
  (license license:gpl3+))

