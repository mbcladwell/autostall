(define-module (labsolns seqeval)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages cran))

(define-public seqeval
(package
  (name "seqeval")
  (version "0.1")
(source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "git://github.com/mbcladwell/seqeval.git")
                      (commit "d7f09cd0a1f563ecf596c3e53168457bace475b4")))
                (sha256 (base32 "15cyk9ywc5sflxvadh00rzv412cb7zxrfam0a4p6zd3sysml2nyz"))
  		))

  (build-system gnu-build-system)
  (arguments `(	#:phases (modify-phases %standard-phases
					(add-after 'unpack 'patch-prefix
						   (lambda* (#:key inputs outputs #:allow-other-keys)
						     (substitute* '("./scripts/seqeval.sh"
								    "./app.R")
								  (("abcdefgh")
								   (assoc-ref outputs "out" )) )
						     #t))
					(add-before 'install 'copy-app
						    (lambda* (#:key outputs #:allow-other-keys)
						      (let* ((out  (assoc-ref outputs "out")))
							     (install-file "app.R" out)     			     	     
							     #t)))
					(add-before 'install 'copy-executable
						    (lambda* (#:key outputs #:allow-other-keys)
						      (let* ((out  (assoc-ref outputs "out"))
							     (bin-dir (string-append out "/bin"))
	    						    
							     )            				       
							(install-file "scripts/seqeval.sh" bin-dir)
							#t)))
				(add-before 'install 'copy-seqs
						    (lambda* (#:key outputs #:allow-other-keys)
						      (let* ((out  (assoc-ref outputs "out"))
							     (dummy(install-file "./reverse.txt" out))
							     )            				       
							(install-file "./forward.txt" out)
							#t)))
					(add-after 'install 'wrap-seqeval
						   (lambda* (#:key inputs outputs #:allow-other-keys)
						     (let* ((out (assoc-ref outputs "out"))
							    (bin-dir (string-append out "/bin"))					   
							    (dummy (chmod (string-append out "/bin/seqeval.sh") #o555 ))) ;;read execute, no write
						       (wrap-program (string-append out "/bin/seqeval.sh")
								     `( "PATH" ":" prefix  (,bin-dir) ))
						       #t)))					
	      ) ))
  (native-inputs
    `(("autoconf" ,autoconf)
      ("automake" ,automake)
      ("pkg-config" ,pkg-config)
      ("texinfo" ,texinfo)))
  (inputs `(("guile" ,guile-3.0)
	    
	    ))
  (propagated-inputs `(
		       ("r" ,r)
		       ("r-seqinr" ,r-seqinr)
	               ("r-shiny" ,r-shiny)	              
		       ))
  (synopsis "")
  (description "")
  (home-page "www.labsolns.com")
  (license license:gpl3+)))


