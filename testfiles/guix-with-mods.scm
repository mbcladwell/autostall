(use-modules
  (guix packages)
  ((guix licenses) #:prefix license:)
  (guix download)
  (guix build-system gnu)
  (guix build utils)
  (gnu packages)
  (gnu packages autotools)
  (gnu packages guile)
  (gnu packages guile-xyz)
  (gnu packages pkg-config)
  (guix gexp)
  (gnu packages texinfo))

(package
  (name "labsolns")
  (version "0.2")
 
 (source (origin
              (method url-fetch)
              (uri "file:./labsolns-0.1.tar.gz")
              (sha256
               (base32
                "1vk1kp2xhz35xa5n27cxlq9c88wk6qm7fqaac8rb0pb6k9pvsv7v"))
              ;; (modules '((guix build utils)))
	      ;; (snippet
	      ;; ;; (with-imported-modules '((guix build utils)) 
	      ;;  '(begin                 
	      ;; 	  (substitute* '("labsolns/lnpg.scm")
	      ;; 		       (("abcdefgh") %store-directory  ))			
	      ;; 	  #t))
	      ))

  
  (build-system gnu-build-system)
  (arguments `(#:tests? #false ; there are none
	       #:phases (modify-phases %standard-phases
       		       (add-after 'unpack 'patch-prefix
			       (lambda* (#:key inputs outputs #:allow-other-keys)
					(substitute* "labsolns/lnpg.scm"
						(("abcdefgh")
						(assoc-ref outputs "out" )) )
					#t))
		       (add-after 'unpack 'mod-guix-load-path
			       (lambda* (#:key inputs outputs #:allow-other-keys)
				 (begin
				   (system* "echo" "export" (string-append  "GUILE_LOAD_PATH=" (assoc-ref outputs "out" ) "${GUILE_LOAD_PATH:+:}$GUILE_LOAD_PATH >> $HOME/.bashrc"))
				    ;;currently GUILE_LOAD_PATH is empty so no ":" (getenv GUILE_LOAD_PATH) to append to
				   (setenv "GUILE_LOAD_PATH" (string-append (assoc-ref outputs "out" )  ))
				   (setenv "GUIX_PROFILE" "$HOME/.guix-profile/etc/profile")
				   )
					#t))
		       
		       (add-before 'install 'make-dir
			       (lambda* (#:key outputs #:allow-other-keys)
				    (let* ((out  (assoc-ref outputs "out"))
					   (scripts-dir (string-append out "/scripts"))
					   (dummy (mkdir-p scripts-dir))
					   )            				       
				       (copy-recursively "./scripts" scripts-dir)
				       #t))))
	       ))
  (native-inputs
    `(("autoconf" ,autoconf)
      ("automake" ,automake)
      ("pkg-config" ,pkg-config)
      ("texinfo" ,texinfo)
      ))
  (inputs `(("guile" ,guile-3.0)))
  (propagated-inputs `())
  (synopsis "")
  (description "")
  (home-page "www.labsolns.com")
  (license license:gpl3+))


