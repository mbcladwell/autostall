(define-module (labsolns ebbot)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages guile)
  #:use-module (guix build-system guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages texinfo)
  #:use-module (labsolns guile-oauth)
;;  #:use-module (labsolns artanis-07)
  #:use-module (json)
  #:use-module (gnutls)
  #:use-module (gnu packages tls)

  )



(define-public artanis-07
             (let ((commit "bbc8d311e80a876b983c2cc834f0b56a3d7a6d68")
        (revision "4"))
  (package
    (name "artanis")
   (version (string-append "0.7." (string-take commit 7)))
    (source (origin
	     (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/mbcladwell/artanis")
                   (commit commit)))
             (file-name (git-file-name name version))
              (sha256
               (base32 "0jd0jh0jz2vfmxdzcpnmqa38lw9m0l3dyz1ynaw9d96xb5bzc1va"))
              (modules '((guix build utils)))

	      ))
    (build-system guile-build-system)
    (inputs
     (list bash-minimal guile-3.0 nspr nss))
    ;; FIXME the bundled csv contains one more exported procedure
    ;; (sxml->csv-string) than guile-csv. The author is maintainer of both
    ;; projects.
    ;; TODO: Add guile-dbi and guile-dbd optional dependencies.
    (propagated-inputs
     (list guile-json-4 guile-curl guile-readline))
    (native-inputs
     (list bash-minimal                           ;for the `source' builtin
       pkg-config
      guile-3.0                                   ;;required for guile-build-system see docs
           util-linux))                           ;for the `script' command
    (arguments
     `(
       ;; #:modules ((guix build guile-build-system)
       ;; 		  (guix build gnu-build-system)
       ;; ;;           ;;  #:select (target-guile-effective-version)
		   
       ;; 		   ;;            ,@%default-gnu-modules
       ;; 		   )
       ;; 		 #:imported-modules ((guix build guile-build-system)
       ;; 				     (guix build gnu-build-system)
       ;; 			    ;;                     ,@%default-gnu-imported-modules
       ;; 			    )
     
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-site-dir
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "artanis/commands/help.scm"
               (("\\(%site-dir\\)")
                (string-append "\""
                               (assoc-ref outputs "out")
                               "/share/guile/site/3.0/"
                               "\"")))))
	 (add-after 'patch-site-dir 'modify-executable
	   (lambda* (#:key inputs outputs #:allow-other-keys)
	     (let ((out  (assoc-ref outputs "out")))					  
	       (substitute* '("./bin/art.in")
		 (("guileexecutable")
		  (string-append (assoc-ref inputs "guile") "/bin/guile"))) )
				    #t))		    	 	 
         (add-after 'modify-executable 'patch-reference-to-libnss
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "artanis/security/nss.scm"
               (("ffi-binding \"libnss3\"")
                (string-append
                 "ffi-binding \""
                 (assoc-ref inputs "nss") "/lib/nss/libnss3.so"
                 "\""))
               (("ffi-binding \"libssl3\"")
                (string-append
                 "ffi-binding \"" (assoc-ref inputs "nss") "/lib/nss/libssl3.so\"")))))
         (add-after 'patch-reference-to-libnss 'substitute-root-dir
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
		   (bin-dir (string-append out "/bin"))
		   (_ (mkdir-p bin-dir))
                   )
	       (copy-recursively "./bin" bin-dir))))
         (add-after 'substitute-root-dir 'wrap-art
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (scm (string-append out "/share/guile/site/3.0/"))
                    (go (string-append out "/lib/guile/3.0/site-ccache")))
               (wrap-program (string-append bin "/art.in")
                 `("GUILE_LOAD_PATH" ":" prefix
                   (,scm ,(getenv "GUILE_LOAD_PATH")))
                 `("GUILE_LOAD_COMPILED_PATH" ":" prefix
                   (,go ,(getenv "GUILE_LOAD_COMPILED_PATH"))))))))))
    (synopsis "Web application framework written in Guile")
    (description "GNU Artanis is a web application framework written in Guile
Scheme.  A web application framework (WAF) is a software framework that is
designed to support the development of dynamic websites, web applications, web
services and web resources.  The framework aims to alleviate the overhead
associated with common activities performed in web development.  Artanis
provides several tools for web development: database access, templating
frameworks, session management, URL-remapping for RESTful, page caching, and
more.")
    (home-page "https://www.gnu.org/software/artanis/")
    (license (list license:gpl3+ license:lgpl3+))))) ;dual license


(define-public ebbot
             (let ((commit "bbc8d311e80a876b983c2cc834f0b56a3d7a6d68")
        (revision "4"))
(package
  (name "ebbot")
  (version (string-append "0.1." (string-take commit 7)))
  (source (origin
           (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/mbcladwell/ebbot")
                      (commit commit)))
                        (file-name (git-file-name name version))
                (sha256 
             (base32 "0sg9zfbqbx0bnzly0x23kc87nsxzyll0vgvmzwqkqnrbds8m9px7"))))
  (build-system guile-build-system)
  (arguments `(
	     ;;  #:modules (((guix build guile-build-system)
		;;	   #:select (target-guile-effective-version))
		;;	  ,@%gnu-build-system-modules)
		;;	 #:imported-modules ((guix build guile-build-system)
		;;			     ,@%gnu-build-system-modules)
	     ;;  #:tests? #false ; there are none
			#:phases (modify-phases %standard-phases
    		       (add-after 'unpack 'patch-prefix
				  (lambda* (#:key inputs outputs #:allow-other-keys)
				    (let ((out  (assoc-ref outputs "out")))					  
				      (substitute* '("scripts/ebbot.sh" "scripts/format.sh" "scripts/init-acct.sh" "scripts/masttoot.sh" "scripts/tweet.sh")
					(("ebbotstorepath")
					 out))
				      (substitute* '("scripts/ebbot.sh" "scripts/format.sh" "scripts/init-acct.sh" "scripts/masttoot.sh" "scripts/tweet.sh")
					(("guileloadpath")
					 (string-append  out "/share/guile/site/3.0:"
							 (assoc-ref inputs "guile")  "/share/guile/site/3.0:"
							 (assoc-ref inputs "guile-json")  "/share/guile/site/3.0:"
							 (assoc-ref inputs "guile-oauth")  "/share/guile/site/3.0:"
							 (getenv "GUILE_LOAD_PATH") "\"")))
				      (substitute* '("scripts/ebbot.sh" "scripts/format.sh" "scripts/init-acct.sh" "scripts/masttoot.sh" "scripts/tweet.sh")
					(("guileexecutable")
					 (string-append (assoc-ref inputs "guile") "/bin/guile")))
				      
				      (substitute* '("scripts/ebbot.sh" "scripts/format.sh" "scripts/init-acct.sh" "scripts/masttoot.sh" "scripts/tweet.sh")
					(("guileloadcompiledpath")
					 (string-append  out "/lib/guile/3.0/site-ccache:"
							 (assoc-ref inputs "guile")  "/lib/guile/3.0/site-ccache:"
							 (assoc-ref inputs "guile-json")  "/lib/guile/3.0/site-ccache:"
							 (assoc-ref inputs "guile-oauth")  "/lib/guile/3.0/site-ccache:"
							 (getenv "GUILE_LOAD_COMPILED_PATH") "\""))))
				    #t))		    
		       (add-after 'patch-prefix 'make-dir
			 (lambda* (#:key outputs #:allow-other-keys)
			   (let* ((out  (assoc-ref outputs "out"))
				  (ebbot-dir (string-append out "/share/guile/site/3.0/ebbot"))
				  (mkdir-p ebbot-dir)
				  (dummy (copy-recursively "./ebbot" ebbot-dir))) 
			     #t)))
		       
			   (add-after 'make-dir 'make-bin-dir
				  (lambda* (#:key inputs outputs #:allow-other-keys)
				    (let* ((out (assoc-ref outputs "out"))
					   (bin-dir (string-append out "/bin"))
					   (scm  "/share/guile/site/3.0")
					   (go   "/lib/guile/3.0/site-ccache")
					   (all-files '("ebbot.sh" "format.sh" "init-acct.sh" "masttoot.sh" "tweet.sh")))				      
				      (map (lambda (file)
					     (begin
					       (install-file (string-append "./scripts/" file) bin-dir)
					       (chmod (string-append bin-dir "/" file) #o555 ) ;;read execute, no write
					       (wrap-program (string-append bin-dir "/" file)
							     `( "PATH" ":" prefix  (,bin-dir) )							     
							     `("GUILE_LOAD_PATH" prefix
							       (,(string-append out scm)
								))
							     `("GUILE_LOAD_COMPILED_PATH" prefix
							       (,(string-append out go)))
							     )))
					     all-files))					   					   	    
				      #t))

		       )))
  (native-inputs
    `(("guile" ,guile-3.0)))
  ;; (propagated-inputs `( ("guile-json" ,guile-json-4) ("guile-oauth" ,guile-oauth)("bash" ,bash)
  ;; 			("gnutls" ,gnutls)("guile-gnutls" ,guile-gnutls)("artanis" ,artanis)
  ;; 			))
  (propagated-inputs (list guile-json-4 guile-oauth bash gnutls guile-gnutls artanis-07)
			)
  (synopsis "Auto tweeter for educational tweets concerning propaganda")
  (description "Auto tweeter for educational tweets concerning propaganda")
  (home-page "www.build-a-bot.biz")
  (license license:gpl3+))))

