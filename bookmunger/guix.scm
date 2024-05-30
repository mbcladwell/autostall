(define-module (bookmunger)
 
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
;;  #:use-module (dbi dbi)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((srfi srfi-1) #:select (alist-delete)))

(define-public bookmunger
             (let ((commit "3e7af8dfe0971d2a4e286a6c61def25c34cbb0b7")
        (revision "4"))
  (package
    (name "bookmunger")
    (version (string-append "0.1." (string-take commit 7)))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mbcladwell/bookmunger.git")
             (commit commit)))
              (file-name (git-file-name name version))
              (sha256
             (base32 "0l264qscqwj2fkqz50mq4vnkcny18bnlwp4gvrvg8ykc81kw3d90"))))
    (build-system guile-build-system)

    (arguments `(
	 ;;       #:modules
         ;;       ((ice-9 match)	
         ;; (ice-9 pretty-print) ,@%gnu-build-system-modules)
         
	       #:phases (modify-phases %standard-phases

				          (add-after 'unpack 'augment-GUILE_LOAD_PATH
				  (lambda* (#:key outputs #:allow-other-keys)
				    (let* ((out  (assoc-ref outputs "out"))
					   (scm  "/share/guile/site/3.0:"))
				      (setenv "GUILE_LOAD_PATH"
					      (string-append out scm
					        out scm "/bookmunger:"	 ;;needed for libraries				       
					     ;;  (assoc-ref inputs "artanis") scm
					     ;;  (assoc-ref inputs "guile-json") scm
					     ;;  (assoc-ref inputs "guile-redis") scm
					;;	(getenv "GUILE_LOAD_PATH")
						))
				      #t)))

				       
   )))


    
    (native-inputs
     `(("guile" ,guile-3.0)
       ))
      ;; (propagated-inputs
      ;;  `(("guile-dbi" ,guile-dbi)
      ;;  ))    
    (home-page "https://notabug.org/cwebber/guile-squee")
    (synopsis "Book library compatible with Urbit")
    (description
     "A simple command line book library suitable for storing books in your Urbit pier.")
    (license license:lgpl3+))))

bookmunger
