
(define-module (labsolns labsolns))

(use-modules
;;  (srfi srfi-19)   ;; date time
 (labsolns lnpg)
 (ice-9 pretty-print))

(define (main args)
  ;;args:  ip port username password dbname method
  ;;first item of args list is the file name so skip
  ;;method: 'init' or 'refresh'  
  (init-db args))
	 

