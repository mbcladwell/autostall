see: https://www.stihie.net/labsolns/artforguix/

The follow files are modified:

commands/work.scm
tpl/parser.scm
mvc/controller.scm  
webapi/restful.scm  

utils.scm
env.scm


|File |	line |	Modified source code |
|utils.scm 	279     299	(if (immutable-toplevel)
	       280 	300     (format #f “~a/pub/~a” (immutable-toplevel) path)
	       833 	853     (let* ((toplevel (immutable-toplevel))
	       1309 	1329    (immutable-toplevel) file)))

tpl/parser.scm 	40 	43      (format #f “~a/~a/~a” (immutable-toplevel) pub args))))
	        52 	63      (mfile (format #f “~a/~a/manifest.json” (immutable-toplevel) path))

commands/work.scm
                77 	(let ((entry (string-append (immutable-toplevel) “/” *artanis-entry*)))
	       126 	(add-to-load-path (immutable-toplevel))

mvc/controller.scm
                45 	(immutable-toplevel) ’name method)))
         	62 	(define toplevel (immutable-toplevel))

webapi/restful.scm
                58 	(load (format #f “~a/app/api/~a.scm” (immutable-toplevel) s)))

env.scm        44      add: %immutable-toplevel
               45      add: immutable-toplevel
               135    find:  (define (current-toplevel)
                               (or (%current-toplevel)
                                    (find-ENTRY-path identity #t)))

and replace with:

(define %immutable-toplevel (make-parameter #f))

(define (immutable-toplevel)
    (or (%immutable-toplevel)
  (find-ENTRY-path identity #t)))
  
  (define (current-toplevel)
    (string-append \"/tmp/\" (and=> (string-match \".+/(.+)$\" (getcwd)) (lambda (m) (match:substring m 1)))))
