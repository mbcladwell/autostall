(let ((port (open-file "./out.scm" "a")))(format port "(define-module (artanis version) #:export (artanis-version)) (define artanis-version \"GNU Artanis-~a\")~%" "0.7"))
