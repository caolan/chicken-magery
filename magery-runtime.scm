(module magery-runtime

;; exports
(templates
 lookup
 stringify
 html-escape)

(import chicken scheme)
(use srfi-69 data-structures irregex)

(define templates
  (make-parameter (make-hash-table
                   test: eq?
                   hash: symbol-hash)))

(define (lookup path data)
  (cond
   ((not data) #f)
   ((null? path) data)
   (else
    (lookup (cdr path)
            (alist-ref (car path) data eq? #f))))) 

(define (stringify x)
  (->string x))

(define (html-escape str)
  (irregex-replace/all
   "[<>\"'&]"
   str
   (lambda (m)
     (case (string-ref (irregex-match-substring m) 0)
       ((#\<) "&lt;")
       ((#\>) "&gt;")
       ((#\&) "&ampt;")
       ((#\") "&quot;")
       ((#\') "&apos;")))))

)
