(module magery-runtime

;; exports
(templates
 lookup
 stringify
 html-escape)

(import chicken scheme)
(use srfi-69 data-structures irregex srfi-13)

(define templates
  (make-parameter (make-hash-table
                   test: eq?
                   hash: symbol-hash)))

(define (lookup path data)
  (cond
   ((null? path) data)
   ((eq? data 'undefined)
    'undefined)
   ((vector? data)
    (if (equal? path '(length))
        (vector-length data)
        'undefined))
   (else
    (lookup (cdr path)
            (alist-ref (car path) data eq? 'undefined)))))

(define (stringify x)
  (cond
   ((string? x) x)
   ((number? x) (number->string x))
   ((eq? x 'undefined) "")
   ((eq? x 'null) "")
   ((eq? x #t) "true")
   ((eq? x #f) "false")
   ((vector? x) (string-join (map stringify (vector->list x)) ","))
   ((or (pair? x) (null? x)) "[object Object]")
   (else
    (abort (make-composite-condition
            (make-property-condition
             'exn
             'location 'stringify
             'message (sprintf "Cannot stringify non-JSON value: ~S~n" x))
            (make-property-condition 'magery))))))

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
