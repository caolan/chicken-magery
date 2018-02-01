(module magery-runtime

;; exports
(templates
 make-templates
 lookup
 stringify
 html-escape
 truthy?
 falsy?
 magery-each
 make-compiled-template
 compiled-template-render
 compiled-template-src)

(import chicken scheme)
(use srfi-69 data-structures irregex srfi-13 srfi-133)


(define-record compiled-template src render)
(define-record-printer (compiled-template x out)
  (fprintf out "#<compiled-template ~S>"
           (compiled-template-src x)))


(define (make-templates)
  (make-hash-table
   test: eq?
   hash: symbol-hash))

(define templates 
  (make-parameter (make-templates)))

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

(define (magery-each name path f data)
  (let ((iterable (lookup path data)))
    (when (vector? iterable)
      (vector-for-each
       (lambda (v) (f (cons (cons name v) data)))
       iterable))))

(define (falsy? x)
  (or (not x)
      (eq? x 'undefined)
      (eq? x 'null)
      (and (number? x) (= 0 x))
      (and (string? x) (string=? "" x))
      (and (vector? x) (= 0 (vector-length x)))))

(define truthy?
  (compose not falsy?))

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

(define (html-escape str #!optional keep-quot)
  (irregex-replace/all
   "[<>\"'&]"
   str
   (lambda (m)
     (case (string-ref (irregex-match-substring m) 0)
       ((#\<) "&lt;")
       ((#\>) "&gt;")
       ((#\&) "&ampt;")
       ((#\") (if keep-quot "\"" "&quot;"))
       ((#\') "&apos;")))))

)
