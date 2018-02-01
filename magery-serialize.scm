;; borrows heavily from the SXML-to-HTML.scm module in the sxml-transforms egg
(module magery-serialize

;; exports
(sxml->html5
 sxml->html5-string)

(import chicken scheme)
(use sxml-transforms ports)


(define (sxml->html5 tree)
 (SRV:send-reply
   (pre-post-order tree
    `((@
      ((*default*       ; local override for attributes
        . ,(lambda (attr-key . value) (enattr attr-key value))))
      . ,(lambda (trigger . value) (cons '@ value)))
     (link . ,(lambda (tag . elems) (entag-self-closing tag elems)))
     (*default* . ,(lambda (tag . elems) (entag* tag elems)))
     (*text* . ,(lambda (trigger str) 
		  (if (string? str) (string->goodHTML str) str)))))))

;; customized (entag) procedure to avoid adding newline before each tag
(define (entag* tag elems)
  (if (and (pair? elems) (pair? (car elems)) (eq? '@ (caar elems)))
    (list #\< tag (cdar elems) #\>
      (and (pair? (cdr elems))
	(list (cdr elems) "</" tag #\>)))
    (list #\< tag #\> (and (pair? elems) (list elems "</" tag #\>))
      )))

(define (entag-self-closing tag elems)
  (if (and (pair? elems)
           (pair? (car elems))
           (eq? '@ (caar elems)))
    (list #\newline #\< tag (cdar elems) #\>)
    (list #\newline #\< tag #\>)))

(define (sxml->html5-string tree)
  (with-output-to-string (cut sxml->html5 tree)))

)
