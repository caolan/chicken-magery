(module magery-runtime

;; exports
(templates)

(import chicken scheme)
(use srfi-69)

(define templates
  (make-parameter (make-hash-table
                   test: eq?
                   hash: symbol-hash)))

)
