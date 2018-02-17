(module magery

;; exports
(read-templates
 eval-templates
 compile-templates
 write-component
 write-page
 templates
 make-templates)

(import chicken scheme)
(use magery-compiler magery-runtime srfi-69 ports utils files extras)


(define (eval-templates filename)
  (eval (with-input-from-file filename read-templates)))

(define (compile-templates filename)
  (let ((tmp-file (create-temporary-file "scm")))
    (with-output-to-file tmp-file
      (lambda () (write (with-input-from-file filename read-templates))))
    (compile-file tmp-file load: #t)
    (delete-file tmp-file)))

(define (write-component name data #!optional (port (current-output-port)))
  (if (hash-table-exists? (templates) name)
      (with-output-to-port port
        (lambda ()
          ((compiled-template-render (hash-table-ref (templates) name)) data)))
      (abort (make-composite-condition
              (make-property-condition
               'exn
               'location 'template-write
               'message (sprintf "No such template ~S" (symbol->string name)))
              (make-property-condition 'magery)))))

(define (write-page top-level-component data #!optional (port (current-output-port)))
  (write-string "<!DOCTYPE html>\n" port)
  (write-component top-level-component data port)
  (newline))

)
