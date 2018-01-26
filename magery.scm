(module magery

;; exports
(read-templates
 eval-templates
 compile-templates
 template-write)

(import chicken scheme)
(use magery-compiler magery-runtime srfi-69 ports utils files)


(define (eval-templates filename)
  (let ((code (with-input-from-file filename read-templates)))
    ;; TODO: remove debugging output
    (write code)
    (newline)
    (eval code)))

(define (compile-templates filename)
  (let ((tmp-file (create-temporary-file "scm")))
    (with-output-to-file tmp-file
      (lambda () (write (with-input-from-file filename read-templates))))
    (compile-file tmp-file load: #t)
    (delete-file tmp-file)))

(define (template-write name data #!optional (port (current-output-port)))
  (if (hash-table-exists? (templates) name)
      (with-output-to-port port
        (lambda ()
          ((hash-table-ref (templates) name) data)
          (newline)))
      (abort (make-composite-condition
              (make-property-condition
               'exn
               'location 'template-write
               'message (sprintf "No such template: ~S" name))
              (make-property-condition 'magery)))))

)
