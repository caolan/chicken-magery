(module magery

;; exports
(read-templates
 magery-eval-file
 magery-compile-file
 magery-eval-directory
 magery-compile-directory
 write-component
 write-page
 templates
 make-templates)

(import chicken scheme)

(use magery-compiler
     magery-runtime
     srfi-69
     ports
     utils
     files
     extras
     posix)


(define (magery-eval-file filename)
  (eval (with-input-from-file filename read-templates)))

(define (magery-compile-file filename)
  (let ((tmp-file (create-temporary-file "scm")))
    (with-output-to-file tmp-file
      (lambda () (write (with-input-from-file filename read-templates))))
    (compile-file tmp-file load: #t)
    (delete-file tmp-file)))

(define (magery-directory-iter dir f)
  (find-files
   dir
   action: (lambda (filename _) (f filename))
   test: (lambda (x)
           (and (pathname-extension x)
                (string=? (pathname-extension x) "html")))))

(define (magery-eval-directory dir)
  (magery-directory-iter dir magery-eval-file))

(define (magery-compile-directory dir)
  (magery-directory-iter dir magery-compile-file))

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
  (write-string "<!DOCTYPE html>\n" #f port)
  (write-component top-level-component data port)
  (newline))

)
