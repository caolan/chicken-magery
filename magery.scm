(module magery

;; exports
(read-templates
 template-write
 magery-eval-file
 magery-compile-file
 magery-eval-directory
 magery-compile-directory
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

(define (template-write name data #!optional (port (current-output-port)))
  (if (hash-table-exists? (templates) name)
      (with-output-to-port port
        (lambda ()
          ((compiled-template-render (hash-table-ref (templates) name)) data)
          (newline)))
      (abort (make-composite-condition
              (make-property-condition
               'exn
               'location 'template-write
               'message (sprintf "No such template \"~A\"" name))
              (make-property-condition 'magery)))))

)
