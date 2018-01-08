(use test posix files magery html-parser medea)

(define test-directory
  "tests/magery-tests/components")

(define test-dirs
  (map (lambda (dir)
         (make-pathname test-directory dir))
       (sort (directory test-directory) string<?)))

(define (run-test dir)
  (let* ((data-file (make-pathname dir "data.json"))
         (template-file (make-pathname dir "template.html"))
         (expected-file (make-pathname dir "expected.html"))
         (data (call-with-input-file data-file read-json))
         (expected (call-with-input-file expected-file html->sxml)))
    (eval-templates template-file)
    ;; (compile-templates template-file)
    (test (pathname-strip-directory dir)
          expected
          (with-input-from-string
              (with-output-to-string
                (lambda () (template 'main data)))
            html->sxml))))

(test-group "magery-tests suite"
  (for-each run-test test-dirs))


(test-exit)
