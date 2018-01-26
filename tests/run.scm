(use test posix files magery medea)

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
         (expected (read-all expected-file)))
    (eval-templates template-file)
    ;; (compile-templates template-file)
    (let ((result (with-output-to-string (lambda () (template-write 'app-main data)))))
      (newline)
      (print result)
      (print expected)
      (test (pathname-strip-directory dir) expected result))))

(test-group "magery-tests suite"
  (for-each run-test (take test-dirs 79)))

(test-exit)
