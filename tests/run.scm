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
         (error-file (make-pathname dir "error.txt"))
         (data (call-with-input-file data-file read-json))
         (expected (read-all expected-file))
         (error-msg (if (file-exists? error-file) (read-all error-file) #f))
         (error-expected (and error-msg (not (string=? "" error-msg))))
         (test-name (pathname-strip-directory dir)))
    (parameterize
        ((templates (make-templates)))
      (if error-expected
          (test-error (sprintf "~A (error)" test-name)
                      (begin
                        (eval-templates template-file)
                        (with-output-to-string
                          (lambda () (template-write 'app-main data)))))
          (test test-name
                expected
                (begin
                  (eval-templates template-file)
                  (with-output-to-string
                    (lambda () (template-write 'app-main data)))))))))

(test-group "magery-tests suite"
  (for-each run-test test-dirs))

(test-exit)
