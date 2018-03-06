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
         (error-msg (string-trim-both
                     (if (file-exists? error-file) (read-all error-file) #f)))
         (error-expected (and error-msg (not (string=? "" error-msg))))
         (test-name (pathname-strip-directory dir)))
    (parameterize
        ((templates (make-templates)))
      (if error-expected
          (handle-exceptions exn
            (test (sprintf "~A (error)" test-name)
                  error-msg
                  ((condition-property-accessor 'exn 'message) exn))
            (begin
                  (magery-eval-file template-file)
                  (with-output-to-string
                    (lambda ()
                      (template-write 'app-main data)
                      (newline)))
                  (test (sprintf "~A (error)" test-name)
                        (sprintf "ERROR: ~A" error-msg)
                        'NO-ERROR)))
          (test test-name
                expected
                (begin
                  (magery-eval-file template-file)
                  (with-output-to-string
                    (lambda () (template-write 'app-main data)))))))))

(test-group "magery-tests suite"
  (for-each run-test test-dirs))

(test-group "load templates recursively from directory"
  (parameterize
      ((templates (make-templates)))
    (magery-eval-directory "./tests/fixtures")
    (let ((data '((name . "world"))))
      (test "test-greeting"
	    (string-append
	      "<test-greeting>\n"
	      "    <example-wrapper>\n"
	      "    <div><h1>Hello, world!</h1></div>\n"
	      "</example-wrapper>\n"
	      "</test-greeting>\n")
            (with-output-to-string
              (lambda ()
                (template-write 'test-greeting data)))))))
  
(test-exit)
