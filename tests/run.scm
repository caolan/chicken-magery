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
         (error-msg (if (file-exists? error-file)
                        (irregex-replace "\n$" (read-all error-file) "")
                        #f))
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
                      (write-component 'app-main data)
                      (newline)))
                  (test (sprintf "~A (error)" test-name)
                        (sprintf "ERROR: ~A" error-msg)
                        'NO-ERROR)))
          (test test-name
                expected
                (begin
                  (magery-eval-file template-file)
                  (with-output-to-string
                    (lambda ()
                      (write-component 'app-main data)
                      (newline)))))))))

(test-group "magery-tests suite"
  (for-each run-test test-dirs))

(test-group "render full page"
  (parameterize
      ((templates (make-templates)))
    (magery-eval-file "./tests/fixtures1.html")
    (let ((data '((name . "world"))))
      (test "fixtures1.html"
            (string-append "<!DOCTYPE html>\n"
                           "<html>\n"
                           "        <head>\n"
                           "            <title>Test page</title>\n"
                           "        </head>\n"
                           "        <body>\n"
                           "            <p>Hello, world!</p>\n"
                           "        </body>\n"
                           "    </html>\n")
            (with-output-to-string
              (lambda ()
                (write-page 'app-main data)))))))

(test-group "render html element as wrapper"
  (parameterize
      ((templates (make-templates)))
    (magery-eval-file "./tests/fixtures2.html")
    (let ((data '((name . "world"))))
      (test "fixtures2.html"
            (string-append "<!DOCTYPE html>\n"
                           "<html>\n"
                           "        <head>\n"
                           "            <title>Test page</title>\n"
                           "        </head>\n"
                           "        <body>\n"
                           "            \n"
                           "        <p>Hello, world!</p>\n"
                           "    \n"
                           "        </body>\n"
                           "    </html>\n")
            (with-output-to-string
              (lambda ()
                (write-page 'app-main data)))))))

(test-group "load templates recursively from directory"
  (parameterize
      ((templates (make-templates)))
    (magery-eval-directory "./tests/fixtures")
    (let ((data '((name . "world"))))
      (test "test-greeting"
            "<div><h1>Hello, world!</h1></div>"
            (with-output-to-string
              (lambda ()
                (write-component 'test-greeting data)))))))
  

(test-exit)
