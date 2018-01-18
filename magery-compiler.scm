(module magery-compiler

;; exports
(read-templates)

(import chicken scheme)

(use htmlprag
     data-structures
     miscmacros
     defstruct
     comparse
     utils
     srfi-1
     srfi-13
     srfi-14)

(define SELF-CLOSING
  '(area base br col embed hr img input keygen link menuitem meta
    param source track wbr))

(define BOOLEAN-ATTRIBUTES
  '(allowfullscreen async autofocus
    autoplay capture checked controls default defer
    disabled formnovalidate hidden itemscope loop
    multiple muted novalidate open readonly required
    reversed selected))


(define-record template name src children)
(define-record-printer (template x out)
  (fprintf out "#<template ~S ~S>"
           (template-name x)
           (template-children x)))

(define-record template-call name attributes children)
(define-record-printer (template-call x out)
  (fprintf out "#<template-call ~S ~S ~S>"
           (template-call-name x)
           (template-call-attributes x)
           (template-call-children x)))

(define-record raw value)
(define-record-printer (raw x out)
  (fprintf out "#<raw ~S>" (raw-value x)))

(define-record variable path)
(define-record-printer (variable x out)
  (fprintf out "#<variable ~S>" (variable-path x)))

(define-record data-if path children)
(define-record-printer (data-if x out)
  (fprintf out "#<data-if ~S ~S>"
           (data-if-path x)
           (data-if-children x)))

(define-record data-unless path children)
(define-record-printer (data-unless x out)
  (fprintf out "#<data-unless ~S ~S>"
           (data-unless-path x)
           (data-unless-children x)))

(define-record data-each name iterable children)
(define-record-printer (data-each x out)
  (fprintf out "#<data-each ~S in ~S ~S>"
           (data-each-name x)
           (data-each-iterable x)
           (data-each-children x)))


(define (magery-syntax-error message)
  (abort
   (make-composite-condition
    (make-property-condition 'exn 'message message)
    (make-property-condition 'magery)
    (make-property-condition 'syntax))))

(define (doctype? node)
  (and (pair? node)
       (eq? (car node) '*DECL*)
       (eq? (cadr node) 'DOCTYPE)))

(define (document? node)
  (and (pair? node)
       (eq? (car node) '*TOP*)))

(define (comment? node)
  (and (pair? node)
       (eq? (car node) '*COMMENT*)))

(define (text? node)
  (string? node))

(define (element? node)
  (and (pair? node)
       (symbol? (car node))
       (not (document? node))
       (not (comment? node))
       (not (doctype? node))))

(define (tagname node)
  (assert (element? node))
  (car node))

(define (has-attributes? node)
  (assert (element? node))
  (and (pair? (cdr node))
       (pair? (cadr node))
       (eq? '@ (caadr node))))

(define (attributes node)
  (if (has-attributes? node)
      (cdadr node)
      '()))
  
(define (attribute-exists? node name)
  (and (has-attributes? node)
       (assq name (attributes node))))
      
(define (attribute-ref node name)
  (car (alist-ref name (attributes node))))

(define attribute-name car)

(define (attribute-value attr)
  (if (null? (cdr attr)) "" (cadr attr)))

(define (compile-attribute-expansion attr)
  (let ((parts (compile-variables (attribute-value attr))))
    (if (and (= 1 (length parts))
             (variable? (car parts)))
        `(lookup (quote ,(variable-path (car parts))) data)
        `(string-join
          ,(cons 'list
                 (map (lambda (part)
                        (if (raw? part)
                            (raw-value part)
                            `(lookup (quote ,(variable-path (car parts))) data)))
                      parts))))))

(define (compile-attributes attrs)
  (map (lambda (attr)
         (if (memq (attribute-name attr) BOOLEAN-ATTRIBUTES)
             (let ((parts (compile-variables (attribute-value attr))))
               (if (and (= 1 (length parts))
                        (variable? (car parts)))
                   ;; check truthiness of a single variable
                   (list (make-data-if
                          (variable-path (car parts))
                          (list (make-raw (sprintf " ~A" (attribute-name attr))))))
                   ;; if it's an interpolated string, boolean attribute is always true
                   (list (make-raw (sprintf " ~A" (attribute-name attr))))))
             ;; render normal attribute
             (list (make-raw (sprintf " ~A=\"" (attribute-name attr)))
                   (compile-variables (attribute-value attr))
                   (make-raw "\""))))
       (reverse attrs)))

(define (compile-opening-tag name attrs)
  (list (make-raw (sprintf "<~A" name))
        (compile-attributes attrs)
        (make-raw ">")))

(define (compile-closing-tag name)
  (make-raw (sprintf "</~A>" name)))

(define (compile-element node queue is-root)
  (case (tagname node)
    ((template)
     (if (attribute-exists? node 'data-tagname)
         (if is-root
             (let ((tag (attribute-ref node 'data-tagname)))
               (make-template
                (string->symbol tag)
                node
                ;; TODO: assert template tagname follows safe naming scheme
                `(,(compile-opening-tag tag '())
                  ,@(map (cut compile-node <> queue #f) (children node))
                  ,(compile-closing-tag tag))))
             (queue-add! queue node))
         (abort (make-property-condition
                 'exn
                 'message "Not implemented: <template> tag without data-template attribute"
                 'location 'compile-element))))
    (else
     (let loop ((attrs '())
                (processing (attributes node)))
       (if (not (null? processing))
           (let ((next (car processing)))
             (case (attribute-name next)
               ((data-if)
                (list
                 (make-data-if (string->path (attribute-value next))
                               (loop attrs (cdr processing)))))
               ((data-unless)
                (list
                 (make-data-unless (string->path (attribute-value next))
                                   (loop attrs (cdr processing)))))
               ((data-each)
                (let* ((result (parse-data-each (attribute-value next)))
                       (name (first result))
                       (iterable (second result)))
                  (list
                   (make-data-each name
                                   iterable
                                   (loop attrs (cdr processing))))))
               ((data-key)
                ;; skip data-key on server
                (loop attrs (cdr processing)))
               (else
                (if (string-prefix? "on" (symbol->string (attribute-name next)))
                    ;; skip on* event handlers
                    (loop attrs (cdr processing))
                    (loop (cons next attrs) (cdr processing))))))
           ;; finished processing attributes
           (if (string-contains (symbol->string (tagname node)) "-")
               ;; possible component
               (list (make-template-call
                      (tagname node)
                      attrs
                      (map (cut compile-node <> queue #f) (children node))))
               ;; normal element
               (list (compile-opening-tag (tagname node) attrs)
                     (if (memq (tagname node) SELF-CLOSING)
                         '()
                         (list (map (cut compile-node <> queue #f) (children node))
                               (compile-closing-tag (tagname node)))))))))))

(define (parse-data-each value)
  (assert (string? value))
  (parse data-each-expression (->parser-input (string-trim-both value))))

(define data-each-expression
  (bind
   (sequence
     (as-string (one-or-more (none-of* (in char-set:whitespace) item)))
     (skip (one-or-more (in char-set:whitespace)))
     (skip (char-seq "in"))
     (skip (one-or-more (in char-set:whitespace)))
     (as-string (one-or-more item)))
   (lambda (x)
     (result (list (string->symbol (first x))
                   (string->path (last x)))))))

(define (string->path str)
  (map (compose string->symbol string-trim-both)
       (string-split str ".")))
  
(define variable-expansion
  (bind (enclosed-by (char-seq "{{")
                     (as-string (one-or-more (none-of* (char-seq "}}") item)))
                     (char-seq "}}"))
        (lambda (x)
          (result (make-variable (string->path x))))))

(define raw-text
  (bind (as-string (one-or-more (none-of* (char-seq "{{") item)))
        (compose result make-raw)))

(define template-string
  (zero-or-more (any-of variable-expansion raw-text)))

(define (compile-variables value)
  (assert (string? value))
  (parse template-string (->parser-input value)))

(define (compile-text node)
  (compile-variables node))

(define (compile-comment node)
  (make-raw (sprintf "<!--~A-->" (last node))))

(define (children node)
  (let ((rest (cdr node)))
    (if (and (pair? rest)
             (pair? (car rest))
             (eq? (caar rest) '@))
        (cdr rest)
        rest)))

(define (compile-node node queue is-root)
  (printf "compile-node: ~S~n" node)
  (cond
   ((element? node)
    (compile-element node queue is-root))
   ((text? node)
    (compile-text node))
   ((document? node)
    (for-each
     (cut compile-node <> queue is-root)
     (children node)))
   ((comment? node)
    (compile-comment node))
   ((doctype? node)
    ;; ignore
    #f)
   (else
    (magery-syntax-error
     (sprintf "Unknown node type: ~S"
              (if (pair? node) (car node) node))))))

;; combines adjacent text nodes etc.
(define (collapse-syntax-tree tree)
  (printf "collapse-syntax-tree: ~S~n" tree)
  (fold-right
   (lambda (x collapsed)
     (cond
      ((template? x)
       (cons (make-template (template-name x)
                            (template-src x)
                            (collapse-syntax-tree (template-children x)))
             collapsed))
      ((raw? x)
       (if (and (not (null? collapsed))
                (raw? (car collapsed)))
           (cons (make-raw (string-append (raw-value x) (raw-value (car collapsed))))
                 (cdr collapsed))
           (cons x collapsed)))
      ((variable? x)
       (cons x collapsed))
      ((data-if? x)
       (cons (make-data-if (data-if-path x)
                           (collapse-syntax-tree (data-if-children x)))
             collapsed))
      ((data-unless? x)
       (cons (make-data-unless (data-unless-path x)
                           (collapse-syntax-tree (data-unless-children x)))
             collapsed))
      ((data-each? x)
       (cons (make-data-each (data-each-name x)
                             (data-each-iterable x)
                             (collapse-syntax-tree (data-each-children x)))
             collapsed))
      ((template-call? x)
       (cons (make-template-call (template-call-name x)
                                 (template-call-attributes x)
                                 (collapse-syntax-tree (template-call-children x)))
             collapsed))
      ((pair? x)
       (if (null? collapsed)
           (collapse-syntax-tree x)
           (append (collapse-syntax-tree (append x (take collapsed 1)))
                   (cdr collapsed))))
      ((or (not x) (null? x))
       ;; skip #f and null
       collapsed)
      (else
       (abort (make-property-condition
               'exn
               'message (sprintf "Unexpected element in uncollapsed syntax tree: ~S" x))))))
   '()
   tree))

(define (template->scheme x)
  `(hash-table-set!
    (templates)
    (quote ,(template-name x))
    (lambda (data)
      ,@(if (null? (template-children x))
            (list #f)
            (map ->scheme (template-children x))))))

(define (raw->scheme x)
  `(write-string ,(raw-value x)))

(define (variable->scheme x)
  `(write-string
    (html-escape (stringify (lookup (quote ,(variable-path x)) data)))))

(define (data-if->scheme x)
  `(when (truthy? (lookup (quote ,(data-if-path x)) data))
     ,@(map ->scheme (data-if-children x))))

(define (data-unless->scheme x)
  `(when (falsy? (lookup (quote ,(data-unless-path x)) data))
     ,@(map ->scheme (data-unless-children x))))

(define (data-each->scheme x)
  `(magery-each
    (quote ,(data-each-name x))
    (quote ,(data-each-iterable x))
    (lambda (data)
      ,@(map ->scheme (data-each-children x)))
    data))

(define (template-call->scheme x)
  `((hash-table-ref (templates) (quote ,(template-call-name x)))
    (append (list
             ,@(map (lambda (attr)
                      `(cons (quote ,(attribute-name attr))
                             ,(compile-attribute-expansion attr)))
                    (template-call-attributes x)))
            data)))

(define (->scheme x)
  (cond ((template? x) (template->scheme x))
        ((raw? x) (raw->scheme x))
        ((variable? x) (variable->scheme x))
        ((data-if? x) (data-if->scheme x))
        ((data-unless? x) (data-unless->scheme x))
        ((data-each? x) (data-each->scheme x))
        ((template-call? x) (template-call->scheme x))
        (else
         (abort (make-property-condition
                 'exn
                 'message (sprintf "Unexpected element in collapsed syntax tree: ~S" x))))))

;; converts AST to scheme source code
(define (syntax-tree->scheme tree)
  (printf "syntax-tree->scheme: ~S~n" tree)
  `(begin
     (use magery-runtime)
     ,@(map ->scheme tree)))

(define (read-templates #!optional (port (current-input-port)))
  (let ((html (html->sxml (read-all port)))
        (queue (make-queue)))
    ;; we ignore parent elements which are not template definitions,
    ;; this compile-node call is only to find top-level template
    ;; definitions and add them to the queue
    (compile-node html queue #f)
    (let loop ((result '()))
      (if (queue-empty? queue)
          (syntax-tree->scheme (collapse-syntax-tree result))
          (begin
            (loop (cons (compile-node (queue-remove! queue) queue #t)
                        result)))))))

)
