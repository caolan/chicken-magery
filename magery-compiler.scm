(module magery-compiler

;; exports
(read-templates)

(import chicken scheme)

(use html-parser
     data-structures
     miscmacros
     defstruct
     srfi-1
     comparse
     srfi-13)

(define SELF-CLOSING
  '(area base br col embed hr img input keygen link menuitem meta
         param source track wbr))

(defstruct template
  name
  src
  (children '()))

(define-record-printer (template x out)
  (fprintf out "#<template ~S ~S>"
           (template-name x)
           (template-children x)))

(define-record raw
  value)

(define-record-printer (raw x out)
  (fprintf out "#<raw ~S>" (raw-value x)))

(define-record variable
  path)

(define-record-printer (variable x out)
  (fprintf out "#<variable ~S>" (variable-path x)))

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

(define (compile-element node queue is-root)
  (case (tagname node)
    ((template)
     (if (attribute-exists? node 'data-tagname)
         (if is-root
             (let ((tag (attribute-ref node 'data-tagname)))
               (make-template
                name: (string->symbol tag)
                src: node
                ;; TODO: assert template tagname follows safe naming scheme
                children: `(,(make-raw (sprintf "<~A>" tag))
                            ,@(map (cut compile-node <> queue #f) (children node))
                            ,(make-raw (sprintf "</~A>" tag)))))
             (queue-add! queue node))
         (abort (make-property-condition
                 'exn
                 'message "Not implemented: <template> tag without data-template attribute"
                 'location 'compile-element))))
    (else
     (list (make-raw (sprintf "<~A" (tagname node)))
           (map (lambda (attr)
                  (list (make-raw (sprintf " ~A=\"" (car attr)))
                        (compile-variables (cadr attr))
                        (make-raw "\"")))
                (attributes node))
           (make-raw ">")
           (if (memq (tagname node) SELF-CLOSING)
               '()
               (list (map (cut compile-node <> queue #f) (children node))
                     (make-raw (sprintf "</~A>" (tagname node)))))))))

(define variable-expansion
  (bind (enclosed-by (char-seq "{{")
                     (as-string (one-or-more (none-of* (char-seq "}}") item)))
                     (char-seq "}}"))
        (lambda (path)
          (result
           (make-variable (map (compose string->symbol string-trim-both)
                               (string-split path ".")))))))

(define raw-text
  (bind (as-string (one-or-more (none-of* (char-seq "{{") item)))
        (compose result make-raw)))

(define template-string
  (one-or-more (any-of variable-expansion raw-text)))

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
  (fold-right
   (lambda (x collapsed)
     (cond
      ((template? x)
       (cons (update-template x children: (collapse-syntax-tree (template-children x)))
             collapsed))
      ((raw? x)
       (if (and (not (null? collapsed))
                (raw? (car collapsed)))
           (cons (make-raw (string-append (raw-value x) (raw-value (car collapsed))))
                 (cdr collapsed))
           (cons x collapsed)))
      ((variable? x)
       (cons x collapsed))
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
               'message (sprintf "Unexpected element in syntax tree: ~S" x))))))
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

(define (->scheme x)
  (cond ((template? x) (template->scheme x))
        ((raw? x) (raw->scheme x))
        ((variable? x) (variable->scheme x))
        (else
         (abort (make-property-condition
                 'exn
                 'message (sprintf "Unexpected element in collapsed syntax tree: ~S" x))))))

;; converts AST to scheme source code
(define (syntax-tree->scheme tree)
  `(begin
     (use magery-runtime)
     ,@(map ->scheme tree)))

(define (read-templates #!optional (port (current-input-port)))
  (let ((html (html->sxml port))
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
