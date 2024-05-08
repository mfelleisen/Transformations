#lang racket 

;; ---------------------------------------------------------------------------------------------------
;; test them all

(provide
 ;; SYNTAX
 #; (test f in mod-name ... with test-case ...)
 ;; tests the definitions of `f` in sub-modules `mod-name` ... with the specified cases
 test)

(provide ;; to keep things simple 
 in-suffix)

;; ---------------------------------------------------------------------------------------------------
(require (for-syntax syntax/parse))
(require (for-syntax racket/format))
(require "Matthias/in-suffix.rkt")

;; ---------------------------------------------------------------------------------------------------
(define-syntax (test stx)
  (syntax-parse stx
    [(_ exported (~datum in) name:id ...
        [(~optional (~seq (~datum #:show-graph)  sg) #:defaults ([sg #'#false]))
         (~optional (~seq (~datum #:measure)     mm) #:defaults ([mm #'#false]))]
        (~datum with) test-case ...
        )
     #:do [(define n* (map syntax-e (syntax->list #'(name ...))))
           (define p* (map (λ (n) (~a (symbol->string n) ":")) n*))]
     #:with (ss ...) (map (with-prefix stx #'exported) n*)
     
     #`(module+ test
         #,(datum->syntax stx '(require rackunit))
         ; rr ...
         ;; add more test cases here
         (let ([exported ss])
           (eprintf "testing ~a\n" 'name)
           test-case
           ...)
         ...)]))

(define-syntax-rule (timed mm eb ...) (time (for ([i mm]) eb ...)))
(define-syntax-rule (id mm eb ...) (let () eb ...))

;; compile time for `test`
#; {Syntax -> [String Symbol -> Syntax]}
(define-for-syntax ((require-with-prefix stx) pre mod-name)
  (datum->syntax stx `(require (prefix-in ,(string->symbol pre) (submod ".." ,mod-name)))))

#; {Syntax -> [Symbol -> Syntax]}
(define-for-syntax ((with-prefix stx syntax-name) pre)
  (define name (syntax-e syntax-name))
  (datum->syntax stx (string->symbol (~a name "-" pre))))

