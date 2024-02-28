#lang racket

;; ---------------------------------------------------------------------------------------------------
(provide
 ;; SYNTAX 
 #; (def-module module-kind exported-function imported-sub-modules ...)
 ;; creates a new "submodule" form that
 ;; - always exports `exported-function`
 ;; - always requires `imported-modules` ...
 #; (def-module module% f a b c)
 ;; means programmers can now write 
 #; (module% (define f ...) ...)
 ;; and `f` is exported and defined in the context of `a` `b` and `c`
 def-module)

(require (for-syntax syntax/parse))

(define-syntax (def-module-mf stx)
  (syntax-parse stx
    [(_ module% exported keywords ...)
     #'(define-syntax (module% stx)
         (syntax-parse stx
           [(_ m def (... ...))
            (datum->syntax
             stx
             `(module ,#'m racket
                (provide ,#'exported)
                (require (submod ".." keywords)) ...
                ,@#'(def (... ...))))]))]))

(define-syntax (def-module stx)
  (syntax-parse stx
    [(_ module% exported keywords ...)
     #`(define-syntax module% (make-def-module-transformer #'exported #'(keywords ...)))]))

(define-for-syntax ((make-def-module-transformer exported0 lib0) stx)
  (define/syntax-parse exported exported0)
  (define/syntax-parse (lib ...) lib0)
  (syntax-parse stx
    [(_ m def ...)
     #:with (rr ...) (map (λ (k) (datum->syntax stx `(require (submod ".." ,k)))) (attribute lib))
     #`(module m #,(datum->syntax stx 'racket)
         (#,(datum->syntax stx 'provide) exported)
         rr ...
         def ...)]))

;; ---------------------------------------------------------------------------------------------------
;; test them all

(provide
 ;; SYNTAX
 #; (test f in mod-name ... with test-case ...)
 ;; tests the definitions of `f` in sub-modules `mod-name` ... with the specified cases
 test)

(require (for-syntax syntax/parse))
(require (for-syntax racket/format))

; (module+ test (require rackunit))

(define-syntax (test stx)
  (syntax-parse stx
    [(_ exported (~datum in) name ... (~datum with) test-case ...)
     #:do [(define n* (map syntax-e (syntax->list #'(name ...))))
           (define p* (map (λ (n) (~a (symbol->string n) ":")) n*))]
     #:with (rr ...) (map (require-with-prefix stx) p* n*)
     #:with (ss ...) (map (sum-sublists-with-prefix stx #'exported) p*)
     #`(module+ test
         #,(datum->syntax stx '(require rackunit))
         rr ...
         ;; add more test cases here
         (let ([exported ss])
           test-case
           ...)
         ...)]))

#; {Syntax -> [String Symbol -> Syntax]}
(define-for-syntax ((require-with-prefix stx) pre mod-name)
  (datum->syntax stx `(require (prefix-in ,(string->symbol pre) (submod ".." ,mod-name)))))

#; {Syntax -> [Symbol -> Syntax]}
(define-for-syntax ((sum-sublists-with-prefix stx syntax-name) pre)
  (define name (syntax-e syntax-name))
  (datum->syntax stx (string->symbol (~a pre name))))
  

