#lang racket

(provide in-suffix)

(#%require '#%unsafe)

(begin-for-syntax
  (define lst-sym (string->uninterned-symbol "lst"))
  (define rest-sym (string->uninterned-symbol "rest")))

(define-syntax-rule (unless-unsafe e)
  (unless (variable-reference-from-unsafe? (#%variable-reference)) e))

(define (check-list l)
  (unless (list? l) (raise-argument-error 'in-list "list?" l)))

(define-sequence-syntax in-suffix
  (lambda () #'in-list)
  (lambda (stx)
    (syntax-case stx (list)
      [[(id) (_ (list expr))] #'[(id) (:do-in ([(id) expr]) (void) () #t () #t #f ())]]
      [[(id) (_ lst-expr)]
       (with-syntax ([lst lst-sym]
                     [rest rest-sym])
         #'[(id)
            (:do-in
             ;;outer bindings
             ([(lst) lst-expr])
             ;; outer check
             (unless-unsafe (check-list lst))
             ;; loop bindings
             ([lst lst])
             ;; pos check
             (pair? lst)
             ;; inner bindings
             ([(id) lst]
              [(rest) (unsafe-cdr lst)]) ; so `lst` is not necessarily retained during body
             ;; pre guard
             #t
             ;; post guard
             #t
             ;; loop args
             (rest))])]
      [_ #f])))
