#lang racket

(require "testing.rkt")

;; -----------------------------------------------------------------------------
#; {[Listof [Listof Number]] -> Number}
;; compute the sum of all numbers in the given lst

;; -----------------------------------------------------------------------------
(module keywords racket
  (provide go return push)
  (define [go th] [th])
  (define return identity)
  (define push list))

(def-module module% sum-sublists keywords)

;; -----------------------------------------------------------------------------
(module% modern
  (define from #false)
  (define rationale "none")

  (define (sum-sublists lst)
    (for/sum ([1lst lst])
      (for/sum ([n 1lst])
        n))))

;; -----------------------------------------------------------------------------
(module% 2010-style
  (define from #false)
  (define rationale "none")
  
  (define (sum-sublists lst)
    (foldl + 0 (map (lambda (sub) (foldl + 0 sub)) lst))))

;; -----------------------------------------------------------------------------
(module% unfold-fold-and-map ;; uses only first-order functions
  (define from #false)
  (define rationale "none")
  
  (define (sum-sublists lst)
    (fold-outer (map lst)))

  #; {[Listof Number] -> Number}
  (define (fold-inner lst)
    (cond
      [(empty? lst) 0]
      [else (+ (first lst) (fold-inner (rest lst)))]))

  #; {[Listof Number] -> Number}
  (define (f sub)
    (fold-inner sub))

  #; {[Listof [Listof Number]] -> [Listof Number]}
  (define (map lst)
    (cond
      [(empty? lst) '()]
      [else (cons (f (first lst)) (map (rest lst)))]))

  #; {[Listof Number] -> Number}
  [define (fold-outer lst)
    (cond
      [(empty? lst) 0]
      [else (+ (first lst) (fold-outer (rest lst)))])])

;; -----------------------------------------------------------------------------
(module% hoist-fold ;; and eliminate duplicate
  (define from #false)
  (define rationale "none")

  (define (sum-sublists lst)
    (fold (map lst)))

  #; {[Listof Number] -> Number}
  (define (fold lst)
    (cond
      [(empty? lst) 0]
      [else (+ (first lst) (fold (rest lst)))]))

  #; {[Listof Number] -> Number}
  (define (f sub)
    (fold sub))

  #; {[Listof [Listof Number]] -> [Listof Number]}
  (define (map lst)
    (cond
      [(empty? lst) '()]
      [else (cons (f (first lst)) (map (rest lst)))])))

;; -----------------------------------------------------------------------------
(module% cps ;; notice that this one uses `lambda` -- more features?
  (define from #false)
  (define rationale "none")
  
  (define (sum-sublists lst)
    (map lst (λ (x) (fold x return))))

  #; {type [Cont X] = [X -> Empty]}

  #; {[Listof Number] [Cont Number] -> Number}
  (define (fold lst k)
    (cond
      [(empty? lst) (return-to k 0)]
      [else (fold (rest lst) (lambda (x) (return-to k (+ (first lst) x))))]))

  #; {[Listof [Listof Number]] [Cont [Listof Number]] -> [Listof Number]}
  (define (map lst k)
    (cond
      [(empty? lst) (return-to k '())]
      [else
       (fold (first lst)
             (lambda (x)
               (map (rest lst)
                    (lambda (y)
                      (return-to k (cons x y))))))]))

  #; {[Cont X] X -> Empty}
  (define (return-to k v)
    (k v)))

;; -----------------------------------------------------------------------------
(module% closurized
  (define from #false)
  (define rationale "none")

  (define (sum-sublists lst)
    (map lst 'fold1))

  #; {type [Cont X] =
           'never-return ||
           'fold1 ||
           `(plus ,[Cont X] ,X) ||
           `(cons ,[Cont X] ,X) ||
           `(cdr ,[Cont X] ,X)}

  #; {[Listof Number] [Cont Number] -> Number}
  (define (fold lst k)
    (cond
      [(empty? lst) (return-to k 0)]
      [else (fold (rest lst) `(plus ,k ,lst))]))

  #; {[Listof [Listof Number]] [Cont [Listof Number]] -> [Listof Number]}
  (define (map lst k)
    (cond
      [(empty? lst) (return-to k '())]
      [else (fold (first lst) `(cdr ,k ,lst))]))

  #; {[Cont X] X -> Empty}
  (define (return-to k v)
    (match k
      ['never-return    (return v)]
      ['fold1           (fold v 'never-return)]
      [`(plus ,k ,lst)  (return-to k (+ (first lst) v))]
      [`(cons ,k ,x)   (return-to k (cons x v))]
      [`(cdr ,k ,lst) (map (rest lst) `(cons ,k ,v))])))

;; -----------------------------------------------------------------------------
(module% asm ;; and inline the inner fold
  (define from #false)
  (define rationale "none")

  (define lst #false)
  (define k   'fold1)
  (define v   #false)
  
  (define (sum-sublists lst0)
    ;; set registers 
    (set! lst lst0)
    ;; follow instructions:
    (map))

  [define (fold)
    (cond
      [(empty? lst)
       (set! k k)
       (set! v 0)
       (pop)]
      [else
       (set! k   (push 'plus k lst))
       (set! lst (rest lst))
       (fold)])]
  
  [define (map)
    (cond
      [(empty? lst)
       (set! k k)
       (set! v '())
       (pop)]
      [else
       (set! k (push 'cdr k lst))
       (set! lst (first lst))
       (fold)])]
  
  [define (pop)
    (match k
      ['never-return  (return v)]
      ['fold1         (set! k 'never-return)  (set! lst v)             (go fold)]
      [`(plus ,γ ,u)  (set! k γ)              (set! v (+ (first u) v)) (go pop)]
      [`(cons ,γ ,x)  (set! k γ)              (set! v (cons x v))      (go pop)]
      [`(cdr ,γ ,u) (set! k (push 'cons γ v)) (set! lst (rest u))      (go map)])])

;; -----------------------------------------------------------------------------
(module% c-style
  (define from #false)
  (define rationale "none")
  
  (define (sum-sublists lst)
    (define sum 0)
    (for ([1lst lst])
      (for ([n 1lst])
        (set! sum (+ sum n))))
    (return sum)))

;; -----------------------------------------------------------------------------
;; test them all 

(test sum-sublists
      in
      modern 2010-style unfold-fold-and-map hoist-fold cps asm closurized c-style
      with
      (check-equal? (sum-sublists '[[1 2 3] [4] [5 6] [7 8 9]]) 45))
