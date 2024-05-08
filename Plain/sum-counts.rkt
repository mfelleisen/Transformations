#lang racket

(require "../testing-2.rkt")

;; ---------------------------------------------------------------------------------------------------
;; MODULE plain

(define (sc-plain l0) ;; contract  sc/c
  (define L (length l0))
  (for*/sum ([i (in-range 0 L)] [j (in-range i L)])
    (sqr (distinct-plain (slice-plain l0 i j)))))

#; {[Listof X] N N -> [Listof X]}
;; ASSUME (<= i j)
;; extract the sublist `(list x_i ... x_{j-1})` from l = `(list x ...)`
(define (slice-plain l i j)
  (take (drop l i) (- (+ j 1) i)))
    
#; {[Listof X] -> N}
;; the number of `eqv?` distinct Xs in `l`
(define (distinct-plain l)
  (set-count (apply seteqv l)))

;; ---------------------------------------------------------------------------------------------------
;; MODULE elim

(define (sc-elim l0) ;; contract  sc/c
  (let loop ([l l0])
    (cond
      [(empty? l) 0]
      [else (+ (scan-for-distinct-values-elim l) (loop (rest l)))])))

#; {[Listof X] accumulator [Setof X] -> N}
;; determine the squares of the number of distinct values in each sublist of `l`
;; ACCU `seen` keeps track of the distinct elements encountered up to `l`
(define (scan-for-distinct-values-elim l [seen (seteqv)])
  (cond
    [(empty? l) 0]
    [else (define seen++ (set-add seen (first l)))
          (+ (sqr (set-count seen++)) (scan-for-distinct-values-elim (rest l) seen++))]))

;; ---------------------------------------------------------------------------------------------------
;; MODULE elim-2

(define (sc-elim-2 l0) ;; contract  sc/c
  (let loop ([l l0])
    (cond
      [(empty? l) 0]
      [else (+ (scan-for-distinct-values-elim-2 l) (loop (rest l)))])))

#; {[Listof X] accumulator [Setof X] -> N}
;; determine the squares of the number of distinct values in each sublist of `l`
;; ACCU `seen` keeps track of the distinct elements encountered up to `l`
(define (scan-for-distinct-values-elim-2 l)
  (define seen (seteqv))
  (for/sum ([x l])
    (set! seen (set-add seen x))
    (sqr (set-count seen))))

;; ---------------------------------------------------------------------------------------------------
;; MODULE elim-3
  
(define (sc-elim-3 l0) ;; contract  sc/c
  (for/sum ([l (in-suffix l0)])
    (scan-for-distinct-values-elim-3 l)))

#; {[Listof X] accumulator [Setof X] -> N}
;; determine the squares of the number of distinct values in each sublist of `l`
;; ACCU `seen` keeps track of the distinct elements encountered up to `l`
(define (scan-for-distinct-values-elim-3 l)
  (define seen (seteqv))
  (for/sum ([x l])
    (set! seen (set-add seen x))
    (sqr (set-count seen))))

;; ---------------------------------------------------------------------------------------------------
;; MODULE inline

(define (sc-inline l0) ;; contract  sc/c
  (let loop ([l l0])
    (cond
      [(empty? l) 0]
      [else
       (define seen (seteqv))
       (define sum 
         (for/sum ([x l])
           (set! seen (set-add seen x))
           (sqr (set-count seen))))
       (+ sum (loop (rest l)))])))

;; ---------------------------------------------------------------------------------------------------
;; MODULE inline-2
  
(define (sc-inline-2 l0) ;; contract  sc/c
  (for/sum ([l (in-suffix l0)])
    (define seen (seteqv))
    (for/sum ([x (in-list l)])
      (set! seen (set-add seen x))
      (sqr (set-count seen)))))

;; ---------------------------------------------------------------------------------------------------
;; MODULE inline-3
  
(define (sc-inline-3 l0) ;; contract  sc/c
  (for/sum ([l (in-suffix l0)])
    (for/fold ([seen (seteqv)] [sum 0] #:result sum) ([x (in-list l)])
      (define s++n (set-add seen x))
      (values s++n (+ sum (sqr (set-count s++n)))))))

;; ---------------------------------------------------------------------------------------------------
(test sc
      in
      plain elim elim-2 elim-3 inline inline-2 inline-3
      [#:show-graph #true]
      with
      (check-equal? (sc (list 1 2 1)) 15)
      (check-equal? (sc (list 1 1)) 3)
      (check-equal? (sc (list 2 2 5 5)) 22)
      (check-equal? (sc (list 5 2 4 2 1 3 2 4 3 1)) 578)
      (check-equal? (sc (list 2 3 2 1 2 5 3 4 5 2)) 629)
      (check-equal? (sc (list 5 1 5 2 3 5 1 5 1)) 385)
      (check-equal? (sc (list 4 5 4 3 4 2)) 120)
      (check-equal? (sc (list 2)) 1)
      (check-equal? (sc (list 3 4 2 5 2 4 1 2 2 5)) 535)
      (check-equal? (sc (list 4 4 2 4 1)) 57)
      (check-equal? (sc (list 2 2 5)) 12)
      (check-equal? (sc (list 4 5 1 2 2 1 3 3)) 266)
      (check-equal? (sc (list 3 1 5 5 2 3 2 2 1)) 334)
      (check-equal? (sc (list 2 5 2 5 3 2 5 2)) 205)
      (check-equal? (sc (list 5 4 1 4 5 2 4)) 203)
      (check-equal? (sc (list 1 3 3 4 3 1 2 1)) 253)
      (check-equal? (sc (list 4)) 1)
      (check-equal? (sc (list 1 4 2 1 5 4 3 1 4)) 507)
      (check-equal? (sc (list 2 4 5 3 2 5 1 5 4 4)) 626)
      (check-equal? (sc (list 3 4 1 4 5 2 2)) 220)
      (check-equal? (sc (list 3 5 1 1 3)) 62)
      (check-equal? (sc (list 4 3 2 5 3)) 89)
      (check-equal? (sc (list 2 5)) 6)
      (check-equal? (sc (list 1 5 1 4 5)) 70)
      (check-equal? (sc (list 5 1)) 6)
      (check-equal? (sc (list 4 5 4 3 3 5 3)) 138)
      (check-equal? (sc (list 5 4 3)) 20)
      (check-equal? (sc (list 5 5 3 3 4 5 4 5 5)) 234)
      (check-equal? (sc (list 3 1 5 5 3 4 5 5 1 4)) 456)
      (check-equal? (sc (list 4 2 3 1 1)) 81)
      (check-equal? (sc (list 4 5 3 1 2 5 5 3 5)) 434)
      (check-equal? (sc (list 3 2 1 2 5 2 4 5 1 5)) 531)
      (check-equal? (sc (list 1 3 1 4 4)) 62)
      (check-equal? (sc (list 5 1 2 1 2 1 2 3 1)) 257)
      (check-equal? (sc (list 2 4)) 6)
      (check-equal? (sc (list 4 5 4 5)) 28)
      (check-equal? (sc (list 3 1 5 5 5 4 3 3 2)) 334)
      (check-equal? (sc (list 3 2 5 2 1 5 3)) 203)
      (check-equal? (sc (list 4 4 2 5 5 4 2 2 1)) 294)
      (check-equal? (sc (list 1)) 1)
      (check-equal? (sc (list 1 1 3 3 3 4 4)) 96)
      (check-equal? (sc (list 3 2 2 3 4)) 57)
      (check-equal? (sc (list 1 5 3 2 4 4)) 161)
      (check-equal? (sc (list 5 4 1 1 3)) 69)
      (check-equal? (sc (list 4 3 3 5 3 4 5 3 3 1)) 376)
      (check-equal? (sc (list 2 3 4 1 5 1 3 3 4)) 432)
      (check-equal? (sc (list 5 1 4 2 1 1)) 129)
      (check-equal? (sc (list 5 4 4 1)) 30)
      (check-equal? (sc (list 1 5 1 3 2 1)) 139)
      (check-equal? (sc (list 5 3)) 6)
      (check-equal? (sc (list 4 1 4 3)) 38)
      (check-equal? (sc (list 1 5 4 3 4 2 4 5 5 4)) 513)
      (check-equal? (sc (list 4 2 3 4 3 2 5 4 4)) 378)
      (check-equal? (sc (list 2 3 3 2 1 5 2 2)) 262)
      (check-equal? (sc (list 2 1 4 2 4 1 4 3)) 243)
      (check-equal? (sc (list 1 4 4 1 3)) 57)
      (check-equal? (sc (list 2 3 2 1)) 38)
      (check-equal? (sc (list 1 4 2 1)) 43)
      (check-equal? (sc (list 2 4 3 2 5 1)) 169)
      (check-equal? (sc (list 2 5 3 2 1 3 1 3 2)) 348)
      (check-equal? (sc (list 4 1)) 6)
      (check-equal? (sc (list 4 3 1 4 3 4 3 4 1)) 263)
      (check-equal? (sc (list 5 1 1 1 4 3)) 89)
      (check-equal? (sc (list 4 5)) 6)
      (check-equal? (sc (list 5 2 2 3 1 2 5 3)) 289)
      (check-equal? (sc (list 3 2 4)) 20)
      (check-equal? (sc (list 5 3 5 2 3 2)) 106)
      (check-equal? (sc (list 3 2 1)) 20)
      (check-equal? (sc (list 4 4 2 4 3)) 57)
      (check-equal? (sc (list 1 4 4)) 12)
      (check-equal? (sc (list 1 4 4 3 1 2 1 4 3)) 387)
      (check-equal? (sc (list 1 5 4 2 5 5 5 3)) 249)
      (check-equal? (sc (list 2 1 5 3)) 50)
      (check-equal? (sc (list 2 3 5 1 5 2 3 2 3 4)) 533)
      (check-equal? (sc (list 5 3 4 4 3 5 4 5)) 202)
      (check-equal? (sc (list 4 4 2 2 4 1)) 80)
      (check-equal? (sc (list 2 3)) 6)
      (check-equal? (sc (list 4 2 3 2)) 38)
      (check-equal? (sc (list 1 2 2)) 12)
      (check-equal? (sc (list 4 1 5 1 5 4 5 1)) 205)
      (check-equal? (sc (list 4 5 3 1)) 50)
      (check-equal? (sc (list 4 2 3 4 2 4 3 3 2)) 275)
      (check-equal? (sc (list 4 3)) 6)
      (check-equal? (sc (list 1 3 5 4 4 4)) 113)
      (check-equal? (sc (list 1 2 4 2 1 2 2 4 1 3)) 391)
      (check-equal? (sc (list 4 2 5 3 2)) 89)
      (check-equal? (sc (list 3 4 5 3 2 5)) 144)
      (check-equal? (sc (list 5 4 5)) 15)
      (check-equal? (sc (list 2 4 5 1)) 50)
      (check-equal? (sc (list 5 4 1 4 2 1 5)) 203)
      (check-equal? (sc (list 2 3 3 2 2 3 1)) 110)
      (check-equal? (sc (list 1 4 2 5)) 50)
      (check-equal? (sc (list 3)) 1)
      (check-equal? (sc (list 5)) 1)
      (check-equal? (sc (list 1 3 5 3 2 1 1 4 3)) 441)
      (check-equal? (sc (list 1 5 2 2 3 3 3)) 140)
      (check-equal? (sc (list 1 2 1 4 5 5 4 1 1 1)) 407)
      (check-equal? (sc (list 2 2 1 1 1 2 5 4 5)) 296)
      (check-equal? (sc (list 3 2 3)) 15)
      (check-equal? (sc (list 2 1 5 4 3 3 2 1 5 5)) 652))
