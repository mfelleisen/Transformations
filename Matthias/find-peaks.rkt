#lang racket

(require "../testing.rkt")

(module general racket ;; constraints collected from problem statememt 
  (provide peaks/c)

  (define LENGTH 100)
  (define peak/c (between/c 1 100))

  (define lon/c
    (and/c (flat-named-contract 'small (listof peak/c))
           (flat-named-contract 'short (compose (integer-in 2 LENGTH) length))))

  (define peaks/c (-> lon/c [listof natural?])))

(def-module module% peaks general)

;; ---------------------------------------------------------------------------------------------------
(module% base-bsl
  (define from '[])
  (define rationale "a 2-accumulator problem only because of the low-level thinking in the problem")

  (define/contract (peaks measurements0) peaks/c
    #; {Real Real Real -> Boolean}
    (define (peak? left x right)
      (and (< left x) (> x right)))

    #; {[Listof Real] Real N -> [Listof N]}
    ;; ACCU `to-the-left` is the item in `measurements0` to the immediate left of `mesasurememts`
    ;; ACCU `index` is the index of the first item on `measurements`
    (define (peaks/accu measurements to-the-left index)
      (cond
        [(empty? (rest measurements)) '()]
        [else
         (define x (first measurements))
         (define to-the-right (second measurements))
         (define remainder (peaks/accu (rest measurements) x (add1 index)))
         (if (peak? to-the-left x to-the-right) (cons index remainder) remainder)]))
    
    (peaks/accu (rest measurements0) (first measurements0) 1)))

;; ---------------------------------------------------------------------------------------------------
(module% functional
  (define from '[])
  (define rationale "a 2-accumulator problem only because of the low-level thinking in the problem")

  (define/contract (peaks measurements0) peaks/c
    (for/fold ([r '()] [left (first measurements0)] [x (second measurements0)] #:result (reverse r))
              ([right (rest (rest measurements0))] [i (in-naturals)])
      (if (peak? left x right)
          (values (cons (add1 i) r) x right)
          (values r                 x right))))

  #; {Real Real Real -> Boolean}
  (define (peak? left x right)
    (and (< left x) (> x right))))

;; ---------------------------------------------------------------------------------------------------
(module% imperative
  (define from `[[functional ,IMPERATIVE-ACCU]])
  (define rationale "a 2-accumulator problem only because of the low-level thinking in the problem")

  (define/contract (peaks measurements0) peaks/c
    (define r     '[])
    (define left  (first measurements0))
    (define x     (second measurements0))
    (for/fold  ([_ 'any] #:result (reverse r)) ([right (rest (rest measurements0))] [i (in-naturals)])
      (when (peak? left x right) (set! r (cons (add1 i) r)))
      (set! left x)
      (set! x    right)))

  #; {Real Real Real -> Boolean}
  (define (peak? left x right)
    (and (< left x) (> x right))))

;; ---------------------------------------------------------------------------------------------------
(module% imperative-simplified
  (define from `[[functional ,IMPERATIVE-FOLD]])
  (define rationale "a 2-accumulator problem only because of the low-level thinking in the problem")

  (define/contract (peaks measurements0) peaks/c
    (define left (first measurements0))
    (define x    (second measurements0))
    (for/list ([right (rest (rest measurements0))] [i (in-naturals)]
               #:when (begin0 (peak? left x right) (set!-values [left x] (values x right))))
      (+ i 1)))

  #; {Real Real Real -> Boolean}
  (define (peak? left x right)
    (and (< left x) (> x right))))

;; ---------------------------------------------------------------------------------------------------
(test peaks
      in
      base-bsl
      functional imperative-simplified imperative
      [#:show-graph #true]
      with
      (check-exn #px"short" (λ () (peaks '[2])))
      (check-equal? (peaks (list 2 4 4)) '[])
      (check-equal? (peaks (list 1 4 3 8 5)) (list 1 3))
      (check-equal? (peaks (list 1 1 1)) '[])
      (check-equal? (peaks (list 1 1 3)) '[])
      (check-equal? (peaks (list 1 1 5)) '[])
      (check-equal? (peaks (list 1 2 5)) '[])
      (check-equal? (peaks (list 1 4 1)) (list 1))
      (check-equal? (peaks (list 1 4 3)) (list 1))
      (check-equal? (peaks (list 1 5 5)) '[])
      (check-equal? (peaks (list 1 6 4)) (list 1))
      (check-equal? (peaks (list 2 1 1)) '[])
      (check-equal? (peaks (list 2 1 2)) '[])
      (check-equal? (peaks (list 2 2 3)) '[])
      (check-equal? (peaks (list 2 2 5)) '[])
      (check-equal? (peaks (list 2 3 2)) (list 1))
      (check-equal? (peaks (list 2 3 6)) '[])
      (check-equal? (peaks (list 2 4 3)) (list 1))
      (check-equal? (peaks (list 2 4 5)) '[])
      (check-equal? (peaks (list 2 6 4)) (list 1))
      (check-equal? (peaks (list 3 3 3)) '[])
      (check-equal? (peaks (list 3 3 5)) '[])
      (check-equal? (peaks (list 3 4 6)) '[])
      (check-equal? (peaks (list 3 5 1)) (list 1))
      (check-equal? (peaks (list 3 5 3)) (list 1))
      (check-equal? (peaks (list 3 5 4)) (list 1))
      (check-equal? (peaks (list 3 5 6)) '[])
      (check-equal? (peaks (list 4 2 1)) '[])
      (check-equal? (peaks (list 4 2 2)) '[])
      (check-equal? (peaks (list 4 2 4)) '[])
      (check-equal? (peaks (list 4 2 6)) '[])
      (check-equal? (peaks (list 4 4 1)) '[])
      (check-equal? (peaks (list 4 4 2)) '[])
      (check-equal? (peaks (list 4 4 5)) '[])
      (check-equal? (peaks (list 4 5 4)) (list 1))
      (check-equal? (peaks (list 4 6 1)) (list 1))
      (check-equal? (peaks (list 4 6 6)) '[])
      (check-equal? (peaks (list 5 1 2)) '[])
      (check-equal? (peaks (list 5 2 1)) '[])
      (check-equal? (peaks (list 5 2 2)) '[])
      (check-equal? (peaks (list 5 2 4)) '[])
      (check-equal? (peaks (list 5 3 1)) '[])
      (check-equal? (peaks (list 5 5 1)) '[])
      (check-equal? (peaks (list 5 5 2)) '[])
      (check-equal? (peaks (list 5 5 6)) '[])
      (check-equal? (peaks (list 5 6 1)) (list 1))
      (check-equal? (peaks (list 5 6 4)) (list 1))
      (check-equal? (peaks (list 6 1 1)) '[])
      (check-equal? (peaks (list 6 1 2)) '[])
      (check-equal? (peaks (list 6 1 5)) '[])
      (check-equal? (peaks (list 6 2 2)) '[])
      (check-equal? (peaks (list 6 2 5)) '[])
      (check-equal? (peaks (list 6 3 2)) '[])
      (check-equal? (peaks (list 6 3 3)) '[])
      (check-equal? (peaks (list 6 3 6)) '[])
      (check-equal? (peaks (list 6 4 3)) '[])
      (check-equal? (peaks (list 6 5 2)) '[])
      (check-equal? (peaks (list 6 5 4)) '[])
      (check-equal? (peaks (list 6 6 4)) '[])
      (check-equal? (peaks (list 1 1 1 4)) '[])
      (check-equal? (peaks (list 1 1 7 7)) '[])
      (check-equal? (peaks (list 1 3 6 5)) (list 2))
      (check-equal? (peaks (list 1 4 7 8)) '[])
      (check-equal? (peaks (list 1 6 6 6)) '[])
      (check-equal? (peaks (list 1 8 1 8)) (list 1))
      (check-equal? (peaks (list 2 2 1 2)) '[])
      (check-equal? (peaks (list 2 3 7 6)) (list 2))
      (check-equal? (peaks (list 2 5 4 5)) (list 1))
      (check-equal? (peaks (list 2 7 1 2)) (list 1))
      (check-equal? (peaks (list 2 7 2 6)) (list 1))
      (check-equal? (peaks (list 2 7 5 3)) (list 1))
      (check-equal? (peaks (list 2 7 7 6)) '[])
      (check-equal? (peaks (list 3 1 2 5)) '[])
      (check-equal? (peaks (list 3 3 4 2)) (list 2))
      (check-equal? (peaks (list 3 3 7 8)) '[])
      (check-equal? (peaks (list 3 4 2 4)) (list 1))
      (check-equal? (peaks (list 3 4 5 4)) (list 2))
      (check-equal? (peaks (list 3 4 7 6)) (list 2))
      (check-equal? (peaks (list 3 5 5 3)) '[])
      (check-equal? (peaks (list 3 6 4 7)) (list 1))
      (check-equal? (peaks (list 3 8 5 5)) (list 1))
      (check-equal? (peaks (list 4 2 4 3)) (list 2))
      (check-equal? (peaks (list 4 2 6 8)) '[])
      (check-equal? (peaks (list 4 3 3 8)) '[])
      (check-equal? (peaks (list 4 4 8 7)) (list 2))
      (check-equal? (peaks (list 4 5 1 1)) (list 1))
      (check-equal? (peaks (list 4 6 1 7)) (list 1))
      (check-equal? (peaks (list 4 6 2 1)) (list 1))
      (check-equal? (peaks (list 4 6 2 2)) (list 1))
      (check-equal? (peaks (list 5 1 7 6)) (list 2))
      (check-equal? (peaks (list 5 3 2 2)) '[])
      (check-equal? (peaks (list 5 3 6 3)) (list 2))
      (check-equal? (peaks (list 5 3 8 3)) (list 2))
      (check-equal? (peaks (list 5 4 4 6)) '[])
      (check-equal? (peaks (list 5 4 4 8)) '[])
      (check-equal? (peaks (list 5 5 1 7)) '[])
      (check-equal? (peaks (list 5 5 8 2)) (list 2))
      (check-equal? (peaks (list 5 6 7 4)) (list 2))
      (check-equal? (peaks (list 5 7 4 3)) (list 1))
      (check-equal? (peaks (list 5 8 7 8)) (list 1))
      (check-equal? (peaks (list 6 2 8 6)) (list 2))

      )
