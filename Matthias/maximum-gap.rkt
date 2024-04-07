#lang racket

(require "../testing.rkt")

(module general racket ;; constraints collected from problem statememt 
  (provide LENGTH LIMIT mg/c)

  (define LENGTH 105)
  (define LIMIT  (+ 109 1))

  (define lon/c
    (and/c (flat-named-contract 'small (listof (integer-in 0 LIMIT)))
           (flat-named-contract 'short (compose (<=/c LENGTH) length))))

  ;; You must write an algorithm that runs in linear time and uses linear extra space.
  ;; Mine uses linear space in the size of the numbers on the given list, which by the
  ;; constraints is linear in terms of the size of the given list. 
  (define mg/c (-> lon/c natural?)))

(def-module module% mg general)

;; ---------------------------------------------------------------------------------------------------
(module% base-not-linear
  (define from '[])
  (define rationale "linearity constraint is bad: the most natural approach is to make a mask")
  
  (define/contract [mg l] mg/c
    (cond
      [(or (empty? l) (empty? (rest l))) 0]
      [else
       (define M (make-mask l))
       (define L (vector-length M))
       (for/fold ([left 0] [mg 0] #:result (max mg (- L left 1))) ([i (in-range 1 L 1)])
         (cond
           [(zero? (vector-ref M i)) (values left mg)]
           [else                     (values i (max (- i left) mg))]))]))

  #; {[Listof N] -> [Vector (U 01 )]}
  (define (make-mask l)
    (define max-of (apply max l))
    (define min-of (apply min l))
    (define width  (add1 (- max-of min-of)))
    (define M (make-vector width 0))
    (for ([x (in-list l)]) (vector-set! M (- x min-of) 1))
    M))

;; ---------------------------------------------------------------------------------------------------
(module% stateful-interval
  (define from '[])
  (define rationale "linearity constraint is bad: the most natural approach is to make a mask")
  
  (define/contract [mg l] mg/c
    (cond
      [(or (empty? l) (empty? (rest l))) 0]
      [else
       (define largest   (apply max l))
       (define smallest  (apply min l))
       (define intervals (populate-intervals l largest smallest))
       (determine-maximum-gap intervals smallest)]))

  #; {type Intervals = [Vectorof Interval] || size is n+1 intervals}
  #; {type Interval  = (U False [List Natural Natural])}
  
  #; {[Vector False] Natural -> [Vector Interval]}
  ;; create intervals of size `(/ (- largest smallest) (length l))`
  ;; place each number x into the interval `(- x smallest)` to actually create the interval 
  (define (populate-intervals l largest smallest)
    (define total-width    (- largest smallest))
    (define interval-width (max (quotient total-width (length l)) 1))
    (define intervals      (make-vector (add1 (quotient total-width interval-width)) #false))
    (for ([x (in-list l)])
      (define i (quotient (- x smallest) interval-width))
      (define new-interval 
        (match (vector-ref intervals i)
          [#false            (list x x)]
          [(list left right) (list (min left x) (max x right))]))
      (vector-set! intervals i new-interval))
    intervals)

  #; {[Vector Interval] -> Natural}
  (define (determine-maximum-gap P0 min-of)
    (for/fold ([max-gap 0] [previous-right min-of] #:result max-gap) ([I (in-vector P0)])
      (match I
        [#false            (values max-gap previous-right)]
        [(list left right) (values (max #;(- right left) (- left previous-right) max-gap) right)]))))

;; ---------------------------------------------------------------------------------------------------
(module% inline
  (define from `[[stateful-interval ,INLINE]])
  (define rationale "inline all functions and avoid parameter passing")
  
  (define/contract [mg l] mg/c
    (cond
      [(or (empty? l) (empty? (rest l))) 0]
      [else
       (define largest        (apply max l))
       (define smallest       (apply min l))
       (define total-width    (- largest smallest))
       (define interval-width (max (quotient total-width (length l)) 1))
       #; {type Intervals = [Vectorof Interval] || size is n+1 intervals}
       #; {type Interval  = (U False [List Natural Natural])}
       (define intervals      (make-vector (add1 (quotient total-width interval-width)) #false))
       (for ([x (in-list l)])
         (define i (quotient (- x smallest) interval-width))
         (define new-interval 
           (match (vector-ref intervals i)
             [#false            (list x x)]
             [(list left right) (list (min left x) (max x right))]))
         (vector-set! intervals i new-interval))
       (for/fold ([max-gap 0] [previous-right smallest] #:result max-gap) ([I (in-vector intervals)])
         (match I
           [#false            (values max-gap previous-right)]
           [(list left right) (values (max (- left previous-right) max-gap) right)]))])))

;; ---------------------------------------------------------------------------------------------------
(module% c
  (define from `[[inline ,IMPERATIVE]])
  (define rationale "inline all functions and avoid parameter passing")
  
  (define/contract [mg l] mg/c
    (cond
      [(or (empty? l) (empty? (rest l))) 0]
      [else
       (define largest        (apply max l))
       (define smallest       (apply min l))
       (define total-width    (- largest smallest))
       (define interval-width (max (quotient total-width (length l)) 1))
       #; {type Intervals = [Vectorof Interval] || size is n+1 intervals}
       #; {type Interval  = (U False [List Natural Natural])}
       (define intervals      (make-vector (add1 (quotient total-width interval-width)) #false))
       (for ([x (in-list l)])
         (define i (quotient (- x smallest) interval-width))
         (define new-interval 
           (match (vector-ref intervals i)
             [#false            (list x x)]
             [(list left right) (list (min left x) (max x right))]))
         (vector-set! intervals i new-interval))

       (define max-gap 0)
       (define previous-right smallest)

       
       (for ([I (in-vector intervals)])
         (match I
           [#false (void)]
           [(list left right)
            (set! max-gap (max (- left previous-right) max-gap))
            (set! previous-right right)]))

       max-gap])))

;; ---------------------------------------------------------------------------------------------------
(module% object
  (define from `[[stateful-interval ,CLASS]])
  (define rationale "linearity constraint is bad: the most natural approach is to make a mask")
  
  (define/contract [mg l] mg/c
    (cond
      [(or (empty? l) (empty? (rest l))) 0]
      [else (send (new interval% [l l]) determine-maximum-gap)]))

  (define interval%
    (class object% (init-field l)
      (field
       [largest        (apply max l)]
       [smallest       (apply min l)]
       [total-width    (- largest smallest)]
       [interval-width (max (quotient total-width (length l)) 1)]
       
       #; {type Intervals = [Vectorof Interval] || size is total-width/interval-width + 1 intervals}
       #; {type Interval  = (U False [List Natural Natural])}
       [intervals      (make-vector (add1 (quotient total-width interval-width)) #false)])
      
      ;; place each number x into the interval `(- x smallest)` to actually create the intervals
      (define/private (populate-intervals)
        (for ([x (in-list l)])
          (define i (quotient (- x smallest) interval-width))
          (define new-interval 
            (match (vector-ref intervals i)
              [#false            (list x x)]
              [(list left right) (list (min left x) (max x right))]))
          (vector-set! intervals i new-interval))
        intervals)

      (define/public (determine-maximum-gap)
        (for/fold ([max-gap 0] [previous-right smallest] #:result max-gap) ([I (in-vector intervals)])
          (match I
            [#false            (values max-gap previous-right)]
            [(list left right) (values (max (- left previous-right) max-gap) right)])))
      
      (super-new)
      (populate-intervals))))
    

;; ---------------------------------------------------------------------------------------------------
(test mg
      in
      stateful-interval object inline c
      [#:show-graph #true]
      with
      
      (check-equal? (mg (list 1 0 1 0 1 0 1 0 1 0)) 1 "a")

      ;; corrected from llms/ source 
      (check-exn #px"small" (λ () (mg (list 1 2 4 8 16 32 64 128 256 512))) "b")
      
      (check-equal? (mg (list 0 0 0 0 0 0 0 0 0 1)) 1 "c")
      (check-equal? (mg (list 0)) 0 "d")
      (check-equal? (mg (list 1 1 1 1 1 1 1 1 1 1)) 0 "e")
      (check-equal? (mg (list 1 3 6 9 12 15 18 21 24 27)) 3 "f")
      (check-equal? (mg (list 9 8 7 6 5 4 3 2 1 0)) 1 "g")
      (check-equal? (mg (list 1 2 3 4 5 6 7 8 9 10)) 1 "h")

      ;; corrected from llms/ source
      (check-exn #px"small" (λ () (mg (list 1000 2000 3000 4000 5000 6000 7000 8000 9000 10000)) "i"))
      
      (check-equal? (mg (list 10 20 30 40 50 60 70 80 90 100)) 10 "j")
      (check-equal? (mg (list 0 0 0 1 1 1 1 1 1 1)) 1 "k")

      ;; corrected from llms/ source
      (check-exn #px"small" (λ () (mg (list 10 100 1000 10000 100000 1000000 10000000))) "l")

      ;; corrected from llms/ source
      (check-exn #px"small" (λ () (mg (list 100 200 300 400 500 600 700 800 900 1000))) "m")
      (check-equal? (mg (list 3 6 9 1)) 3 "n")

      ;; corrected from llms/ source
      (check-exn #px"small" (λ () (mg (list 1 10 100 1000 10000 100000 1000000))) "o")
      (check-equal? (mg (list 0 0 0 0 0 0 0 0 0 0)) 0 "p")
      (check-equal? (mg (list )) 0 "q")
      (check-equal? (mg (list 1 2 3 5 8 13 21 34 55 89)) 34 "r")
      (check-equal? (mg (list 5 10 15 20 25 30 35 40 45 50)) 5 "s")
      (check-equal? (mg (list 1 1 1 1 1 2 2 2 2 2)) 1 "t")
      (check-equal? (mg (list 10)) 0 "u"))