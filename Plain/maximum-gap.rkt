#lang racket

(require "../testing-2.rkt")

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai6

(define (mg-ai6 nums)
  ;; Check if the list contains less than two elements, return 0 if so.
  (if (< (length nums) 2)
      0
      (let* ((min-val (apply min nums)) ; Find minimum value in the list.
             (max-val (apply max nums)) ; Find maximum value in the list.
             (length (length nums))
             (bucket-size (max 1 (quotient (- max-val min-val) (- length 1)))) ; Calculate the size of each bucket.
             (bucket-num (+ 1 (quotient (- max-val min-val) bucket-size))) ; Calculate the number of buckets.
             ;; Initialize buckets with pairs [min, max] initialized to [inf, -inf].
             (buckets (make-vector bucket-num (list +inf.0 -inf.0))))
        ;; Fill the buckets with min and max values.
        (for-each (lambda (num)
                    (let ((idx (quotient (- num min-val) bucket-size)))
                      (vector-set! buckets idx 
                                   (list (min (first (vector-ref buckets idx)) num)
                                         (max (second (vector-ref buckets idx)) num)))))
                  nums)
        ;; Calculate the maximum gap.
        (let loop ((i 1) (prev-max (second (vector-ref buckets 0))) (max-gap 0))
          (if (< i bucket-num)
              (let ((bucket (vector-ref buckets i)))
                (if (= (first bucket) +inf.0) ; If the bucket is empty, skip it.
                    (loop (+ i 1) prev-max max-gap)
                    (let ((gap (- (first bucket) prev-max))) ; Calculate the gap.
                      (loop (+ i 1) (second bucket) (max max-gap gap)))))
              (inexact->exact max-gap))))))

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai5

(define (mg-ai5 nums)
  ;; If the list has less than 2 elements, return 0.
  (if (< (length nums) 2)
      0
      (let* ((min-val (apply min nums)) ; Find the minimum value in nums.
             (max-val (apply max nums)) ; Find the maximum value in nums.
             (length (length nums))
             (bucket-size (max 1 (quotient (- max-val min-val) (- length 1)))) ; Determine the size of each bucket.
             (bucket-num (+ 1 (quotient (- max-val min-val) bucket-size))) ; Calculate the number of buckets needed.
             ;; Initialize buckets with pairs of [Infinity, -Infinity] to represent [min, max] values in each bucket.
             (buckets (make-vector bucket-num (list +inf.0 -inf.0))))
        ;; Fill the buckets with the min and max value found in each.
        (for-each (lambda (num)
                    (let* ((idx (quotient (- num min-val) bucket-size))
                           (bucket (vector-ref buckets idx))
                           (new-min (min (first bucket) num))
                           (new-max (max (second bucket) num)))
                      (vector-set! buckets idx (list new-min new-max))))
                  nums)
        ;; Calculate the maximum gap by iterating through the buckets.
        (let loop ((i 0) (prev-max min-val) (max-gap 0))
          (if (< i bucket-num)
              (let* ((bucket (vector-ref buckets i))
                     (current-min (first bucket))
                     (current-max (second bucket)))
                ;; Skip empty buckets.
                (if (= current-min +inf.0)
                    (loop (+ i 1) prev-max max-gap)
                    (loop (+ i 1) 
                          current-max 
                          (max max-gap (- current-min prev-max)))))
              (inexact->exact max-gap))))))

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai4

(define (mg-ai4 nums)
  ;; If there are less than two elements, return 0 as per the problem statement.
  (if (< (length nums) 2)
      0
      (let* ((min-val (apply min nums))  ;; Find the minimum value in the list.
             (max-val (apply max nums))  ;; Find the maximum value in the list.
             (length (length nums))  ;; Calculate the length of the input list.
             ;; Calculate the size of each bucket. Avoid division by zero by ensuring bucket size is at least 1.
             (bucket-size (max 1 (quotient (- max-val min-val) (- length 1))))
             ;; Calculate the number of buckets needed.
             (bucket-num (+ 1 (quotient (- max-val min-val) bucket-size)))
             ;; Initialize buckets with lists of [min, max] values, starting with [inf, -inf].
             (buckets (make-vector bucket-num (list +inf.0 -inf.0))))
        ;; Fill the buckets with the minimum and maximum values that fall into each bucket.
        (for-each (lambda (num)
                    (let* ((idx (quotient (- num min-val) bucket-size))
                           (bucket (vector-ref buckets idx))
                           (new-min (min (first bucket) num))
                           (new-max (max (second bucket) num)))
                      (vector-set! buckets idx (list new-min new-max))))
                  nums)
        ;; Compute the maximum gap by iterating through the buckets.
        (let loop ((i 0)
                   (prev-max min-val)
                   (max-gap 0))
          (if (< i bucket-num)
              (let ((bucket (vector-ref buckets i)))
                (if (= (first bucket) +inf.0)  ;; Skip empty buckets.
                    (loop (+ i 1) prev-max max-gap)
                    (let* ((current-min (first bucket))
                           (current-max (second bucket))
                           (current-gap (- current-min prev-max)))
                      (loop (+ i 1) current-max (max max-gap current-gap)))))
              (inexact->exact max-gap))))))

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai3 

(define (mg-ai3 nums)
  ;; Check if the list contains less than two elements.
  (if (< (length nums) 2)
      0
      (let* ((min-val (apply min nums))             ; Find the minimum value in the list.
             (max-val (apply max nums))             ; Find the maximum value in the list.
             (length (length nums))                 ; Get the length of the list.
             (bucket-size (max 1 (quotient (- max-val min-val) (- length 1)))) ; Calculate the size of each bucket.
             (bucket-num (+ 1 (quotient (- max-val min-val) bucket-size))) ; Calculate the number of buckets.
             (buckets (make-vector bucket-num (list +inf.0 -inf.0)))) ; Initialize buckets with positive and negative infinity.
        ;; Fill the buckets with min and max values.
        (for-each (lambda (num)
                    (let ((idx (quotient (- num min-val) bucket-size)))
                      (vector-set! buckets idx (list (min (first (vector-ref buckets idx)) num)
                                                     (max (second (vector-ref buckets idx)) num)))))
                  nums)
        ;; Calculate the maximum gap.
        (let loop ((i 1) (prev-max (second (vector-ref buckets 0))) (max-gap 0))
          (if (< i bucket-num)
              (let ((bucket (vector-ref buckets i)))
                (if (= (first bucket) +inf.0) ; Skip the bucket if it's empty.
                    (loop (+ i 1) prev-max max-gap)
                    (let ((gap (- (first bucket) prev-max))) ; Calculate the gap between buckets.
                      (loop (+ i 1) (second bucket) (max max-gap gap)))))
              (inexact->exact max-gap))))))

;; ---------------------------------------------------------------------------------------------------
;; MODULE a12

(define (mg-ai2 nums)
  ;; Check if the list has less than two elements
  (if (< (length nums) 2)
      0
      (let* ((min-val (apply min nums)) ;; Find the minimum value in nums
             (max-val (apply max nums)) ;; Find the maximum value in nums
             (length (length nums))
             (bucket-size (max 1 (quotient (- max-val min-val) (- length 1)))) ;; Calculate the size of each bucket
             (bucket-num (+ 1 (quotient (- max-val min-val) bucket-size))) ;; Calculate the number of buckets needed
             ;; Initialize buckets with pairs of positive and negative infinity to represent the min and max of each bucket
             (buckets (make-vector bucket-num (list +inf.0 -inf.0))))
        ;; Populate the buckets with the min and max values
        (for-each (lambda (num)
                    (let ((idx (quotient (- num min-val) bucket-size)))
                      (vector-set! buckets idx 
                                   (list (min (first (vector-ref buckets idx)) num)
                                         (max (second (vector-ref buckets idx)) num)))))
                  nums)
        ;; Calculate the maximum gap
        (let loop ((i 1) (prev-max (second (vector-ref buckets 0))) (max-gap 0))
          (if (< i bucket-num)
              (let ((bucket (vector-ref buckets i)))
                (if (= (first bucket) +inf.0) ;; If the bucket is empty, continue to the next
                    (loop (+ i 1) prev-max max-gap)
                    (let ((gap (- (first bucket) prev-max))) ;; Calculate the gap between the current and previous non-empty buckets
                      (loop (+ i 1) (second bucket) (max max-gap gap)))))
              (inexact->exact max-gap))))))

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai1 

(define (mg-ai1 nums)
  ;; Check if the list has less than 2 elements, return 0 in that case.
  (if (< (length nums) 2)
      0
      (let* (;; Calculate the min and max values of the list.
             (min-val (apply min nums))
             (max-val (apply max nums))
             ;; Calculate the length of the list.
             (length (length nums))
             ;; Calculate the size of each bucket. The max function ensures the bucket size is at least 1.
             (bucket-size (max 1 (quotient (- max-val min-val) (- length 1))))
             ;; Calculate the number of buckets needed.
             (bucket-num (+ 1 (quotient (- max-val min-val) bucket-size)))
             ;; Initialize the buckets with pairs of infinity and negative infinity for min and max values respectively.
             (buckets (make-vector bucket-num (list +inf.0 -inf.0))))
        ;; Populate the buckets with min and max values.
        (for-each (lambda (num)
                    (let ((idx (quotient (- num min-val) bucket-size)))
                      (vector-set! buckets idx (list (min (car (vector-ref buckets idx)) num)
                                                     (max (cadr (vector-ref buckets idx)) num)))))
                  nums)
        ;; Calculate the maximum gap.
        (let loop ((i 0) (prev-max min-val) (max-gap 0))
          (if (< i bucket-num)
              (let ((bucket (vector-ref buckets i)))
                (if (= (car bucket) +inf.0)  ;; Skip empty buckets.
                    (loop (+ i 1) prev-max max-gap)
                    (let* ((current-min (car bucket))
                           (gap (- current-min prev-max))
                           (new-max-gap (max max-gap gap)))
                      (loop (+ i 1) (cadr bucket) new-max-gap))))
              (inexact->exact max-gap))))))

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai0

(define (mg-ai0 nums)
  ;; Check if the input list has less than two elements, return 0 if true
  (if (< (length nums) 2)
      0
      (let* ((min-val (apply min nums)) ; Find the minimum value in the list
             (max-val (apply max nums)) ; Find the maximum value in the list
             (length (length nums)) ; Get the length of the list
             (bucket-size (max 1 (quotient (- max-val min-val) (- length 1)))) ; Calculate the bucket size
             (bucket-num (add1 (quotient (- max-val min-val) bucket-size))) ; Calculate the number of buckets
             ;; Initialize buckets, each containing a pair of [min inf, max -inf] to track min and max in each bucket
             (buckets (make-vector bucket-num (list +inf.0 -inf.0))))
        ;; Populate the buckets with the min and max values of elements that fall into them
        (for-each (lambda (num)
                    (let ((idx (quotient (- num min-val) bucket-size)))
                      (vector-set! buckets idx 
                                   (list (min (first (vector-ref buckets idx)) num) 
                                         (max (second (vector-ref buckets idx)) num)))))
                  nums)
        ;; Calculate the maximum gap by iterating through the buckets
        (let loop ((idx 1) (prev-max (second (vector-ref buckets 0))) (max-gap 0))
          (if (= idx bucket-num)
              #; {FIXED code instead of going back to test cases using check-within}
              (inexact->exact max-gap) ; Return the maximum gap found
              (let ((current (vector-ref buckets idx)))
                (if (= (first current) +inf.0) ; Skip empty buckets
                    (loop (add1 idx) prev-max max-gap)
                    (let ((gap (- (first current) prev-max))) ; Calculate the gap between current min and previous max
                      (loop (add1 idx) 
                            (second current) 
                            (max max-gap gap))))))))))


;; ---------------------------------------------------------------------------------------------------
(define LENGTH 105)
(define LIMIT  (+ 109 1))

;; ---------------------------------------------------------------------------------------------------
;; MODULE base-not-linear
  
(define (mg-base-not-linear l) ;; contract  mg/c
  (cond
    [(or (empty? l) (empty? (rest l))) 0]
    [else
     (define M (make-mask-base-not-linear l))
     (define L (vector-length M))
     (for/fold ([left 0] [mg 0] #:result (max mg (- L left 1))) ([i (in-range 1 L 1)])
       (cond
         [(zero? (vector-ref M i)) (values left mg)]
         [else                     (values i (max (- i left) mg))]))]))

#; {[Listof N] -> [Vector (U 01 )]}
(define (make-mask-base-not-linear l)
  (define max-of (apply max l))
  (define min-of (apply min l))
  (define width  (add1 (- max-of min-of)))
  (define M (make-vector width 0))
  (for ([x (in-list l)]) (vector-set! M (- x min-of) 1))
  M)

;; ---------------------------------------------------------------------------------------------------
;; MODULE stateful-interval
  
(define (mg-stateful-interval l) ;; contract  mg/c
  (cond
    [(or (empty? l) (empty? (rest l))) 0]
    [else
     (define largest   (apply max l))
     (define smallest  (apply min l))
     (define intervals (populate-intervals-stateful-interval l largest smallest))
     (determine-maximum-gap-stateful-interval intervals smallest)]))

#; {type Intervals = [Vectorof Interval] || size is n+1 intervals}
#; {type Interval  = (U False [List Natural Natural])}
  
#; {[Vector False] Natural -> [Vector Interval]}
;; create intervals of size `(/ (- largest smallest) (length l))`
;; place each number x into the interval `(- x smallest)` to actually create the interval 
(define (populate-intervals-stateful-interval l largest smallest)
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
(define (determine-maximum-gap-stateful-interval P0 min-of)
  (for/fold ([max-gap 0] [previous-right min-of] #:result max-gap) ([I (in-vector P0)])
    (match I
      [#false            (values max-gap previous-right)]
      [(list left right) (values (max #;(- right left) (- left previous-right) max-gap) right)])))

;; ---------------------------------------------------------------------------------------------------
;; MODULE inline
  
(define (mg-inline l) ;; contract  mg/c
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
         [(list left right) (values (max (- left previous-right) max-gap) right)]))]))

;; ---------------------------------------------------------------------------------------------------
;; MODULE c
  
(define (mg-c l) ;; contract  mg/c
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

     max-gap]))

;; ---------------------------------------------------------------------------------------------------
;; MODULE object
  
(define (mg-HIGH l) ;; contract  mg/c
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
    (populate-intervals)))
    

;; ---------------------------------------------------------------------------------------------------
(test mg
      in
      ai6 ai5 ai4 ai3 ai2 ai1 ai0
      
      stateful-interval HIGH inline c
      [#:show-graph #true]
      with
      
      (check-equal? (mg (list 1 0 1 0 1 0 1 0 1 0)) 1 "a")

      ;; corrected from llms/ source
      #;
      (check-exn #px"small" (λ () (mg (list 1 2 4 8 16 32 64 128 256 512))) "b")
      
      (check-equal? (mg (list 0 0 0 0 0 0 0 0 0 1)) 1 "c")
      (check-equal? (mg (list 0)) 0 "d")
      (check-equal? (mg (list 1 1 1 1 1 1 1 1 1 1)) 0 "e")
      (check-equal? (mg (list 1 3 6 9 12 15 18 21 24 27)) 3 "f")
      (check-equal? (mg (list 9 8 7 6 5 4 3 2 1 0)) 1 "g")
      (check-equal? (mg (list 1 2 3 4 5 6 7 8 9 10)) 1 "h")

      ;; corrected from llms/ source
      #;
      (check-exn #px"small" (λ () (mg (list 1000 2000 3000 4000 5000 6000 7000 8000 9000 10000)) "i"))
      
      (check-equal? (mg (list 10 20 30 40 50 60 70 80 90 100)) 10 "j")
      (check-equal? (mg (list 0 0 0 1 1 1 1 1 1 1)) 1 "k")

      ;; corrected from llms/ source
      #;
      (check-exn #px"small" (λ () (mg (list 10 100 1000 10000 100000 1000000 10000000))) "l")

      ;; corrected from llms/ source
      #;
      (check-exn #px"small" (λ () (mg (list 100 200 300 400 500 600 700 800 900 1000))) "m")
      (check-equal? (mg (list 3 6 9 1)) 3 "n")

      ;; corrected from llms/ source
      #;
      (check-exn #px"small" (λ () (mg (list 1 10 100 1000 10000 100000 1000000))) "o")
      (check-equal? (mg (list 0 0 0 0 0 0 0 0 0 0)) 0 "p")
      (check-equal? (mg (list )) 0 "q")
      (check-equal? (mg (list 1 2 3 5 8 13 21 34 55 89)) 34 "r")
      (check-equal? (mg (list 5 10 15 20 25 30 35 40 45 50)) 5 "s")
      (check-equal? (mg (list 1 1 1 1 1 2 2 2 2 2)) 1 "t")
      (check-equal? (mg (list 10)) 0 "u"))
