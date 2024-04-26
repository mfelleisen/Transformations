#lang racket

;; this one is so easy, an F I student can do it just before Thanksgiving.
;; After some work, this is coooool. I wa sable to eliminate the inner loop. 

;; "I have no clue why this is called 'count test devices'"
;; if someone explained this, I might be able to turn it into an example for HtDP/V

(require "../testing.rkt")

(module general racket ;; constraints collected from problem statememt 
  (provide ctd/c)

  (define LENGTH 100)
  (define %/c (integer-in 0 100))

  (define lon/c
    (and/c (flat-named-contract 'small (listof %/c))
           (flat-named-contract 'short (compose (<=/c LENGTH) length))))

  ;; You are given a 0-indexed integer array batteryPercentages having
  ;; length n, denoting the battery percentages of n 0-indexed devices.

  ;; Your task is to test each device i in order from 0 to n - 1, by
  ;; performing the following test operations:

  ;; If batteryPercentages[i] is greater than 0:
  ;; Increment the count of tested devices.
  ;; Decrease the battery percentage of all devices with indices j in
  ;; the range [i + 1, n - 1] by 1, ensuring their battery percentage
  ;; never goes below 0, i.e, batteryPercentages[j] = max(0,
  ;; batteryPercentages[j] - 1).

  ;; Move to the next device.
  ;; Otherwise, move to the next device without performing any test.
  ;; Return an integer denoting the number of devices that will be
  ;; tested after performing the test operations in order.

  (define ctd/c (-> lon/c natural?)))

(def-module module% ctd general)

;; ---------------------------------------------------------------------------------------------------
(module% base-bsl
  (define from '[])
  (define rationale "this is a simple example of generative recursion -- change (rest .) to conform")

  (define/contract (ctd percentages) ctd/c
    (cond
      [(empty? percentages) 0]
      [else
       (define one (first percentages))
       (if (> one 0)
           (add1 (ctd (decrement (rest percentages))))
           (ctd (rest percentages)))]))
           
  #; {[Listof %] -> [Listof %]}
  (define (decrement percentages)
    (cond
      [(empty? percentages) '()]
      [else (cons (max 0 (sub1 (first percentages))) (decrement (rest percentages)))])))

;; ---------------------------------------------------------------------------------------------------
(module% plain-racket
  (define from `[[base-bsl ,LETLOOP]])
  (define rationale "this is Racket from a beginner who knows about `let loop`")

  (define/contract (ctd percentages0) ctd/c
    (let ctd ([percentages percentages0])
      (cond
        [(empty? percentages) 0]
        [else
         (define one (first percentages))
         (if (> one 0)
             (add1 (ctd (decrement (rest percentages))))
             (ctd (rest percentages)))])))

  #; {[Listof %] -> [Listof %]}
  (define (decrement percentages)
    (cond
      [(empty? percentages) '()]
      [else (cons (max 0 (sub1 (first percentages))) (decrement (rest percentages)))])))

;; ---------------------------------------------------------------------------------------------------
(module% ho-racket 
  (define from `[[plain-racket ,HO]])
  (define rationale "time to use `map` for `decrement`")

  (define/contract (ctd percentages0) ctd/c
    (let ctd ([percentages percentages0])
      (cond
        [(empty? percentages) 0]
        [else
         (define one (first percentages))
         (if (> one 0)
             (add1 (ctd (decrement (rest percentages))))
             (ctd (rest percentages)))])))

  #; {[Listof %] -> [Listof %]}
  (define (decrement percentages)
    (map (λ (x) (max 0 (- x 1))) percentages)))

;; ---------------------------------------------------------------------------------------------------
(module% inline-racket 
  (define from `[[ho-racket ,INLINE]])
  (define rationale "it is okay to inline `decrement` for Leet code")

  (define/contract (ctd percentages0) ctd/c
    (let ctd ([percentages percentages0])
      (cond
        [(empty? percentages) 0]
        [else
         (define one (first percentages))
         (if (> one 0)
             (add1 (ctd (map (λ (x) (max 0 (- x 1))) (rest percentages))))
             (ctd (rest percentages)))]))))

;; ---------------------------------------------------------------------------------------------------
(module% accu-racket 
  (define from `[[inline-racket ,ACCUMULATOR]])
  (define rationale "a beginner may use an accumulator for counting because it feels 'natural'")

  (define/contract (ctd percentages0) ctd/c
    (let ctd ([percentages percentages0] [count 0])
      (cond
        [(empty? percentages) count]
        [else
         (define one (first percentages))
         (if (> one 0)
             (ctd (map (λ (x) (max 0 (- x 1))) (rest percentages)) (add1 count))
             (ctd (rest percentages) count))]))))

;; ---------------------------------------------------------------------------------------------------
(module% accu-2-racket 
  (define from `[[accu-racket ,ACCUMULATOR]])
  (define rationale "a strong beginner may realize that the -1 ops can be accumulated; think Wand 82")

  (define/contract (ctd percentages0) ctd/c
    (let ctd ([percentages percentages0] [count 0] [delta 0])
      (cond
        [(empty? percentages) count]
        [else
         (define one (first percentages))
         (if (> (- one delta) 0)
             (ctd (rest percentages) (add1 count) (add1 delta))
             (ctd (rest percentages) count        delta))]))))

;; ---------------------------------------------------------------------------------------------------
(module% for-racket 
  (define from `[[accu-2-racket ,HO]])
  (define rationale  "the let-loop can now be turned into a for/fold")

  (define/contract (ctd percentages0) ctd/c
    (for/fold ([count 0] [delta 0] #:result count) ([x (in-list percentages0)])
      (if (> (- x delta) 0)
          (values (add1 count) (add1 delta))
          (values count        delta)))))

;; ---------------------------------------------------------------------------------------------------
(module% base-isl+
  (define from `[[base-bsl ,HO]])
  (define rationale "a beginner got to know ISL+")

  (define/contract (ctd percentages) ctd/c
    (cond
      [(empty? percentages) 0]
      [else
       (define one (first percentages))
       (if (> one 0)
           (add1 (ctd (decrement (rest percentages))))
           (ctd (rest percentages)))]))
           
  #; {[Listof %] -> [Listof %]}
  (define (decrement percentages)
    (map (λ (x) (max 0 (- x 1))) percentages)))

;; ---------------------------------------------------------------------------------------------------
(module% base-bsl-accu
  (define from `[[base-bsl ,ACCUMULATOR]])
  (define rationale "a beginner using accumulators with generative recursion")

  (define/contract (ctd percentages) ctd/c
    (ctd/bsl percentages 0))

  #; {[Listof %] N -> N}
  (define (ctd/bsl percentages counted#)
    (cond
      [(empty? percentages) counted#]
      [else
       (define one (first percentages))
       (if (> one 0)
           (ctd/bsl (decrement (rest percentages)) (add1 counted#))
           (ctd/bsl (rest percentages) counted#))]))

  #; {[Listof %] -> [Listof %]}
  (define (decrement percentages)
    (cond
      [(empty? percentages) '()]
      [else (cons (max 0 (sub1 (first percentages))) (decrement (rest percentages)))])))

;; ---------------------------------------------------------------------------------------------------
(module% base-isl+-accu
  (define from `[[base-bsl-accu ,HO] [base-isl+ ,ACCUMULATOR]])
  (define rationale "a beginner got to know ISL+")

  (define/contract (ctd percentages) ctd/c
    (ctd/bsl percentages 0))

  #; {[Listof %] N -> N}
  (define (ctd/bsl percentages counted#)
    (cond
      [(empty? percentages) counted#]
      [else
       (define one (first percentages))
       (if (> one 0)
           (ctd/bsl (decrement (rest percentages)) (add1 counted#))
           (ctd/bsl (rest percentages) counted#))]))

  #; {[Listof %] -> [Listof %]}
  (define (decrement percentages)
    (map (λ (x) (max 0 (- x 1))) percentages)))

;; ---------------------------------------------------------------------------------------------------
(module% base-isl+-2-accu
  (define from `[[base-isl+-accu ,ACCUMULATOR]])
  (define rationale "a strong beginner may realize that the -1 ops can be accumulated; think Wand 82")

  (define/contract (ctd percentages) ctd/c
    (ctd/bsl percentages 0 0))

  #; {[Listof %] N -> N}
  (define (ctd/bsl percentages counted# delta)
    (cond
      [(empty? percentages) counted#]
      [else
       (define one (first percentages))
       (if (> (- one delta) 0)
           (ctd/bsl (rest percentages) (add1 counted#) (add1 delta))
           (ctd/bsl (rest percentages) counted#        delta))])))

;; ---------------------------------------------------------------------------------------------------
(test ctd
      in
      base-bsl  base-bsl-accu base-isl+ base-isl+-accu base-isl+-2-accu
      plain-racket ho-racket inline-racket accu-racket accu-2-racket 
      for-racket
      [#:show-graph #true]
      with
      (check-equal? (ctd (list 0 100)) 1)

      (check-equal? (ctd (list 1 1 2 1 3)) 3)
      (check-equal? (ctd (list 0 1 2)) 2)
      (check-equal? (ctd (list 0)) 0)
      (check-equal? (ctd (list 1)) 1)
      (check-equal? (ctd (list 0 0)) 0)
      (check-equal? (ctd (list 0 1)) 1)
      (check-equal? (ctd (list 0 2)) 1)
      (check-equal? (ctd (list 1 0)) 1)
      (check-equal? (ctd (list 1 2)) 2)
      (check-equal? (ctd (list 2 1)) 1)
      (check-equal? (ctd (list 2 2)) 2)
      (check-equal? (ctd (list 0 0 1)) 1)
      (check-equal? (ctd (list 0 0 2)) 1)
      (check-equal? (ctd (list 1 1 0)) 1)
      (check-equal? (ctd (list 1 2 0)) 2)
      (check-equal? (ctd (list 1 3 1)) 2)
      (check-equal? (ctd (list 2 0 1)) 1)
      (check-equal? (ctd (list 2 2 0)) 2)
      (check-equal? (ctd (list 2 2 2)) 2)
      (check-equal? (ctd (list 3 0 3)) 2)
      (check-equal? (ctd (list 3 3 1)) 2)
      (check-equal? (ctd (list 3 3 3)) 3)
      (check-equal? (ctd (list 0 2 1 4)) 2)
      (check-equal? (ctd (list 1 4 4 1)) 3)
      (check-equal? (ctd (list 3 1 2 0)) 2)
      (check-equal? (ctd (list 3 2 1 1)) 2)
      (check-equal? (ctd (list 3 2 1 3)) 3)
      (check-equal? (ctd (list 4 1 4 4)) 3)
      (check-equal? (ctd (list 4 2 0 1)) 2)
      (check-equal? (ctd (list 4 2 1 3)) 3)
      (check-equal? (ctd (list 4 4 4 2)) 3)
      (check-equal? (ctd (list 0 3 1 3 5)) 3)
      (check-equal? (ctd (list 0 4 2 5 3)) 3)
      (check-equal? (ctd (list 0 5 4 2 0)) 2)
      (check-equal? (ctd (list 2 2 3 0 2)) 3)
      (check-equal? (ctd (list 2 3 5 0 1)) 3)
      (check-equal? (ctd (list 2 4 5 2 0)) 3)
      (check-equal? (ctd (list 4 3 3 5 4)) 4)
      (check-equal? (ctd (list 5 4 1 0 3)) 3)
      (check-equal? (ctd (list 5 5 5 2 0)) 3)
      (check-equal? (ctd (list 0 2 4 3 0 2)) 3)
      (check-equal? (ctd (list 0 4 5 3 3 2)) 3)
      (check-equal? (ctd (list 1 3 1 5 4 5)) 5)
      (check-equal? (ctd (list 1 6 0 3 3 6)) 4)
      (check-equal? (ctd (list 3 1 3 5 2 0)) 3)
      (check-equal? (ctd (list 3 2 6 2 6 0)) 4)
      (check-equal? (ctd (list 4 1 5 3 5 2)) 4)
      (check-equal? (ctd (list 4 3 3 2 4 3)) 4)
      (check-equal? (ctd (list 4 5 2 3 6 2)) 4)
      (check-equal? (ctd (list 5 1 1 2 1 4)) 3)
      (check-equal? (ctd (list 5 1 6 6 3 6)) 4)
      (check-equal? (ctd (list 6 1 5 1 4 5)) 4)
      (check-equal? (ctd (list 6 2 2 3 4 6)) 5)
      (check-equal? (ctd (list 6 2 3 0 2 0)) 3)
      (check-equal? (ctd (list 1 0 6 3 6 3 1)) 4)
      (check-equal? (ctd (list 2 1 7 3 0 3 3)) 3)
      (check-equal? (ctd (list 2 3 7 0 6 4 4)) 4)
      (check-equal? (ctd (list 2 5 2 4 2 1 3)) 3)
      (check-equal? (ctd (list 2 5 2 7 6 5 5)) 5)
      (check-equal? (ctd (list 4 2 6 4 7 6 7)) 7)
      (check-equal? (ctd (list 4 2 6 6 3 3 7)) 5)
      (check-equal? (ctd (list 4 4 3 0 2 6 6)) 5)
      (check-equal? (ctd (list 5 2 2 3 4 6 6)) 6)
      (check-equal? (ctd (list 5 4 6 0 7 2 2)) 4)
      (check-equal? (ctd (list 6 6 7 0 1 7 2)) 4)
      (check-equal? (ctd (list 0 5 1 4 5 0 4 8)) 5)
      (check-equal? (ctd (list 1 0 7 0 7 4 5 7)) 6)
      (check-equal? (ctd (list 2 5 3 4 4 8 6 5)) 6)
      (check-equal? (ctd (list 2 6 3 4 5 6 2 6)) 6)
      (check-equal? (ctd (list 4 5 2 1 3 7 3 5)) 5)
      (check-equal? (ctd (list 6 5 4 8 6 8 3 6)) 6)
      (check-equal? (ctd (list 7 4 0 8 5 5 2 0)) 5)
      (check-equal? (ctd (list 7 5 3 2 3 5 8 6)) 6)
      (check-equal? (ctd (list 8 0 4 3 2 6 6 1)) 5)
      (check-equal? (ctd (list 8 3 0 1 0 8 6 8)) 5)
      (check-equal? (ctd (list 8 6 7 1 0 1 3 7)) 4)
      (check-equal? (ctd (list 0 6 8 8 0 1 2 3 4)) 4)
      (check-equal? (ctd (list 2 7 9 7 2 9 0 3 9)) 6)
      (check-equal? (ctd (list 8 1 9 8 5 3 4 4 1)) 4)
      (check-equal? (ctd (list 8 4 0 1 1 6 5 3 5)) 5)
      (check-equal? (ctd (list 8 4 1 5 8 5 8 7 9)) 8)
      (check-equal? (ctd (list 8 4 9 8 9 0 0 4 9)) 6)
      (check-equal? (ctd (list 8 9 4 4 1 9 8 9 1)) 7)
      (check-equal? (ctd (list 2 5 8 9 1 5 10 9 6 3)) 7)
      (check-equal? (ctd (list 2 6 5 4 1 5 3 3 3 9)) 6)
      (check-equal? (ctd (list 7 7 7 3 6 6 4 3 5 10)) 6)
      (check-equal? (ctd (list 9 3 10 1 8 2 4 3 3 0)) 4)
      (check-equal? (ctd (list 10 10 2 0 2 7 6 7 10 4)) 6)
      (check-equal? (ctd (list 0 8 7 9 4 10 4 3 7 11 7)) 7)
      (check-equal? (ctd (list 1 2 3 5 6 11 3 2 11 0 8)) 8)
      (check-equal? (ctd (list 5 10 4 10 10 6 8 1 8 10 3)) 9)
      (check-equal? (ctd (list 7 10 2 7 11 8 11 4 1 4 5)) 6)
      (check-equal? (ctd (list 7 11 0 4 1 10 5 3 2 0 2)) 5)
      (check-equal? (ctd (list 8 8 1 8 6 2 5 2 8 5 6)) 6)
      (check-equal? (ctd (list 8 9 10 10 1 5 4 6 7 2 4)) 7)
      (check-equal? (ctd (list 9 9 2 3 2 2 9 6 11 1 10)) 7)
      (check-equal? (ctd (list 10 0 6 2 6 6 11 1 8 10 5)) 7)
      (check-equal? (ctd (list 1 4 7 2 12 8 1 11 5 10 2 3)) 7)
      (check-equal? (ctd (list 2 5 4 4 9 6 10 0 11 8 2 10)) 9)
      (check-equal? (ctd (list 4 11 9 8 9 11 11 5 11 6 12 11)) 10))
