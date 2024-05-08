#lang racket

(require "../testing-2.rkt")

;; ---------------------------------------------------------------------------------------------------
(define (ctd-ai0 batteryPercentages)
  ;; A helper function to simulate the testing and battery reduction process.
  (define (test-devices lst tested-count)
    (if (null? lst)
        tested-count  ; If the list is empty, return the count of tested devices.
        (let* ((first (car lst))  ; The battery percentage of the current device.
               (rest (cdr lst))  ; The battery percentages of subsequent devices.
               (is-tested (> first 0)))  ; Check if the current device can be tested.
          (if is-tested
              ;; If the device is tested, reduce the battery of subsequent devices
              ;; and increment the tested count.
              (test-devices (map (lambda (x) (max 0 (- x 1))) rest)
                            (+ tested-count 1))
              ;; If the device is not tested, just proceed to the next device.
              (test-devices rest tested-count)))))
  ;; Start the recursive testing process with the full list and an initial count of 0.
  (test-devices batteryPercentages 0))

;; ---------------------------------------------------------------------------------------------------
;; MODULE base-bsl

(define (ctd-base-bsl percentages) ;; contract  ctd/c
    (cond
      [(empty? percentages) 0]
      [else
       (define one (first percentages))
       (if (> one 0)
           (add1 (ctd-base-bsl (decrement-base-bsl (rest percentages))))
           (ctd-base-bsl (rest percentages)))]))
           
  #; {[Listof %] -> [Listof %]}
(define (decrement-base-bsl percentages)
    (cond
      [(empty? percentages) '()]
      [else (cons (max 0 (sub1 (first percentages))) (decrement-base-bsl (rest percentages)))]))

;; ---------------------------------------------------------------------------------------------------
;; MODULE plain-racket

(define (ctd-plain-racket percentages0) ;; contract  ctd/c
    (let ctd-loop ([percentages percentages0])
      (cond
        [(empty? percentages) 0]
        [else
         (define one (first percentages))
         (if (> one 0)
             (add1 (ctd-loop (decrement-plain-racket (rest percentages))))
             (ctd-loop (rest percentages)))])))

  #; {[Listof %] -> [Listof %]}
(define (decrement-plain-racket percentages)
    (cond
      [(empty? percentages) '()]
      [else (cons (max 0 (sub1 (first percentages))) (decrement-plain-racket (rest percentages)))]))

;; ---------------------------------------------------------------------------------------------------
;; MODULE ho-racket 

(define (ctd-ho-racket  percentages0) ;; contract  ctd/c
    (let ctd-loop ([percentages percentages0])
      (cond
        [(empty? percentages) 0]
        [else
         (define one (first percentages))
         (if (> one 0)
             (add1 (ctd-loop (decrement-ho-racket  (rest percentages))))
             (ctd-loop (rest percentages)))])))

  #; {[Listof %] -> [Listof %]}
(define (decrement-ho-racket  percentages)
    (map (λ (x) (max 0 (- x 1))) percentages))

;; ---------------------------------------------------------------------------------------------------
;; MODULE inline-racket 

(define (ctd-inline-racket  percentages0) ;; contract  ctd/c
    (let ctd-loop ([percentages percentages0])
      (cond
        [(empty? percentages) 0]
        [else
         (define one (first percentages))
         (if (> one 0)
             (add1 (ctd-loop (map (λ (x) (max 0 (- x 1))) (rest percentages))))
             (ctd-loop (rest percentages)))])))

;; ---------------------------------------------------------------------------------------------------
;; MODULE accu-racket 

(define (ctd-accu-racket  percentages0) ;; contract  ctd/c
    (let ctd-loop ([percentages percentages0] [count 0])
      (cond
        [(empty? percentages) count]
        [else
         (define one (first percentages))
         (if (> one 0)
             (ctd-loop (map (λ (x) (max 0 (- x 1))) (rest percentages)) (add1 count))
             (ctd-loop (rest percentages) count))])))

;; ---------------------------------------------------------------------------------------------------
;; MODULE accu-2-racket 

(define (ctd-accu-2-racket  percentages0) ;; contract  ctd/c
    (let ctd-loop ([percentages percentages0] [count 0] [delta 0])
      (cond
        [(empty? percentages) count]
        [else
         (define one (first percentages))
         (if (> (- one delta) 0)
             (ctd-loop (rest percentages) (add1 count) (add1 delta))
             (ctd-loop (rest percentages) count        delta))])))

;; ---------------------------------------------------------------------------------------------------
;; MODULE for-racket 

(define (ctd-for-racket  percentages0) ;; contract  ctd/c
    (for/fold ([count 0] [delta 0] #:result count) ([x (in-list percentages0)])
      (if (> (- x delta) 0)
          (values (add1 count) (add1 delta))
          (values count        delta))))

;; ---------------------------------------------------------------------------------------------------
;; MODULE base-isl+

(define (ctd-base-isl+ percentages) ;; contract  ctd/c
    (cond
      [(empty? percentages) 0]
      [else
       (define one (first percentages))
       (if (> one 0)
           (add1 (ctd-base-isl+ (decrement-base-isl+ (rest percentages))))
           (ctd-base-isl+ (rest percentages)))]))
           
  #; {[Listof %] -> [Listof %]}
(define (decrement-base-isl+ percentages)
    (map (λ (x) (max 0 (- x 1))) percentages))

;; ---------------------------------------------------------------------------------------------------
;; MODULE base-bsl-accu

(define (ctd-base-bsl-accu percentages) ;; contract  ctd/c
    (ctd/bsl-base-bsl-accu percentages 0))

  #; {[Listof %] N -> N}
(define (ctd/bsl-base-bsl-accu percentages counted#)
    (cond
      [(empty? percentages) counted#]
      [else
       (define one (first percentages))
       (if (> one 0)
           (ctd/bsl-base-bsl-accu (decrement-base-bsl-accu (rest percentages)) (add1 counted#))
           (ctd/bsl-base-bsl-accu (rest percentages) counted#))]))

  #; {[Listof %] -> [Listof %]}
(define (decrement-base-bsl-accu percentages)
    (cond
      [(empty? percentages) '()]
      [else (cons (max 0 (sub1 (first percentages))) (decrement-base-bsl-accu (rest percentages)))]))

;; ---------------------------------------------------------------------------------------------------
;; MODULE base-isl+-accu

(define (ctd-base-isl+-accu percentages) ;; contract  ctd/c
    (ctd/bsl-base-isl+-accu percentages 0))

  #; {[Listof %] N -> N}
(define (ctd/bsl-base-isl+-accu percentages counted#)
    (cond
      [(empty? percentages) counted#]
      [else
       (define one (first percentages))
       (if (> one 0)
           (ctd/bsl-base-isl+-accu (decrement-base-isl+-accu (rest percentages)) (add1 counted#))
           (ctd/bsl-base-isl+-accu (rest percentages) counted#))]))

  #; {[Listof %] -> [Listof %]}
(define (decrement-base-isl+-accu percentages)
    (map (λ (x) (max 0 (- x 1))) percentages))

;; ---------------------------------------------------------------------------------------------------
;; MODULE base-isl+-2-accu

(define (ctd-base-isl+-2-accu percentages) ;; contract  ctd/c
    (ctd/bsl-base-isl+-2-accu percentages 0 0))

  #; {[Listof %] N -> N}
(define (ctd/bsl-base-isl+-2-accu percentages counted# delta)
    (cond
      [(empty? percentages) counted#]
      [else
       (define one (first percentages))
       (if (> (- one delta) 0)
           (ctd/bsl-base-isl+-2-accu (rest percentages) (add1 counted#) (add1 delta))
           (ctd/bsl-base-isl+-2-accu (rest percentages) counted#        delta))]))

;; ---------------------------------------------------------------------------------------------------
(test ctd
      in
      ai0
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
