#lang racket

(require "../testing-2.rkt")

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai5

(define (sc-ai5 nums)
  ;; Calculates the sum of the squares of distinct counts of all subarrays of nums.
  (define (subarray-distinct-squares start)
    ;; Calculates the sum of squares of distinct counts for subarrays starting at 'start'
    (define seen (make-hash))
    (define (helper end total-sum)
      ;; Recursive helper function to process subarrays from 'start' to 'end'
      (if (= end (length nums))
          total-sum
          (begin
            (hash-update! seen (list-ref nums end) add1 0) ; Update count of current element
            (let ((distinct-count (hash-count seen))) ; Number of distinct elements
              (helper (add1 end) (+ total-sum (expt distinct-count 2))))))) ; Add square of distinct count
    (helper start 0))
  
  ;; Sum the results of subarray-distinct-squares for each starting point
  (define (sum-starts i)
    (if (= i (length nums))
        0
        (+ (subarray-distinct-squares i) (sum-starts (add1 i)))))
  
  (sum-starts 0))

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai4

(define (sc-ai4 nums)
  ;; This function calculates the sum of the squares of distinct counts of all subarrays of nums.
  (let ([n (length nums)])
    (for/sum ([i n]) ; Loop over each possible starting index i
      (let loop ([j i] [seen (hash)]) ; Loop over each possible ending index j starting from i
        (if (> j (- n 1))
            0 ; If j exceeds the bounds of nums, stop adding and return 0
            (let* ([current-element (list-ref nums j)] ; Get the element at index j
                   [updated-seen (hash-update seen current-element add1 0)] ; Update the count of the current element in the hash
                   [distinct-count (hash-count updated-seen)]) ; Count the number of distinct elements
              (+ (* distinct-count distinct-count) ; Add the square of the distinct count
                 (loop (+ j 1) updated-seen)))))))) ; Recursive call for the next element
  
;; ---------------------------------------------------------------------------------------------------
;; MODULE ai3

(define (sc-ai3 nums)
  ;; Calculate the sum of squares of distinct counts of all subarrays of nums.
  (define n (length nums))
  
  ;; Function to calculate the sum for all subarrays starting from index i.
  (define (sum-from-start i)
    (let loop ((j i) (seen (make-hash)) (total 0))
      (cond
        ;; When j reaches the end of the array, return the accumulated total.
        [(>= j n) total]
        [else
         ;; Update the hash table with the count of the current element.
         (hash-update! seen (list-ref nums j) add1 0)
         ;; Calculate the distinct count as the number of keys in the hash table.
         (let ((distinct-count (hash-count seen)))
           ;; Recursively calculate for the next element in the subarray.
           (loop (add1 j) seen (+ total (expt distinct-count 2))))])))
  
  ;; Sum the results of sum-from-start for each starting index in the array.
  (for/fold ([total-sum 0]) ([i (in-range n)])
    (+ total-sum (sum-from-start i))))

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai2

(define (sc-ai2 nums)
  ;; Calculate the sum of the squares of distinct counts of all subarrays of nums.
  (define (subarray-distinct-squares start)
    ;; Compute squares of distinct counts for subarrays starting at 'start'.
    (define (helper end seen)
      ;; Recursive helper to process subarrays from 'start' to 'end' with 'seen' as the set of seen elements.
      (if (> end (length nums))
          0
          (let* ((current-element (list-ref nums (- end 1)))
                 (new-seen (set-add seen current-element))
                 (distinct-count (set-count new-seen))
                 (square (expt distinct-count 2)))
            (+ square (helper (+ end 1) new-seen)))))
    (helper (+ start 1) (set)))
  
  ;; Sum up all subarray squares starting from each index.
  (define (sum-all-starts i)
    (if (= i (length nums))
        0
        (+ (subarray-distinct-squares i) (sum-all-starts (+ i 1)))))
  
  (sum-all-starts 0))

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai1

(define (sc-ai1 nums)
  (let ([n (length nums)])
    ;; Function to calculate the sum of squares of distinct counts for all subarrays starting at index i
    (define (sum-from-i i)
      (if (= i n)
          0
          (let loop ([j i] [seen (hash)] [total 0])
            (if (= j n)
                total
                (let* ([elem (list-ref nums j)]
                       [new-seen (hash-update seen elem add1 0)]
                       [distinct-count (hash-count new-seen)]
                       [new-total (+ total (expt distinct-count 2))])
                  (loop (+ j 1) new-seen new-total))))))
    ;; Sum over all starting indices
    (for/sum ([i (in-range n)])
      (sum-from-i i))))

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai0

(define (sc-ai0 nums)
  ;; Calculate the sum of the squares of distinct counts for all subarrays
  (let ([n (length nums)])
    ;; Initialize the total sum
    (define total-sum 0)
    ;; Iterate over each possible starting point of the subarray
    (for ([i (in-range n)])
      ;; Initialize a hash map to track distinct elements and their counts
      (define seen (make-hash))
      ;; Iterate over each possible ending point of the subarray starting from i
      (for ([j (in-range i n)])
        ;; Update the count of the current element in the hash map
        (hash-update! seen (list-ref nums j) add1 0)
        ;; Calculate the number of distinct elements
        (define distinct-count (hash-count seen))
        ;; Add the square of the distinct count to the total sum
        (set! total-sum (+ total-sum (* distinct-count distinct-count))))
      )
    ;; Return the computed total sum
    total-sum))

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
      ai5 ai4 ai3 ai2 ai1 ai0

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
