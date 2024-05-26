#lang racket

(require "../testing-2.rkt")



(define (mp-ai3 n offers)
  ;; Sort offers based on their ending time
  (define sorted-offers (sort offers (lambda (x y) (< (second x) (second y)))))
  
  ;; Helper function to find the last non-conflicting offer using binary search
  (define (find-last-non-conflicting offers i)
    (let loop ((low 0) (high (- i 1)))
      (if (> low high)
          -1
          (let ((mid (quotient (+ low high) 2)))
            (if (< (second (list-ref offers mid)) (first (list-ref offers i)))
                (if (< (second (list-ref offers (+ mid 1))) (first (list-ref offers i)))
                    (loop (+ mid 1) high)
                    mid)
                (loop low (- mid 1)))))))
  
  ;; Initialize DP array where dp[i] will store the maximum profit using the first i offers
  (define dp (make-vector (length sorted-offers) 0))
  
  ;; Base case: the profit of the first offer is just its gold value
  (vector-set! dp 0 (third (list-ref sorted-offers 0)))
  
  ;; Fill the dp array
  (for ([i (in-range 1 (length sorted-offers))])
    (let* ((current-offer (list-ref sorted-offers i))
           (profit-including-current (third current-offer))
           (l (find-last-non-conflicting sorted-offers i)))
      (when (>= l 0)
        (set! profit-including-current (+ profit-including-current (vector-ref dp l))))
      (vector-set! dp i (max (vector-ref dp (- i 1)) profit-including-current))))
  
  ;; The last element in dp array will have the answer to the problem
  (vector-ref dp (- (length sorted-offers) 1)))

(define (mp-ai2 n offers)
  ;; Sort offers by their ending time
  (define sorted-offers (sort offers < #:key second))
  
  ;; Helper function to find the last non-conflicting offer using binary search
  (define (find-last-non-conflicting offers i)
    (let loop ((low 0) (high (- i 1)))
      (if (> low high)
          -1
          (let* ((mid (quotient (+ low high) 2))
                 (mid-offer (list-ref offers mid)))
            (if (< (second mid-offer) (first (list-ref offers i)))
                (if (< (second (list-ref offers (+ mid 1))) (first (list-ref offers i)))
                    (loop (+ mid 1) high)
                    mid)
                (loop low (- mid 1)))))))
  
  ;; Recursive function to calculate maximum profit using memoization
  (define dp (make-vector (length sorted-offers) 0))
  (vector-set! dp 0 (third (list-ref sorted-offers 0))) ;; Base case
  
  (define (calculate-max-profit i)
    (if (= i 0)
        (vector-ref dp 0)
        (let ((profit-including-current (third (list-ref sorted-offers i)))
              (l (find-last-non-conflicting sorted-offers i)))
          (when (>= l 0)
            (set! profit-including-current (+ profit-including-current (vector-ref dp l))))
          (vector-set! dp i (max (vector-ref dp (- i 1)) profit-including-current))
          (vector-ref dp i))))
  
  ;; Fill dp vector using recursion
  (for ([i (in-range 1 (length sorted-offers))])
    (calculate-max-profit i))
  
  ;; Return the last element in dp vector as the result
  (vector-ref dp (- (length sorted-offers) 1)))



(define (mp-ai1 n offers)
  ;; Sort offers based on their ending time
  (define sorted-offers (sort offers (lambda (x y) (< (second x) (second y)))))

  ;; Helper function to perform binary search and find the last non-conflicting offer
  (define (findLastNonConflicting offers i)
    (let loop ((low 0) (high (- i 1)))
      (if (> low high)
          -1
          (let ((mid (quotient (+ low high) 2)))
            (if (< (second (list-ref offers mid)) (first (list-ref offers i)))
                (if (< (second (list-ref offers (+ mid 1))) (first (list-ref offers i)))
                    (loop (+ mid 1) high)
                    mid)
                (loop low (- mid 1)))))))

  ;; Dynamic programming list to store the maximum profit up to each offer
  (define dp (make-vector (length sorted-offers) 0))

  ;; Initialize the base case with the profit of the first offer
  (vector-set! dp 0 (third (list-ref sorted-offers 0)))

  ;; Fill the dp vector
  (for ([i (in-range 1 (length sorted-offers))])
    (let* ((current-offer (list-ref sorted-offers i))
           (profit-including-current (third current-offer))
           (last-index (findLastNonConflicting sorted-offers i)))
      (when (>= last-index 0)
        (set! profit-including-current (+ profit-including-current (vector-ref dp last-index))))
      ;; Calculate maximum profit by either including or excluding the current offer
      (vector-set! dp i (max (vector-ref dp (- i 1)) profit-including-current))))

  ;; The last element in dp vector contains the maximum profit achievable
  (vector-ref dp (- (length sorted-offers) 1)))



(define (mp-ai0 n offers)
  ;; Sort offers by their ending time to process them in order
  (define sorted-offers (sort offers (lambda (x y) (< (second x) (second y)))))
  
  ;; Helper function to find the last non-conflicting offer using binary search
  (define (find-last-non-conflicting offers i)
    (let loop ((low 0) (high (- i 1)))
      (if (> low high)
          -1
          (let* ((mid (quotient (+ low high) 2))
                 (mid-offer (list-ref offers mid)))
            (if (< (second mid-offer) (first (list-ref offers i)))
                (if (< (second (list-ref offers (+ mid 1))) (first (list-ref offers i)))
                    (loop (+ mid 1) high)
                    mid)
                (loop low (- mid 1)))))))
  
  ;; Initialize DP list where dp[i] will store the maximum profit using the first i offers
  (define dp (make-vector (length sorted-offers) 0))
  
  ;; Base case: the profit of the first offer is just its gold value
  (vector-set! dp 0 (third (first sorted-offers)))
  
  ;; Fill the dp array
  (for ([i (in-range 1 (length sorted-offers))])
    (let* ((current-offer (list-ref sorted-offers i))
           (profit-including-current (third current-offer))
           (l (find-last-non-conflicting sorted-offers i)))
      (when (not (= l -1))
        (set! profit-including-current (+ profit-including-current (vector-ref dp l))))
      (vector-set! dp i (max (vector-ref dp (- i 1)) profit-including-current))))
  
  ;; The last element in dp vector will have the answer to the problem
  (vector-ref dp (- (length sorted-offers) 1)))



(define (mp-aiHIGH n offers)
  ;; Define a helper function to find the last non-conflicting offer using binary search
  (define (find-last-non-conflicting offers i)
    (let loop ([low 0] [high (sub1 i)])
      (if (> low high)
          -1
          (let ([mid (quotient (+ low high) 2)])
            (if (< (second (list-ref offers mid)) (first (list-ref offers i)))
                (if (< (second (list-ref offers (add1 mid))) (first (list-ref offers i)))
                    (loop (add1 mid) high)
                    mid)
                (loop low (sub1 mid)))))))

  ;; Sorting offers based on their ending time
  (define sorted-offers (sort offers (lambda (x y) (< (second x) (second y)))))

  ;; Dynamic programming function to calculate maximum profit
  (define (dp i)
    (cond
      [(= i 0) (third (list-ref sorted-offers 0))]
      [else
       (let* ([current-offer (list-ref sorted-offers i)]
              [profit-including-current (third current-offer)]
              [last-index (find-last-non-conflicting sorted-offers i)]
              [profit-including-current (if (>= last-index 0)
                                            (+ profit-including-current (dp last-index))
                                            profit-including-current)])
         (max (dp (sub1 i)) profit-including-current))]))

  ;; Start the dynamic programming from the last index
  (dp (sub1 (length sorted-offers))))


(define (mp-HIGH n offers)


 
     
         
  (define (dp offers)

     (define (next-possible end last first)
      (define pivot (+ first (quotient (- last first) 2)))
      (match-define (list next-start _ _) (vector-ref offers pivot))
      (cond [(= first last) (and (< end next-start) last)]
            [(>= end next-start) (next-possible end last (+ 1 pivot))]
            [else (next-possible end pivot first)]))

    
    (define memo-table (make-hash))
    
    (define (memoize offer-index profit)
      (hash-set! memo-table offer-index profit)
      profit)
    
    (define last (sub1 (vector-length offers)))
    (define (last-offer? i) (= i last))
    
    (define (memo-dp this)
      (match-define (list _ this-end this-profit) (vector-ref offers this))
      (or (hash-ref memo-table this #f)
          (match this
            [(app last-offer? #t) (memoize this this-profit)]
            [(app (curry next-possible this-end last)
                  (? number? i))
             (memoize
              this
              (max (memo-dp (+ 1 this)) (+ this-profit (memo-dp i))))]
            [_ (memoize this (max (memo-dp (+ 1 this)) this-profit))])))
    
    (memo-dp 0))
       
       
  (define sorted-offers (list->vector (sort offers < #:key first)))

  (dp sorted-offers))


(test mp
      in
      ai0
      ai1
      ai2
      ai3
      aiHIGH
      HIGH
      [#:show-graph #true]
      with

      ;; llm test
      (check-equal? (mp 5 (list (list 0 0 1) (list 0 2 2) (list 1 3 2))) 3)
      (check-equal? (mp 5 (list (list 0 0 1) (list 0 2 10) (list 1 3 2))) 10)
      (check-equal? (mp 4 (list (list 1 3 10) (list 1 3 3) (list 0 0 1) (list 0 0 7))) 17)
      (check-equal? (mp 4 (list (list 0 0 6) (list 1 2 8) (list 0 3 7) (list 2 2 5) (list 0 1 5) (list 2 3 2) (list 0 2 8) (list 2 3 10) (list 0 3 2))) 16)
      (check-equal? (mp 15 (list (list 5 5 10) (list 2 6 6) (list 8 11 5) (list 7 11 9) (list 2 4 1) (list 3 8 5) (list 0 6 9) (list 0 10 5) (list 5 10 8) (list 4 5 1))) 20)
      (check-equal? (mp 10 (list (list 1 6 1) (list 0 1 10) (list 3 6 2) (list 0 5 10) (list 0 0 3) (list 0 0 4) (list 1 1 4) (list 0 6 7) (list 4 4 1))) 12)
      (check-equal? (mp 11 (list (list 7 8 6) (list 6 6 4) (list 4 6 9) (list 6 7 4) (list 5 5 8) (list 1 5 9) (list 7 7 8) (list 1 2 5) (list 0 2 9) (list 1 3 8) (list 0 2 7) (list 2 2 8))) 29)
      (check-equal? (mp 3 (list (list 0 0 6) (list 0 1 8) (list 1 2 1) (list 0 1 4) (list 0 1 2) (list 0 0 7) (list 0 0 6) (list 0 0 5))) 8)
      (check-equal? (mp 4 (list (list 0 1 9) (list 1 1 4))) 9)
      (check-equal? (mp 11 (list (list 1 10 6) (list 1 10 5) (list 0 2 7) (list 0 0 8) (list 8 10 7))) 15)
      (check-equal? (mp 3 (list (list 0 1 8) (list 1 1 6) (list 2 2 7) (list 0 2 6) (list 0 2 2) (list 0 0 6) (list 0 0 9) (list 0 1 4))) 22)
      (check-equal? (mp 6 (list (list 0 2 4))) 4)
      (check-equal? (mp 10 (list (list 5 9 3) (list 1 5 8) (list 0 0 6) (list 5 8 10))) 16)
      (check-equal? (mp 5 (list (list 1 1 3) (list 1 1 3) (list 0 0 8) (list 1 3 8) (list 0 2 1) (list 3 3 9) (list 0 0 7) (list 0 2 3) (list 0 0 5) (list 0 3 10) (list 1 3 10) (list 4 4 6) (list 0 1 1) (list 2 4 10))) 26)
      (check-equal? (mp 13 (list (list 2 2 5) (list 1 8 10) (list 2 3 3))) 10)
      (check-equal? (mp 2 (list (list 1 1 8) (list 1 1 8) (list 1 1 10) (list 1 1 7) (list 0 0 7) (list 0 0 3) (list 0 1 8) (list 0 0 4) (list 0 0 4) (list 0 0 7) (list 0 0 10) (list 0 1 4) (list 1 1 1) (list 0 1 5))) 20)
      (check-equal? (mp 3 (list (list 0 1 7) (list 1 1 3) (list 0 0 2) (list 1 1 6) (list 0 0 10) (list 1 1 7) (list 0 2 3) (list 0 1 2) (list 0 0 7))) 17)
      (check-equal? (mp 5 (list (list 0 0 5) (list 1 3 9) (list 0 2 2) (list 1 1 6) (list 1 2 10) (list 0 2 10) (list 1 1 3))) 15)
      (check-equal? (mp 10 (list (list 0 1 9) (list 5 6 10) (list 1 3 8) (list 1 9 7) (list 7 8 1) (list 2 7 1) (list 0 8 7) (list 1 6 6) (list 1 4 4) (list 0 5 4) (list 0 0 3) (list 0 8 6))) 22)
      (check-equal? (mp 4 (list (list 0 0 1) (list 0 0 10) (list 0 2 1) (list 0 0 6) (list 0 3 10) (list 0 1 5) (list 1 2 10) (list 0 0 2) (list 3 3 1) (list 0 0 9) (list 0 1 2) (list 0 0 4) (list 1 3 5) (list 1 1 1))) 21)
      (check-equal? (mp 9 (list (list 0 3 10) (list 5 6 5) (list 1 5 2) (list 1 8 9) (list 1 1 9) (list 1 7 1) (list 3 7 9) (list 2 3 2) (list 4 6 1) (list 4 5 7) (list 2 2 2) (list 6 8 10) (list 1 3 10) (list 1 4 10))) 28)
      (check-equal? (mp 10 (list (list 0 2 2))) 2)
      (check-equal? (mp 10 (list (list 2 7 4) (list 2 4 9) (list 1 8 7) (list 0 4 3))) 9)
      (check-equal? (mp 6 (list (list 0 1 4) (list 1 2 4) (list 0 1 10) (list 1 2 4) (list 2 2 5) (list 1 1 8) (list 2 3 2) (list 4 4 4) (list 0 0 3))) 20)
      (check-equal? (mp 1 (list (list 0 0 8) (list 0 0 3) (list 0 0 8) (list 0 0 8) (list 0 0 5) (list 0 0 9) (list 0 0 6) (list 0 0 1) (list 0 0 8) (list 0 0 1) (list 0 0 5) (list 0 0 9) (list 0 0 2))) 9)
      (check-equal? (mp 15 (list (list 8 10 5) (list 4 12 6) (list 6 11 7) (list 8 11 3) (list 7 13 1) (list 7 7 8) (list 8 10 5) (list 0 11 3) (list 1 1 9) (list 2 11 6) (list 3 11 8))) 22)
      (check-equal? (mp 10 (list (list 5 6 9) (list 0 2 9))) 18)
      (check-equal? (mp 11 (list (list 7 9 5) (list 0 0 8) (list 6 6 3) (list 4 9 1) (list 3 7 5) (list 0 4 7))) 16)
      (check-equal? (mp 7 (list (list 0 2 9) (list 2 4 8) (list 0 3 6) (list 4 4 10) (list 2 2 2) (list 1 1 10) (list 0 0 8) (list 4 4 9) (list 4 4 4) (list 3 3 5) (list 2 5 2) (list 0 3 6) (list 3 4 5))) 35)
      (check-equal? (mp 9 (list (list 3 8 1) (list 0 6 7) (list 0 3 6) (list 1 6 2) (list 2 3 10) (list 3 3 2) (list 1 2 2) (list 1 3 9) (list 0 0 7) (list 1 2 9) (list 5 5 4) (list 5 6 6) (list 1 5 5) (list 0 1 2) (list 0 6 1))) 24)
      (check-equal? (mp 8 (list (list 0 0 7) (list 0 1 8) (list 1 1 1) (list 2 2 7) (list 2 3 1))) 15)
      (check-equal? (mp 8 (list (list 6 6 5) (list 0 1 7) (list 1 7 10))) 12)
      (check-equal? (mp 13 (list (list 0 9 5) (list 6 8 7) (list 0 0 3) (list 4 4 2) (list 1 9 7) (list 9 12 9) (list 1 2 9) (list 1 1 10) (list 3 3 3) (list 0 3 3) (list 4 8 5) (list 0 0 9) (list 7 10 7))) 40)
      (check-equal? (mp 11 (list (list 2 5 1))) 1)
      (check-equal? (mp 3 (list (list 0 0 9) (list 0 2 6) (list 1 1 1) (list 1 2 10) (list 0 0 10) (list 0 0 4) (list 0 2 7) (list 0 0 1) (list 0 0 9) (list 2 2 5))) 20)
      (check-equal? (mp 5 (list (list 1 1 3) (list 1 2 1) (list 0 2 3) (list 1 1 10) (list 3 3 3) (list 2 4 3) (list 0 3 5) (list 4 4 2) (list 2 3 10) (list 3 3 8) (list 3 3 9) (list 0 2 8) (list 0 2 2) (list 1 1 3) (list 0 0 8))) 30)
      (check-equal? (mp 13 (list (list 6 9 3) (list 6 9 6) (list 5 12 10) (list 11 12 4) (list 4 4 2) (list 0 7 8) (list 2 6 6) (list 6 6 4))) 12)
      (check-equal? (mp 3 (list (list 0 2 9) (list 1 1 8) (list 0 1 1) (list 2 2 4) (list 2 2 1) (list 0 0 4) (list 1 1 9) (list 0 0 6) (list 0 1 7))) 19)
      (check-equal? (mp 3 (list (list 1 2 8) (list 0 0 1) (list 0 1 1) (list 0 0 3) (list 1 2 2) (list 0 0 7) (list 0 0 10) (list 1 1 6))) 18)
      (check-equal? (mp 2 (list (list 0 0 3) (list 1 1 10) (list 0 1 6))) 13)
      (check-equal? (mp 3 (list (list 0 0 9) (list 1 1 1) (list 0 2 7) (list 1 1 7) (list 1 2 6) (list 0 0 8) (list 0 2 3) (list 1 2 10) (list 2 2 3) (list 2 2 5))) 21)
      (check-equal? (mp 5 (list (list 2 3 2) (list 0 1 7) (list 0 1 1) (list 0 0 9) (list 2 4 1) (list 3 4 5) (list 1 3 10) (list 0 0 8))) 19)
      (check-equal? (mp 15 (list (list 4 6 9) (list 4 10 9) (list 3 5 4) (list 0 2 6) (list 3 13 7) (list 1 11 6) (list 1 8 4) (list 4 12 4) (list 3 8 8) (list 13 13 7) (list 4 12 3))) 22)
      (check-equal? (mp 8 (list (list 1 5 9) (list 0 4 9) (list 0 0 3) (list 1 2 9) (list 0 0 10) (list 4 7 9) (list 7 7 2) (list 0 2 6) (list 1 1 5) (list 1 4 3) (list 2 4 8) (list 0 1 1) (list 2 3 1))) 28)
      (check-equal? (mp 4 (list (list 0 2 7) (list 2 3 9) (list 2 3 2) (list 1 2 1) (list 1 2 9) (list 0 3 7) (list 0 2 9) (list 1 2 8) (list 0 3 10) (list 0 3 8) (list 0 0 5) (list 2 2 6))) 14)
      (check-equal? (mp 12 (list (list 0 0 4) (list 5 8 2) (list 2 2 10) (list 3 5 7) (list 1 2 1) (list 5 7 8) (list 8 11 3))) 25)
      (check-equal? (mp 2 (list (list 0 0 7) (list 0 1 3) (list 0 0 8))) 8)
      (check-equal? (mp 4 (list (list 2 3 8) (list 0 1 1) (list 3 3 2))) 9)
      (check-equal? (mp 14 (list (list 2 12 4) (list 7 11 4) (list 4 4 5) (list 0 1 6) (list 3 4 1) (list 4 11 9) (list 10 12 7) (list 7 12 1) (list 11 11 1) (list 0 0 5) (list 12 12 8) (list 6 7 6))) 26)
      (check-equal? (mp 10 (list (list 1 4 6) (list 7 9 9) (list 1 4 5) (list 8 8 2) (list 4 7 1) (list 6 8 8) (list 2 3 1) (list 0 1 4))) 15)
      (check-equal? (mp 7 (list (list 2 5 5) (list 1 2 9) (list 1 3 7) (list 2 4 3) (list 0 0 6) (list 0 0 1) (list 4 4 9) (list 1 5 7) (list 2 2 10))) 25)
      (check-equal? (mp 11 (list (list 0 4 10))) 10)
      (check-equal? (mp 3 (list (list 0 1 10) (list 1 2 2) (list 0 2 6) (list 0 0 1) (list 0 0 3) (list 0 1 8) (list 0 0 2) (list 2 2 8) (list 0 0 3) (list 2 2 3) (list 1 2 6) (list 0 0 4) (list 1 2 5))) 18)
      (check-equal? (mp 14 (list (list 11 11 4) (list 1 11 10) (list 11 12 2) (list 7 8 2))) 10)
      (check-equal? (mp 2 (list (list 0 0 1) (list 0 0 1) (list 1 1 9) (list 0 0 1) (list 1 1 2) (list 0 1 10))) 10)
      (check-equal? (mp 6 (list (list 0 5 6) (list 1 2 10) (list 0 2 4) (list 2 4 5) (list 4 4 6) (list 2 2 2) (list 0 0 7) (list 2 5 9) (list 2 2 3))) 23)
      (check-equal? (mp 6 (list (list 0 0 7) (list 2 5 5))) 12)
      (check-equal? (mp 10 (list (list 2 3 2) (list 0 1 6) (list 0 0 2) (list 1 1 5) (list 3 3 8) (list 2 8 7) (list 1 7 8) (list 0 1 4) (list 7 7 8) (list 1 3 7) (list 5 5 10) (list 2 6 6) (list 0 0 4) (list 5 7 4) (list 1 9 4))) 35)
      (check-equal? (mp 10 (list (list 0 2 4) (list 1 4 7) (list 0 1 10) (list 0 5 1))) 10)
      (check-equal? (mp 12 (list (list 0 5 6) (list 4 10 9) (list 7 11 10) (list 10 11 1) (list 6 10 1) (list 2 2 6))) 16)
      (check-equal? (mp 11 (list (list 3 7 8) (list 2 7 10) (list 3 9 3))) 10)
      (check-equal? (mp 4 (list (list 0 0 3) (list 0 2 6) (list 0 0 1) (list 1 1 2) (list 0 2 8) (list 1 1 3) (list 1 3 8) (list 1 1 10) (list 1 2 7) (list 1 1 8) (list 0 0 9))) 19)
      (check-equal? (mp 1 (list (list 0 0 9))) 9)
      (check-equal? (mp 3 (list (list 0 1 5) (list 0 0 5) (list 0 0 6) (list 0 1 6) (list 0 2 10) (list 1 2 6) (list 0 0 9) (list 1 2 9))) 18)
      (check-equal? (mp 4 (list (list 0 0 2) (list 2 3 9) (list 0 1 8) (list 0 0 9) (list 0 0 1) (list 3 3 9) (list 1 2 1) (list 1 3 5) (list 0 1 4) (list 0 1 4))) 19)
      (check-equal? (mp 3 (list (list 0 0 7) (list 2 2 1) (list 1 1 3) (list 0 0 3) (list 1 1 7) (list 0 1 5) (list 0 2 3) (list 1 1 5) (list 0 1 10) (list 1 1 5) (list 1 1 6) (list 0 1 3) (list 0 0 8) (list 1 2 7) (list 1 1 4))) 16)
      (check-equal? (mp 14 (list (list 5 7 2) (list 1 5 3) (list 11 13 2) (list 12 12 5) (list 4 5 6) (list 5 10 2) (list 4 10 8) (list 1 1 4) (list 4 4 2) (list 3 7 9) (list 5 10 1) (list 0 3 2))) 18)
      (check-equal? (mp 11 (list (list 1 1 5) (list 4 4 9) (list 0 0 1) (list 1 3 3) (list 3 7 4) (list 3 9 6) (list 7 10 2) (list 3 7 5) (list 4 4 8) (list 7 8 10) (list 1 3 7) (list 1 4 5) (list 0 0 10))) 36)
      (check-equal? (mp 13 (list (list 4 9 9) (list 1 9 8) (list 1 9 8) (list 0 0 8) (list 8 11 3) (list 2 3 6) (list 9 9 10) (list 5 12 1) (list 4 6 4))) 28)
      (check-equal? (mp 5 (list (list 2 2 7) (list 0 2 10) (list 2 3 10))) 10)
      (check-equal? (mp 10 (list (list 0 4 6) (list 1 1 1) (list 0 5 1) (list 1 6 3) (list 8 9 1) (list 2 3 7) (list 2 3 10) (list 1 2 1) (list 0 0 8) (list 3 5 5) (list 0 0 10))) 22)
      (check-equal? (mp 4 (list (list 0 1 1) (list 0 0 9) (list 1 1 8) (list 3 3 1) (list 1 1 5) (list 0 0 9) (list 0 1 9) (list 0 0 7) (list 2 2 2) (list 2 3 5) (list 1 1 10) (list 1 2 8))) 24)
      (check-equal? (mp 7 (list (list 0 1 9) (list 0 1 4) (list 0 0 3) (list 0 0 1) (list 1 6 5) (list 4 6 9) (list 4 5 7) (list 0 0 3) (list 1 5 9) (list 0 2 2))) 18)
      (check-equal? (mp 12 (list (list 8 8 6) (list 8 8 6) (list 1 10 7) (list 0 0 3) (list 9 10 7) (list 1 7 2) (list 1 1 1) (list 2 3 6) (list 0 11 1) (list 1 8 5) (list 1 5 7) (list 1 2 4) (list 9 9 5) (list 0 3 1))) 23)
      (check-equal? (mp 15 (list (list 5 6 3) (list 2 2 7) (list 0 0 5) (list 1 7 10) (list 11 14 5) (list 13 14 1) (list 2 12 1) (list 0 4 5) (list 0 6 2) (list 6 9 10) (list 3 5 2) (list 0 1 1) (list 1 14 1) (list 1 6 1))) 29)
      (check-equal? (mp 7 (list (list 1 1 5) (list 1 1 4) (list 0 0 9) (list 1 1 6) (list 0 6 4) (list 2 6 3) (list 2 5 9) (list 0 6 3) (list 0 2 1) (list 1 1 6) (list 4 5 5))) 24)
      (check-equal? (mp 1 (list (list 0 0 5) (list 0 0 3) (list 0 0 4) (list 0 0 8) (list 0 0 10) (list 0 0 6) (list 0 0 7) (list 0 0 7) (list 0 0 7) (list 0 0 3) (list 0 0 4) (list 0 0 5))) 10)
      (check-equal? (mp 7 (list (list 2 2 3) (list 2 6 4) (list 4 6 5) (list 0 0 4) (list 1 1 4) (list 2 3 1) (list 2 4 3) (list 0 2 8) (list 1 3 10) (list 1 3 2) (list 1 6 7) (list 0 6 9) (list 2 2 2) (list 1 1 9) (list 4 4 2))) 21)
      (check-equal? (mp 12 (list (list 0 0 7) (list 0 2 3) (list 0 7 2) (list 2 3 1) (list 2 11 6) (list 2 10 2) (list 1 3 6) (list 4 7 9) (list 7 9 3) (list 4 6 1) (list 5 6 8) (list 0 2 4) (list 0 0 3) (list 5 5 9) (list 2 5 3))) 25)
      (check-equal? (mp 9 (list (list 1 8 4) (list 5 6 5) (list 0 2 6) (list 4 5 4))) 11)
      (check-equal? (mp 8 (list (list 0 4 6) (list 2 3 6) (list 2 5 9) (list 2 6 7) (list 6 6 5) (list 4 4 4) (list 1 1 5) (list 2 5 7))) 20)
      (check-equal? (mp 13 (list (list 0 6 10))) 10)
      (check-equal? (mp 6 (list (list 0 1 2) (list 0 0 9) (list 3 3 10) (list 0 3 7) (list 0 0 2) (list 0 0 3) (list 2 2 2) (list 2 3 2) (list 5 5 6) (list 0 1 2) (list 0 5 2))) 27)
      (check-equal? (mp 14 (list (list 3 12 7) (list 1 3 2) (list 4 11 3) (list 0 1 7) (list 1 5 2) (list 1 1 4))) 14)
      (check-equal? (mp 14 (list (list 0 0 3) (list 0 1 3) (list 1 11 3) (list 6 7 6) (list 7 7 5) (list 1 2 8) (list 7 10 9))) 20)
      (check-equal? (mp 13 (list (list 0 12 7) (list 2 2 4) (list 2 2 8) (list 3 3 2) (list 1 11 5) (list 1 7 2))) 10)
      (check-equal? (mp 1 (list (list 0 0 2) (list 0 0 8) (list 0 0 1))) 8)
      (check-equal? (mp 1 (list (list 0 0 1) (list 0 0 4) (list 0 0 7) (list 0 0 2) (list 0 0 5) (list 0 0 1) (list 0 0 4) (list 0 0 2) (list 0 0 6) (list 0 0 6) (list 0 0 3) (list 0 0 3))) 7)
      (check-equal? (mp 1 (list (list 0 0 6) (list 0 0 6) (list 0 0 3) (list 0 0 6) (list 0 0 6) (list 0 0 10) (list 0 0 1) (list 0 0 2))) 10)
      (check-equal? (mp 9 (list (list 4 6 7) (list 1 3 10))) 17)
      (check-equal? (mp 13 (list (list 2 6 3) (list 1 12 6) (list 2 11 3) (list 7 7 2) (list 5 12 4) (list 0 1 2) (list 0 1 8) (list 1 1 3) (list 6 6 4) (list 8 9 7) (list 8 8 2) (list 2 2 2) (list 0 0 9) (list 9 11 7) (list 8 9 7))) 29)
      (check-equal? (mp 8 (list (list 0 1 8) (list 0 0 6) (list 5 5 9))) 17)
      (check-equal? (mp 1 (list (list 0 0 10) (list 0 0 3) (list 0 0 8) (list 0 0 9) (list 0 0 1) (list 0 0 8) (list 0 0 2) (list 0 0 7) (list 0 0 10) (list 0 0 8) (list 0 0 5) (list 0 0 3) (list 0 0 2) (list 0 0 4))) 10)
      (check-equal? (mp 9 (list (list 0 2 6) (list 1 3 5) (list 1 1 5) (list 2 3 10) (list 4 8 4) (list 5 8 5) (list 6 6 10))) 25)
      (check-equal? (mp 6 (list (list 0 0 7))) 7)
      (check-equal? (mp 8 (list (list 1 1 5) (list 1 2 9) (list 1 2 6) (list 0 3 6) (list 1 1 10) (list 3 4 1) (list 3 5 3) (list 1 5 8) (list 0 2 6) (list 5 7 9))) 20)
      (check-equal? (mp 14 (list (list 3 4 4) (list 6 8 1) (list 0 4 1))) 5)
      (check-equal? (mp 11 (list (list 4 4 2) (list 1 2 7) (list 2 8 10) (list 1 1 3) (list 8 10 4) (list 1 2 1) (list 4 6 10))) 21)
      (check-equal? (mp 11 (list (list 1 8 1) (list 1 5 5) (list 0 1 3) (list 10 10 10) (list 1 1 8) (list 1 2 1) (list 2 3 10) (list 2 10 10) (list 2 2 9) (list 0 9 4))) 28)
      (check-equal? (mp 6 (list (list 2 2 6) (list 0 1 2) (list 2 2 2))) 8))


