#lang racket

(require "../testing-2.rkt")

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai0
;;
(define (lols-ai0 nums target)
  ;; Use a hash table to store the sums and their maximum subsequence lengths
  (define dp (make-hash '((0 . 0))))  ; Initialize with base case: sum 0 with length 0

  ;; Iterate over each number in the list
  (for ([num nums])
    ;; Create a temporary hash to avoid mutation during iteration
    (define temp-dp (make-hash))
    ;; Iterate over current sums and their lengths in dp
    (hash-for-each dp (lambda (s length)
                        (let ((new-sum (+ s num)))
                          ;; Only consider new sums <= target
                          (when (<= new-sum target)
                            ;; Update the temporary hash with the new sum and length
                            (hash-update! temp-dp new-sum
                                          (lambda (old-length)
                                            (max old-length (+ length 1)))
                                          (+ length 1))))))
    ;; Merge temporary updates back into dp
    (hash-for-each temp-dp (lambda (key value)
                             (hash-update! dp key (lambda (old) (max old value)) value))))

  ;; Check if target sum is achieved and return the corresponding length
  (let ((result (hash-ref dp target #f)))
    (if result result -1)))


;; ---------------------------------------------------------------------------------------------------
;; MODULE aiHIGH
;;
(define (lols-aiHIGH nums target)
  ;; Helper function to update the hash table without mutating during iteration
  (define (update-dp dp num target)
    (for/fold ([new-dp dp]) ([key (in-hash-keys dp)])
      (let ((new-sum (+ key num)))
        (if (<= new-sum target)
            (hash-set new-dp new-sum (max (hash-ref new-dp new-sum 0) (add1 (hash-ref dp key))))
            new-dp))))

  ;; Initialize the hash table with the base case: sum 0 with length 0
  (define initial-dp (hash 0 0))

  ;; Iterate over each number in nums and update the hash table
  (define final-dp (foldl (lambda (num dp) (update-dp dp num target)) initial-dp nums))

  ;; Check if target sum is achieved and return the corresponding length
  (hash-ref final-dp target -1))

;; ---------------------------------------------------------------------------------------------------
;; MODULE HIGH
;;
(define (lols-HIGH nums target)

  (define memo-table (make-hash))

 
  (define (memoize key val)
    (hash-set! memo-table key val)
    val)

  (define (max-nums key how-many)
    (or (hash-ref memo-table key #f)
        (match key
          [(list _ 0) (memoize key how-many)]
          [(list '() _) (memoize key -1)]
          [(list (cons a b) r)
           #:do [(define new-r (- r a))]
           #:when (>= new-r 0)
           (memoize
            key
            (max (max-nums (list b r) how-many)
                 (max-nums (list b new-r)  (add1 how-many))))]
           [(list (cons a b) r)
            (memoize key (max-nums (list b r)  how-many))]))) 

  
  (max-nums (list nums target) 0))

(test lols
      in
      ai0
      aiHIGH
      HIGH
      [#:show-graph #true]
      with

      ;;llm tests
      (check-equal? (lols (list 1 2 3 4 5) 9) 3)
      (check-equal? (lols (list 4 1 3 2 1 5) 7) 4)
      (check-equal? (lols (list 1 1 5 4 5) 3) -1)
      (check-equal? (lols (list 1000) 12) -1)
      (check-equal? (lols (list 1000) 1000) 1)
      (check-equal? (lols (list 1 2) 10) -1)
      (check-equal? (lols (list 1 1000) 5) -1)
      (check-equal? (lols (list 2 3) 3) 1)
      (check-equal? (lols (list 2 3) 5) 2)
      (check-equal? (lols (list 2 3 5) 5) 2)
      (check-equal? (lols (list 1 3 3 7) 1000) -1)
      (check-equal? (lols (list 1 3 3 7) 2) -1)
      (check-equal? (lols (list 1 3 3 8) 7) 3)
      (check-equal? (lols (list 1 1 2 1) 2) 2)
      (check-equal? (lols (list 1 1 1 1) 5) -1)
      (check-equal? (lols (list 1 1 1 2) 3) 3)
      (check-equal? (lols (list 9 12 8 4 11 13 15 7 5) 84) 9)
      (check-equal? (lols (list 11 5 9 11 12 13 12 5 1 8) 87) 10)
      (check-equal? (lols (list 9 11 11 15 4 14 3 2 13 7) 89) 10)
      (check-equal? (lols (list 11 13 6 13 10) 53) 5)
      (check-equal? (lols (list 10 3 5 11 6 12) 47) 6)
      (check-equal? (lols (list 13 3 6 6 6 15 4) 53) 7)
      (check-equal? (lols (list 1 6 15 6 14 13 14) 69) 7)
      (check-equal? (lols (list 10 7 8 14 15) 54) 5)
      (check-equal? (lols (list 14 15 8 10 8 7) 62) 6)
      (check-equal? (lols (list 7 9 14 14 9 14 5 12 10) 94) 9)
      (check-equal? (lols (list 1 10 6 14 5 13 3 7 10 10) 79) 10)
      (check-equal? (lols (list 5 2 8 6 7 12 13 4 1) 58) 9)
      (check-equal? (lols (list 12 8 2 4 1) 27) 5)
      (check-equal? (lols (list 10 14 11 13 2 11) 61) 6)
      (check-equal? (lols (list 10 2 13 5 7 15) 52) 6)
      (check-equal? (lols (list 3 1 10 1 10 1 2 9 5 13) 55) 10)
      (check-equal? (lols (list 5 13 2 13 9 4 5 7) 58) 8)
      (check-equal? (lols (list 1 15 5 12 13 10 14 8) 78) 8)
      (check-equal? (lols (list 7 4 14 10 13) 48) 5)
      (check-equal? (lols (list 6 14 14 6 2 9 1 4 10) 66) 9)
      (check-equal? (lols (list 14 15 7 5 7 10 6 14 10 11) 99) 10)
      (check-equal? (lols (list 15 13 8 8 6) 50) 5)
      (check-equal? (lols (list 2 6 8 9 13 3) 41) 6)
      (check-equal? (lols (list 13 15 9 3 8 1 9 2 15 5) 80) 10)
      (check-equal? (lols (list 5 13 9 11 6 1) 45) 6)
      (check-equal? (lols (list 7 10 15 7 14 2) 55) 6)
      (check-equal? (lols (list 12 14 13 13 13) 65) 5)
      (check-equal? (lols (list 12 8 7 9 3 10 3 8 2) 62) 9)
      (check-equal? (lols (list 11 1 14 13 14 4 14 11) 82) 8)
      (check-equal? (lols (list 5 9 11 2 5 2 7 11 5 3) 60) 10)
      (check-equal? (lols (list 5 15 3 13 14 15 10) 75) 7)
      (check-equal? (lols (list 10 8 2 2 9) 31) 5)
      (check-equal? (lols (list 7 15 4 3 9 15 12 1 12) 78) 9)
      (check-equal? (lols (list 3 1 12 15 5 10) 46) 6)
      (check-equal? (lols (list 5 3 12 7 5 2 12 10 12 5) 73) 10)
      (check-equal? (lols (list 6 10 3 1 7 11 9 8 13 12) 80) 10)
      (check-equal? (lols (list 11 3 4 11 9) 38) 5)
      (check-equal? (lols (list 15 12 12 13 6 6 4 1) 69) 8)
      (check-equal? (lols (list 9 2 10 7 10 11 14 11 8) 82) 9)
      (check-equal? (lols (list 4 4 3 9 6 8 4 7 7) 52) 9)
      (check-equal? (lols (list 10 14 4 15 9 5) 57) 6)
      (check-equal? (lols (list 4 13 2 3 13 11 8 6) 60) 8)
      (check-equal? (lols (list 1 7 8 14 15 9 8 10 13 7) 92) 10)
      (check-equal? (lols (list 7 7 6 14 7 4) 45) 6)
      (check-equal? (lols (list 9 10 9 7 14 3 6 4 6) 68) 9)
      (check-equal? (lols (list 15 13 14 5 7 13 11 14) 92) 8)
      (check-equal? (lols (list 1 1 10 12 5 6 15 6 8) 64) 9)
      (check-equal? (lols (list 14 13 13 11 14 13 8) 86) 7)
      (check-equal? (lols (list 3 14 4 2 10 3 7) 43) 7)
      (check-equal? (lols (list 6 1 3 11 9 2 10 6 12) 60) 9)
      (check-equal? (lols (list 6 2 5 4 12) 29) 5)
      (check-equal? (lols (list 7 11 15 1 9 9 11) 63) 7)
      (check-equal? (lols (list 7 12 10 15 6 15 14 2) 81) 8)
      (check-equal? (lols (list 12 3 10 12 13 3 4 7 15) 79) 9)
      (check-equal? (lols (list 14 6 11 2 10 1 12 9 2) 67) 9)
      (check-equal? (lols (list 5 8 12 6 15 13 11) 70) 7)
      (check-equal? (lols (list 11 6 1 6 2 6 15) 47) 7)
      (check-equal? (lols (list 12 7 15 10 5 4 7 12 12) 84) 9)
      (check-equal? (lols (list 11 4 4 9 10 7 12) 57) 7)
      (check-equal? (lols (list 4 12 15 6 15 1 4 4 2) 63) 9)
      (check-equal? (lols (list 3 13 4 15 1) 36) 5)
      (check-equal? (lols (list 14 3 7 14 7 7 1 6) 59) 8)
      (check-equal? (lols (list 15 13 1 14 6 8) 57) 6)
      (check-equal? (lols (list 14 2 3 10 15) 44) 5)
      (check-equal? (lols (list 5 5 3 7 12 10 11) 53) 7)
      (check-equal? (lols (list 3 7 3 5 3 14 8) 43) 7)
      (check-equal? (lols (list 5 7 9 14 9 14 4 1 4) 67) 9)
      (check-equal? (lols (list 12 7 8 6 3 9 7 3 4 4) 63) 10)
      (check-equal? (lols (list 9 12 1 4 9 6 15 9 7) 72) 9)
      (check-equal? (lols (list 9 13 12 10 4 9 9 4 4 13) 87) 10)
      (check-equal? (lols (list 13 5 6 8 2 13 1 5 6) 59) 9)
      (check-equal? (lols (list 7 9 8 9 9 3 5) 50) 7)
      (check-equal? (lols (list 15 1 14 8 2 1 10 15 15) 81) 9)
      (check-equal? (lols (list 13 14 1 9 12 2) 51) 6)
      (check-equal? (lols (list 13 12 12 13 8 11 3 14 13) 99) 9)
      (check-equal? (lols (list 2 2 1 12 10 7 11 5 5) 55) 9)
      (check-equal? (lols (list 13 10 3 4 10 3) 43) 6)
      (check-equal? (lols (list 8 9 1 5 8 7 6 8) 52) 8)
      (check-equal? (lols (list 10 1 4 10 9 13 14) 61) 7)
      (check-equal? (lols (list 3 14 11 4 7 9 7 6 8 11) 80) 10)
      (check-equal? (lols (list 4 11 6 6 14 12 2 9 1) 65) 9)
      (check-equal? (lols (list 9 2 15 12 15 6 4 12) 75) 8)
      (check-equal? (lols (list 4 3 5 3 2) 17) 5)
      (check-equal? (lols (list 4 3 13 6 9) 35) 5))

