#lang racket

;; Nearly everyone has used the Multiplication Table. The multiplication table of size m x n is an integer matrix mat where mat[i][j] == i * j (1-indexed).
;; Given three integers m, n, and k, return the kth smallest element in the m x n multiplication table.
;; Example 1:
;; Input: m = 3, n = 3, k = 5
;; Output: 3
;; Explanation: The 5th smallest number is 3.
;; Example 2:
;; Input: m = 2, n = 3, k = 6
;; Output: 6
;; Explanation: The 6th smallest number is 6.
;; Constraints:
;; 1 <= m, n <= 3 * 104
;; 1 <= k <= m * n
(define (findKthNumber m n k)
  ;; Initialize the search range from 1 to m * n
  (let loop ((low 1) (high (* m n)))
    (if (< low high)
        (let* ((mid (quotient (+ low high) 2)) ;; Calculate the mid-point of the current search range
               ;; Count how many numbers are less than or equal to mid
               (count (for/sum ([i (in-range 1 (add1 m))])
                               (min (quotient mid i) n))))
          (if (< count k)
              (loop (add1 mid) high) ;; If count is less than k, search the higher half
              (loop low mid))) ;; Otherwise, search the lower half
        low))) ;; When low meets high, we've found the kth number
(require rackunit)

(define (test-humaneval) 

  (let (( candidate findKthNumber))
    (check-within (candidate 7 8 58) 56 0.001)
    (check-within (candidate 1 10 1) 1 0.001)
    (check-within (candidate 3 2 6) 6 0.001)
    (check-within (candidate 30000 30000 1) 1 0.001)
    (check-within (candidate 1 1 1) 1 0.001)
    (check-within (candidate 2 2 4) 4 0.001)
    (check-within (candidate 3 3 1) 1 0.001)
    (check-within (candidate 7 8 56) 56 0.001)
    (check-within (candidate 7 8 57) 56 0.001)
    (check-within (candidate 2 3 1) 1 0.001)
    (check-within (candidate 1 30000 30000) 30000 0.001)
    (check-within (candidate 6 6 36) 36 0.001)
    (check-within (candidate 10 10 100) 100 0.001)
    (check-within (candidate 7 7 49) 49 0.001)
    (check-within (candidate 5 5 1) 1 0.001)
    (check-within (candidate 2 1 1) 1 0.001)
    (check-within (candidate 1 2 1) 1 0.001)
    (check-within (candidate 3 3 2) 2 0.001)
    (check-within (candidate 2 3 2) 2 0.001)
    (check-within (candidate 1 2 2) 2 0.001)
    (check-within (candidate 2 2 2) 2 0.001)
    (check-within (candidate 7 7 45) 35 0.001)
    (check-within (candidate 7 8 59) 56 0.001)
    (check-within (candidate 10 10 1) 1 0.001)
    (check-within (candidate 2 3 6) 6 0.001)
    (check-within (candidate 10 1 10) 10 0.001)
    (check-within (candidate 1 10 10) 10 0.001)
    (check-within (candidate 2 1 2) 2 0.001)
    (check-within (candidate 30000 1 30000) 30000 0.001)
    (check-within (candidate 3 3 5) 3 0.001)
    (check-within (candidate 5 5 25) 25 0.001)
    (check-within (candidate 2 3 7) 6 0.001)
    (check-within (candidate 7 7 48) 42 0.001)
    (check-within (candidate 3 3 9) 9 0.001)
))

(test-humaneval)