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
  ;; Function to find the kth smallest number in a m x n multiplication table.
  
  ;; Initialize the search range for the kth smallest number.
  (let loop ((low 1) (high (* m n)))
    (if (< low high)
        ;; If the search range is valid (low < high), calculate the middle point.
        (let* ((mid (quotient (+ low high) 2))
               ;; Count how many numbers are less than or equal to 'mid'.
               ;; This is done by adding the minimum between 'mid' divided by 'i' and 'n',
               ;; for each row 'i' in the multiplication table.
               (count (for/sum ([i (in-range 1 (add1 m))])
                                (min (quotient mid i) n))))
          ;; Adjust the search range based on the count relative to 'k'.
          (if (< count k)
              ;; If the count is less than 'k', 'mid' is too small. Search the right half.
              (loop (add1 mid) high)
              ;; If the count is greater than or equal to 'k', 'mid' might be the answer. Search the left half.
              (loop low mid)))
        ;; When low and high converge, 'low' is the kth smallest number.
        low)))

;; Example usage:
;; (findKthNumber 3 3 5) should return 3
;; (findKthNumber 2 3 6) should return 6
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