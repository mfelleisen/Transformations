#lang racket

;; Given two integers n and k, return the kth lexicographically smallest integer in the range [1, n].
;; Example 1:
;; Input: n = 13, k = 2
;; Output: 10
;; Explanation: The lexicographical order is [1, 10, 11, 12, 13, 2, 3, 4, 5, 6, 7, 8, 9], so the second smallest number is 10.
;; Example 2:
;; Input: n = 1, k = 1
;; Output: 1
;; Constraints:
;; 1 <= k <= n <= 109
(define (find_kth_number n k)
  ;; Convert the range [1, n] to a list of numbers.
  (let ([nums (range 1 (add1 n))])
    ;; Convert each number to its string representation, sort the list lexicographically,
    ;; then convert the strings back to numbers.
    (define sorted-nums
      (map string->number (sort (map number->string nums) string<?)))
    ;; Return the k-th element in the sorted list (adjusting for zero-based indexing).
    (list-ref sorted-nums (sub1 k))))
(require rackunit)

(define (test-humaneval) 

  (let (( candidate find_kth_number))
    (check-within (candidate 9999 9999) 9999 0.001)
    (check-within (candidate 2 1) 1 0.001)
    (check-within (candidate 100 10) 17 0.001)
    (check-within (candidate 10000 1) 1 0.001)
    (check-within (candidate 13 1) 1 0.001)
    (check-within (candidate 1 1) 1 0.001)
    (check-within (candidate 2 2) 2 0.001)
    (check-within (candidate 13 2) 10 0.001)
    (check-within (candidate 1000 1) 1 0.001)
    (check-within (candidate 100 2) 10 0.001)
    (check-within (candidate 100 1) 1 0.001)
    (check-within (candidate 15 1) 1 0.001)
    (check-within (candidate 100000 1) 1 0.001)
))

(test-humaneval)