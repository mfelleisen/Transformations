#lang racket

;; Given two integers n and k, return the kth lexicographically smallest integer in the range [1, n].
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
  ;; Create a list of integers from 1 to n.
  (let ((nums (range 1 (+ n 1))))
    ;; Sort the list lexicographically by converting each number to a string.
    (let ((sorted-nums (sort nums string<? #:key number->string)))
      ;; Return the k-th element in the sorted list, adjusting for 0-based indexing.
      (list-ref sorted-nums (- k 1)))))

;; The `range` function generates a list of numbers from 1 to n (inclusive).
;; The `sort` function sorts the list. We use `string<?` for lexicographic comparison,
;; and `#:key number->string` to convert each number to a string before comparison.
;; `list-ref` is used to access the k-th element (considering Racket uses 0-based indexing, hence `- k 1`).
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