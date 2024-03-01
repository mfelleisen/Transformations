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
  ;; First, generate a list of numbers from 1 to n using `range`
  (let ([nums (range 1 (add1 n))])
    ;; Then, sort this list lexicographically. This is done by converting each number to a string
    ;; and using `string<?` for comparison in `sort`.
    (let ([sorted-nums (sort nums (lambda (x y) (string<? (number->string x) (number->string y))))])
      ;; Finally, retrieve the k-th element from this sorted list.
      ;; Since Racket lists are 0-indexed, we subtract 1 from k.
      (list-ref sorted-nums (- k 1)))))

;; The function `find_kth_number` takes two integers `n` and `k`, generates a list of numbers from 1 to `n`,
;; sorts them lexicographically (as per their string representations), and returns the `k`-th number in this sorted list.
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