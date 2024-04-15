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
  ;; Create a list from 1 to n
  (let ([nums (range 1 (add1 n))])
    ;; Sort the list lexicographically by converting numbers to strings
    (define sorted-nums (sort nums string<? #:key number->string))
    ;; Return the kth element (subtract 1 from k for 0-based indexing)
    (list-ref sorted-nums (sub1 k))))

;; The function `range` generates a list of numbers from 1 to n (inclusive of 1, exclusive of n+1).
;; `sort` is used with `string<?` as the comparison function and `number->string` as the key function
;; to sort the numbers lexicographically. `list-ref` is used to access the k-1 index of the sorted list,
;; because Racket lists are 0-indexed, unlike Python which is 1-indexed for list access.
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