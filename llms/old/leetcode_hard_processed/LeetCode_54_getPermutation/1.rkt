#lang racket

;; The set [1, 2, 3, ..., n] contains a total of n! unique permutations.
;; By listing and labeling all of the permutations in order, we get the following sequence for n = 3:
;; "123"
;; "132"
;; "213"
;; "231"
;; "312"
;; "321"
;; Given n and k, return the kth permutation sequence.
;; Example 1:
;; Input: n = 3, k = 3
;; Output: "213"
;; Example 2:
;; Input: n = 4, k = 9
;; Output: "2314"
;; Example 3:
;; Input: n = 3, k = 1
;; Output: "123"
;; Constraints:
;; 1 <= n <= 9
;; 1 <= k <= n!
(define (getPermutation n k)
  ;; Initialize a list to store factorials and the numbers 1 through n.
  (define factorials (list 1))
  (define nums (build-list n (lambda (i) (+ i 1))))
  ;; Calculate factorials from 1! to (n-1)! and store them in `factorials`.
  (for ([i (in-range 1 n)])
    (set! factorials (append factorials (list (* (last factorials) i)))))
  ;; Adjust k to be zero-based.
  (set! k (- k 1))
  ;; Initialize an empty string to accumulate the result.
  (define result "")
  ;; Iterate to form the kth permutation.
  (for ([i (in-range n 0 -1)])
    ;; Determine the index of the current digit.
    (define index (quotient k (list-ref factorials (- i 1))))
    ;; Update k for the next iteration.
    (set! k (remainder k (list-ref factorials (- i 1))))
    ;; Append the chosen number to the result and remove it from the list.
    (set! result (string-append result (number->string (list-ref nums index))))
    (set! nums (remove* (list (list-ref nums index)) nums)))
  ;; Return the resulting string.
  result)

;; Example usage:
;; (getPermutation 3 3) should return "213"
;; (getPermutation 4 9) should return "2314"
;; (getPermutation 3 1) should return "123"
(require rackunit)

(define (test-humaneval) 

  (let (( candidate getPermutation))
    (check-within (candidate 9 2) "123456798" 0.001)
    (check-within (candidate 9 3) "123456879" 0.001)
    (check-within (candidate 7 1) "1234567" 0.001)
    (check-within (candidate 3 1) "123" 0.001)
    (check-within (candidate 2 2) "21" 0.001)
    (check-within (candidate 9 1) "123456789" 0.001)
    (check-within (candidate 7 5040) "7654321" 0.001)
    (check-within (candidate 6 1) "123456" 0.001)
    (check-within (candidate 4 9) "2314" 0.001)
    (check-within (candidate 4 24) "4321" 0.001)
    (check-within (candidate 8 40320) "87654321" 0.001)
    (check-within (candidate 4 14) "3142" 0.001)
    (check-within (candidate 5 120) "54321" 0.001)
    (check-within (candidate 5 1) "12345" 0.001)
    (check-within (candidate 1 1) "1" 0.001)
    (check-within (candidate 2 1) "12" 0.001)
    (check-within (candidate 4 20) "4132" 0.001)
    (check-within (candidate 4 1) "1234" 0.001)
    (check-within (candidate 5 4) "12453" 0.001)
    (check-within (candidate 4 15) "3214" 0.001)
    (check-within (candidate 9 362880) "987654321" 0.001)
    (check-within (candidate 3 3) "213" 0.001)
))

(test-humaneval)