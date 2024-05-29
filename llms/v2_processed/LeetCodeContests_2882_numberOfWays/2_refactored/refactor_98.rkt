#lang racket

;; Given two positive integers n and x, this function computes the number of ways
;; n can be expressed as the sum of the xth power of unique positive integers.
;; The result is returned modulo 10^9 + 7.
;; Given two positive integers n and x.
;; Return the number of ways n can be expressed as the sum of the xth power of unique positive integers, in other words, the number of sets of unique integers [n1, n2, ..., nk] where n = n1x + n2x + ... + nkx.
;; Since the result can be very large, return it modulo 109 + 7.
;; For example, if n = 160 and x = 3, one way to express n is n = 23 + 33 + 53.
;; Example 1:
;; Input: n = 10, x = 2
;; Output: 1
;; Explanation: We can express n as the following: n = 32 + 12 = 10.
;; It can be shown that it is the only way to express 10 as the sum of the 2nd power of unique integers.
;; Example 2:
;; Input: n = 4, x = 1
;; Output: 2
;; Explanation: We can express n in the following ways:
;; - n = 41 = 4.
;; - n = 31 + 11 = 4.
;; Constraints:
;;  * 1 <= n <= 300
;;  * 1 <= x <= 5
(define (numberOfWays n x)
  (define MOD (+ (expt 10 9) 7))  ; Define the modulus

  ;; Calculate the maximum base such that base^x <= n
  (define max-base (inexact->exact (floor (expt n (/ 1.0 x)))))

  ;; Function to compute the number of ways using dynamic programming
  (define (compute-ways n x max-base)
    ;; Initialize the dp vector with 1 way to make 0 and 0 ways for all other values
    (define dp (build-vector (+ n 1) (lambda (i) (if (= i 0) 1 0))))

    (for ([base (in-range 1 (+ max-base 1))])
      (define current-power (expt base x))
      ;; Update dp values from n down to current-power
      (for ([i (in-range n current-power -1)])
        (vector-set! dp i
                     (modulo (+ (vector-ref dp i)
                                (vector-ref dp (- i current-power)))
                             MOD))))

    ;; Return the number of ways to express n
    (vector-ref dp n))

  ;; Compute and return the result
  (compute-ways n x max-base))

;; Example usages
(numberOfWays 10 2)  ; Output: 1
(numberOfWays 4 1)   ; Output: 2

(require rackunit)


(define (test-humaneval) 

  (let (( candidate numberOfWays))
    (check-within (candidate 10 2) 1 0.001)
    (check-within (candidate 4 1) 2 0.001)
    (check-within (candidate 1 1) 1 0.001)
    (check-within (candidate 1 2) 1 0.001)
    (check-within (candidate 1 3) 1 0.001)
    (check-within (candidate 1 4) 1 0.001)
    (check-within (candidate 1 5) 1 0.001)
    (check-within (candidate 2 1) 1 0.001)
    (check-within (candidate 2 2) 0 0.001)
    (check-within (candidate 2 3) 0 0.001)
    (check-within (candidate 2 4) 0 0.001)
    (check-within (candidate 2 5) 0 0.001)
    (check-within (candidate 3 1) 2 0.001)
    (check-within (candidate 3 2) 0 0.001)
    (check-within (candidate 3 3) 0 0.001)
    (check-within (candidate 3 4) 0 0.001)
    (check-within (candidate 3 5) 0 0.001)
    (check-within (candidate 4 2) 1 0.001)
    (check-within (candidate 4 3) 0 0.001)
    (check-within (candidate 4 4) 0 0.001)
    (check-within (candidate 4 5) 0 0.001)
    (check-within (candidate 5 1) 3 0.001)
    (check-within (candidate 5 2) 1 0.001)
    (check-within (candidate 5 3) 0 0.001)
    (check-within (candidate 5 4) 0 0.001)
    (check-within (candidate 5 5) 0 0.001)
    (check-within (candidate 6 1) 4 0.001)
    (check-within (candidate 6 2) 0 0.001)
    (check-within (candidate 6 3) 0 0.001)
    (check-within (candidate 6 4) 0 0.001)
    (check-within (candidate 6 5) 0 0.001)
    (check-within (candidate 7 1) 5 0.001)
    (check-within (candidate 7 2) 0 0.001)
    (check-within (candidate 7 3) 0 0.001)
    (check-within (candidate 7 4) 0 0.001)
    (check-within (candidate 7 5) 0 0.001)
    (check-within (candidate 8 1) 6 0.001)
    (check-within (candidate 8 2) 0 0.001)
    (check-within (candidate 8 3) 1 0.001)
    (check-within (candidate 8 4) 0 0.001)
    (check-within (candidate 8 5) 0 0.001)
    (check-within (candidate 9 1) 8 0.001)
    (check-within (candidate 9 2) 1 0.001)
    (check-within (candidate 9 3) 1 0.001)
    (check-within (candidate 9 4) 0 0.001)
    (check-within (candidate 9 5) 0 0.001)
    (check-within (candidate 10 1) 10 0.001)
    (check-within (candidate 10 3) 0 0.001)
    (check-within (candidate 10 4) 0 0.001)
    (check-within (candidate 10 5) 0 0.001)
    (check-within (candidate 11 1) 12 0.001)
    (check-within (candidate 11 2) 0 0.001)
    (check-within (candidate 11 3) 0 0.001)
    (check-within (candidate 11 4) 0 0.001)
    (check-within (candidate 11 5) 0 0.001)
    (check-within (candidate 12 1) 15 0.001)
    (check-within (candidate 12 2) 0 0.001)
    (check-within (candidate 12 3) 0 0.001)
    (check-within (candidate 12 4) 0 0.001)
    (check-within (candidate 12 5) 0 0.001)
    (check-within (candidate 13 1) 18 0.001)
    (check-within (candidate 13 2) 1 0.001)
    (check-within (candidate 13 3) 0 0.001)
    (check-within (candidate 13 4) 0 0.001)
    (check-within (candidate 13 5) 0 0.001)
    (check-within (candidate 14 1) 22 0.001)
    (check-within (candidate 14 2) 1 0.001)
    (check-within (candidate 14 3) 0 0.001)
    (check-within (candidate 14 4) 0 0.001)
    (check-within (candidate 14 5) 0 0.001)
    (check-within (candidate 15 1) 27 0.001)
    (check-within (candidate 15 2) 0 0.001)
    (check-within (candidate 15 3) 0 0.001)
    (check-within (candidate 15 4) 0 0.001)
    (check-within (candidate 15 5) 0 0.001)
    (check-within (candidate 16 1) 32 0.001)
    (check-within (candidate 16 2) 1 0.001)
    (check-within (candidate 16 3) 0 0.001)
    (check-within (candidate 16 4) 1 0.001)
    (check-within (candidate 16 5) 0 0.001)
    (check-within (candidate 17 1) 38 0.001)
    (check-within (candidate 17 2) 1 0.001)
    (check-within (candidate 17 3) 0 0.001)
    (check-within (candidate 17 4) 1 0.001)
    (check-within (candidate 17 5) 0 0.001)
    (check-within (candidate 18 1) 46 0.001)
    (check-within (candidate 18 2) 0 0.001)
    (check-within (candidate 18 3) 0 0.001)
    (check-within (candidate 18 4) 0 0.001)
    (check-within (candidate 18 5) 0 0.001)
    (check-within (candidate 19 1) 54 0.001)
    (check-within (candidate 19 2) 0 0.001)
    (check-within (candidate 19 3) 0 0.001)
    (check-within (candidate 19 4) 0 0.001)
    (check-within (candidate 19 5) 0 0.001)
    (check-within (candidate 20 1) 64 0.001)
    (check-within (candidate 20 2) 1 0.001)
    (check-within (candidate 20 3) 0 0.001)
    (check-within (candidate 20 4) 0 0.001)
    (check-within (candidate 20 5) 0 0.001)
))

(test-humaneval)