#lang racket

;; Calculates the maximum product of (a XOR x) * (b XOR x) for all 0 <= x < 2^n, modulo 10^9 + 7.
;; Given three integers a, b, and n, return the maximum value of (a XOR x) * (b XOR x) where 0 <= x < 2n.
;; Since the answer may be too large, return it modulo 109 + 7.
;; Note that XOR is the bitwise XOR operation.
;; Example 1:
;; Input: a = 12, b = 5, n = 4
;; Output: 98
;; Explanation: For x = 2, (a XOR x) = 14 and (b XOR x) = 7. Hence, (a XOR x) * (b XOR x) = 98. 
;; It can be shown that 98 is the maximum value of (a XOR x) * (b XOR x) for all 0 <= x < 2n.
;; Example 2:
;; Input: a = 6, b = 7 , n = 5
;; Output: 930
;; Explanation: For x = 25, (a XOR x) = 31 and (b XOR x) = 30. Hence, (a XOR x) * (b XOR x) = 930.
;; It can be shown that 930 is the maximum value of (a XOR x) * (b XOR x) for all 0 <= x < 2n.
;; Example 3:
;; Input: a = 1, b = 6, n = 3
;; Output: 12
;; Explanation: For x = 5, (a XOR x) = 4 and (b XOR x) = 3. Hence, (a XOR x) * (b XOR x) = 12.
;; It can be shown that 12 is the maximum value of (a XOR x) * (b XOR x) for all 0 <= x < 2n.
;; Constraints:
;; 0 <= a, b < 250
;; 0 <= n <= 50
(define (maximumXorProduct a b n)
  (define MOD (+ (expt 10 9) 7))  ; Define the modulo constant
  (define upper-limit (expt 2 n))  ; Calculate the upper limit as 2^n

  (define (calculate-product x)
    (modulo (* (bitwise-xor a x) (bitwise-xor b x)) MOD))

  (for/fold ([max-product 0]) ([x (in-range upper-limit)])
    (max max-product (calculate-product x))))

;; Example usage:
(maximumXorProduct 12 5 4)  ; Output: 98
(maximumXorProduct 6 7 5)   ; Output: 930
(maximumXorProduct 1 6 3)   ; Output: 12

(require rackunit)


(define (test-humaneval) 

  (let (( candidate maximumXorProduct))
    (check-within (candidate 12 5 4) 98 0.001)
    (check-within (candidate 6 7 5) 930 0.001)
    (check-within (candidate 1 6 3) 12 0.001)
    (check-within (candidate 0 0 1) 1 0.001)
    (check-within (candidate 0 1 6) 3906 0.001)
    (check-within (candidate 0 2 7) 15875 0.001)
    (check-within (candidate 0 3 1) 2 0.001)
    (check-within (candidate 0 4 0) 0 0.001)
    (check-within (candidate 0 5 6) 3658 0.001)
    (check-within (candidate 0 6 1) 7 0.001)
    (check-within (candidate 0 7 2) 12 0.001)
    (check-within (candidate 0 8 5) 713 0.001)
    (check-within (candidate 0 9 2) 30 0.001)
    (check-within (candidate 0 10 7) 14875 0.001)
    (check-within (candidate 0 11 4) 84 0.001)
    (check-within (candidate 0 12 2) 45 0.001)
    (check-within (candidate 0 13 2) 42 0.001)
    (check-within (candidate 0 14 0) 0 0.001)
    (check-within (candidate 0 15 6) 3080 0.001)
    (check-within (candidate 1 0 3) 42 0.001)
    (check-within (candidate 1 1 4) 225 0.001)
    (check-within (candidate 1 2 6) 3782 0.001)
    (check-within (candidate 1 3 2) 3 0.001)
    (check-within (candidate 1 4 5) 810 0.001)
    (check-within (candidate 1 5 4) 165 0.001)
    (check-within (candidate 1 6 4) 132 0.001)
    (check-within (candidate 1 7 2) 15 0.001)
    (check-within (candidate 1 8 0) 8 0.001)
    (check-within (candidate 1 9 2) 33 0.001)
    (check-within (candidate 1 10 7) 14756 0.001)
    (check-within (candidate 1 11 7) 14875 0.001)
    (check-within (candidate 1 12 7) 14518 0.001)
    (check-within (candidate 1 13 0) 13 0.001)
    (check-within (candidate 1 14 5) 552 0.001)
    (check-within (candidate 1 15 4) 63 0.001)
    (check-within (candidate 2 0 0) 0 0.001)
    (check-within (candidate 2 1 0) 2 0.001)
    (check-within (candidate 2 2 0) 4 0.001)
    (check-within (candidate 2 3 5) 930 0.001)
    (check-within (candidate 2 4 4) 143 0.001)
    (check-within (candidate 2 5 5) 756 0.001)
    (check-within (candidate 2 6 4) 165 0.001)
    (check-within (candidate 2 7 3) 18 0.001)
    (check-within (candidate 2 8 1) 27 0.001)
    (check-within (candidate 2 9 2) 24 0.001)
    (check-within (candidate 2 10 5) 713 0.001)
    (check-within (candidate 2 11 0) 22 0.001)
    (check-within (candidate 2 12 2) 39 0.001)
    (check-within (candidate 2 13 1) 36 0.001)
    (check-within (candidate 2 14 0) 28 0.001)
    (check-within (candidate 2 15 2) 42 0.001)
    (check-within (candidate 3 0 6) 3782 0.001)
    (check-within (candidate 3 1 6) 3843 0.001)
    (check-within (candidate 3 2 2) 6 0.001)
    (check-within (candidate 3 3 7) 16129 0.001)
    (check-within (candidate 3 4 5) 756 0.001)
    (check-within (candidate 3 5 4) 143 0.001)
    (check-within (candidate 3 6 2) 18 0.001)
    (check-within (candidate 3 7 2) 21 0.001)
    (check-within (candidate 3 8 6) 3300 0.001)
    (check-within (candidate 3 9 1) 27 0.001)
    (check-within (candidate 3 10 5) 690 0.001)
    (check-within (candidate 3 11 1) 33 0.001)
    (check-within (candidate 3 12 1) 36 0.001)
    (check-within (candidate 3 13 2) 39 0.001)
    (check-within (candidate 3 14 0) 42 0.001)
    (check-within (candidate 3 15 6) 3245 0.001)
    (check-within (candidate 4 0 4) 165 0.001)
    (check-within (candidate 4 1 2) 18 0.001)
    (check-within (candidate 4 2 2) 15 0.001)
    (check-within (candidate 4 3 0) 12 0.001)
    (check-within (candidate 4 4 3) 49 0.001)
    (check-within (candidate 4 5 3) 42 0.001)
    (check-within (candidate 4 6 2) 35 0.001)
    (check-within (candidate 4 7 3) 30 0.001)
    (check-within (candidate 4 8 4) 77 0.001)
    (check-within (candidate 4 9 7) 14518 0.001)
    (check-within (candidate 4 10 0) 40 0.001)
    (check-within (candidate 4 11 5) 552 0.001)
    (check-within (candidate 4 12 6) 3465 0.001)
    (check-within (candidate 4 13 7) 14994 0.001)
    (check-within (candidate 4 14 5) 667 0.001)
    (check-within (candidate 4 15 6) 3300 0.001)
    (check-within (candidate 5 0 1) 4 0.001)
    (check-within (candidate 5 1 3) 21 0.001)
    (check-within (candidate 5 2 2) 12 0.001)
    (check-within (candidate 5 3 7) 15375 0.001)
    (check-within (candidate 5 4 7) 16002 0.001)
    (check-within (candidate 5 5 0) 25 0.001)
    (check-within (candidate 5 6 1) 30 0.001)
    (check-within (candidate 5 7 0) 35 0.001)
    (check-within (candidate 5 8 2) 70 0.001)
    (check-within (candidate 5 9 4) 77 0.001)
    (check-within (candidate 5 10 5) 552 0.001)
    (check-within (candidate 5 11 7) 14399 0.001)
    (check-within (candidate 5 12 1) 60 0.001)
    (check-within (candidate 5 13 5) 713 0.001)
    (check-within (candidate 5 14 4) 84 0.001)
    (check-within (candidate 5 15 0) 75 0.001)
    (check-within (candidate 6 0 4) 143 0.001)
))

(test-humaneval)