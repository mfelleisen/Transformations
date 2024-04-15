#lang racket

;; Given n orders, each order consist in pickup and delivery services. 
;; Count all valid pickup/delivery possible sequences such that delivery(i) is always after of pickup(i). 
;; Since the answer may be too large, return it modulo 10^9 + 7.
;; Example 1:
;; Input: n = 1
;; Output: 1
;; Explanation: Unique order (P1, D1), Delivery 1 always is after of Pickup 1.
;; Example 2:
;; Input: n = 2
;; Output: 6
;; Explanation: All possible orders: 
;; (P1,P2,D1,D2), (P1,P2,D2,D1), (P1,D1,P2,D2), (P2,P1,D1,D2), (P2,P1,D2,D1) and (P2,D2,P1,D1).
;; This is an invalid order (P1,D2,P2,D1) because Pickup 2 is after of Delivery 2.
;; Example 3:
;; Input: n = 3
;; Output: 90
;; Constraints:
;; 1 <= n <= 500
(define (countOrders n)
  ;; Define the modulus constant as specified in the problem statement.
  (define MOD 1000000007)
  ;; Start with an initial answer of 1, as the calculation is cumulative.
  (define ans 1)
  ;; Use a loop to calculate the number of valid sequences.
  ;; This loop iterates from 1 through n (inclusive) to calculate the factorial part
  ;; and the permutation part of the formula simultaneously.
  (for ([i (in-range 1 (+ n 1))])
    (set! ans (modulo (* ans (sub1 (* 2 i)) i) MOD)))
  ;; Return the final answer modulo 10^9 + 7.
  ans)
(require rackunit)

(define (test-humaneval) 

  (let (( candidate countOrders))
    (check-within (candidate 6) 7484400 0.001)
    (check-within (candidate 3) 90 0.001)
    (check-within (candidate 7) 681080400 0.001)
    (check-within (candidate 1) 1 0.001)
    (check-within (candidate 5) 113400 0.001)
    (check-within (candidate 4) 2520 0.001)
    (check-within (candidate 2) 6 0.001)
))

(test-humaneval)