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
  ;; Define the modulus value as a constant
  (define MOD 1000000007)
  ;; Initialize the answer to 1, as the calculation starts with multiplication
  (let loop ((i 1) (ans 1))
    (if (> i n)
        ans ; Return the final answer when all n orders have been processed
        ;; Recursive call: calculate the next value of ans by multiplying with (2*i - 1)*i
        ;; and take modulo MOD to ensure the result doesn't exceed the limit.
        (loop (+ i 1) (modulo (* ans (* (- (* 2 i) 1) i)) MOD)))))

;; The function iterates from 1 to n, multiplying the answer by (2*i - 1)*i at each step
;; and taking modulo 10^9 + 7 to keep the number within the constraints.
;; This approach calculates the number of valid sequences by considering each pair of pickup and delivery
;; actions as a single combined step, adjusting for the permutations of these actions and their internal order.
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