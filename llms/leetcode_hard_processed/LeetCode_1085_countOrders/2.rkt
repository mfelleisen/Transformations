#lang racket

;; Given n orders, each order consist in pickup and delivery services. 
;; Count all valid pickup/delivery possible sequences such that delivery(i) is always after of pickup(i). 
;; Since the answer may be too large, return it modulo 10^9 + 7.
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
  ;; Define the modulo base
  (define MOD 1000000007)
  ;; Initialize the answer with 1 since there's at least one way for 0 orders (the empty sequence)
  (define ans 1)
  ;; Loop from 1 through n (inclusive) to calculate the number of valid sequences
  (for ([i (in-range 1 (add1 n))])
    ;; Update the answer according to the formula that counts the valid sequences.
    ;; The formula is derived from considering the permutations and the fixed positions of deliveries after their respective pickups
    (set! ans (modulo (* ans (* (sub1 (* i 2)) i)) MOD)))
  ;; Return the final answer
  ans)

;; Example usage:
;; (countOrders 1) should return 1
;; (countOrders 2) should return 6
;; (countOrders 3) should return 90
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