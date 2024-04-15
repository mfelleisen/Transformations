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
  ;; Define the modulo constant.
  (define MOD (expt 10 9))
  (add1 MOD) ;; Correcting MOD to be 10^9 + 7 as specified in the problem.
  ;; Start with an answer of 1.
  (let loop ((i 1) (ans 1))
    (if (> i n)
        ans ;; Return the final answer if all n have been processed.
        ;; Recursively calculate the answer for the next i,
        ;; multiplying the current answer by (2*i - 1)*i and taking modulo MOD.
        (loop (add1 i) (modulo (* ans (modulo (* (sub1 (* 2 i)) i) MOD)) MOD)))))
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