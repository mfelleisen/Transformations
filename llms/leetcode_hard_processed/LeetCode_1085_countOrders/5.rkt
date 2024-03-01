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
  ;; Define the modulo constant
  (define MOD 1000000007)
  ;; Initialize the answer to 1 as the starting point
  (let loop ((i 1) (ans 1))
    (if (> i n)  ;; If i is greater than n, we have finished calculating
        ans      ;; Return the final answer
        ;; Otherwise, recursively calculate the next part of the answer
        (loop (+ i 1) (modulo (* ans (* (- (* 2 i) 1) i)) MOD)))))

;; The countOrders function iterates from 1 to n, updating the answer by multiplying it with (2 * i - 1) * i
;; at each step and taking modulo 10^9 + 7 to ensure the number stays within limits.
;; This formula accounts for all valid pickup and delivery sequences by considering the permutations of pickups
;; and the permutations of deliveries, ensuring that each delivery comes after its corresponding pickup.
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