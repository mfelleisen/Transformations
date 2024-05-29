#lang racket

;; Initially, you have a bank account balance of 100 dollars.
;; You are given an integer purchaseAmount representing the amount you will spend on a purchase in dollars.
;; At the store where you will make the purchase, the purchase amount is rounded to the nearest multiple of 10. In other words, you pay a non-negative amount, roundedAmount, such that roundedAmount is a multiple of 10 and abs(roundedAmount - purchaseAmount) is minimized.
;; If there is more than one nearest multiple of 10, the largest multiple is chosen.
;; Return an integer denoting your account balance after making a purchase worth purchaseAmount dollars from the store.
;; Note: 0 is considered to be a multiple of 10 in this problem.
;; Example 1:
;; Input: purchaseAmount = 9
;; Output: 90
;; Explanation: In this example, the nearest multiple of 10 to 9 is 10. Hence, your account balance becomes 100 - 10 = 90.
;; Example 2:
;; Input: purchaseAmount = 15
;; Output: 80
;; Explanation: In this example, there are two nearest multiples of 10 to 15: 10 and 20. So, the larger multiple, 20, is chosen.
;; Hence, your account balance becomes 100 - 20 = 80.
;; Constraints:
;;  * 0 <= purchaseAmount <= 100
(define (accountBalanceAfterPurchase purchaseAmount)
  (define initial-balance 100)
  
  (define (round-to-nearest-10 amount)
    (define lower-bound (* 10 (quotient amount 10)))
    (define upper-bound (+ lower-bound 10))
    (if (< (- amount lower-bound) (- upper-bound amount))
        lower-bound
        upper-bound))
  
  (define rounded-amount (round-to-nearest-10 purchaseAmount))
  
  (- initial-balance rounded-amount))

;; The function accountBalanceAfterPurchase uses idiomatic Racket features such as
;; defining local functions (round-to-nearest-10) and using conditional expressions
;; to determine values. It avoids mutation by using define for variable bindings and
;; pure functions for calculations.


(require rackunit)


(define (test-humaneval) 

  (let (( candidate accountBalanceAfterPurchase))
    (check-within (candidate 9) 90 0.001)
    (check-within (candidate 15) 80 0.001)
    (check-within (candidate 10) 90 0.001)
    (check-within (candidate 11) 90 0.001)
    (check-within (candidate 12) 90 0.001)
    (check-within (candidate 13) 90 0.001)
    (check-within (candidate 14) 90 0.001)
    (check-within (candidate 16) 80 0.001)
    (check-within (candidate 17) 80 0.001)
    (check-within (candidate 18) 80 0.001)
    (check-within (candidate 19) 80 0.001)
    (check-within (candidate 1) 100 0.001)
    (check-within (candidate 2) 100 0.001)
    (check-within (candidate 3) 100 0.001)
    (check-within (candidate 4) 100 0.001)
    (check-within (candidate 5) 90 0.001)
    (check-within (candidate 6) 90 0.001)
    (check-within (candidate 7) 90 0.001)
    (check-within (candidate 8) 90 0.001)
    (check-within (candidate 20) 80 0.001)
    (check-within (candidate 21) 80 0.001)
    (check-within (candidate 22) 80 0.001)
    (check-within (candidate 23) 80 0.001)
    (check-within (candidate 24) 80 0.001)
    (check-within (candidate 25) 70 0.001)
    (check-within (candidate 26) 70 0.001)
    (check-within (candidate 27) 70 0.001)
    (check-within (candidate 28) 70 0.001)
    (check-within (candidate 29) 70 0.001)
    (check-within (candidate 30) 70 0.001)
    (check-within (candidate 31) 70 0.001)
    (check-within (candidate 32) 70 0.001)
    (check-within (candidate 33) 70 0.001)
    (check-within (candidate 34) 70 0.001)
    (check-within (candidate 35) 60 0.001)
    (check-within (candidate 36) 60 0.001)
    (check-within (candidate 37) 60 0.001)
    (check-within (candidate 38) 60 0.001)
    (check-within (candidate 39) 60 0.001)
    (check-within (candidate 40) 60 0.001)
    (check-within (candidate 41) 60 0.001)
    (check-within (candidate 42) 60 0.001)
    (check-within (candidate 43) 60 0.001)
    (check-within (candidate 44) 60 0.001)
    (check-within (candidate 45) 50 0.001)
    (check-within (candidate 46) 50 0.001)
    (check-within (candidate 47) 50 0.001)
    (check-within (candidate 48) 50 0.001)
    (check-within (candidate 49) 50 0.001)
    (check-within (candidate 50) 50 0.001)
    (check-within (candidate 51) 50 0.001)
    (check-within (candidate 52) 50 0.001)
    (check-within (candidate 53) 50 0.001)
    (check-within (candidate 54) 50 0.001)
    (check-within (candidate 55) 40 0.001)
    (check-within (candidate 56) 40 0.001)
    (check-within (candidate 57) 40 0.001)
    (check-within (candidate 58) 40 0.001)
    (check-within (candidate 59) 40 0.001)
    (check-within (candidate 60) 40 0.001)
    (check-within (candidate 61) 40 0.001)
    (check-within (candidate 62) 40 0.001)
    (check-within (candidate 63) 40 0.001)
    (check-within (candidate 64) 40 0.001)
    (check-within (candidate 65) 30 0.001)
    (check-within (candidate 66) 30 0.001)
    (check-within (candidate 67) 30 0.001)
    (check-within (candidate 68) 30 0.001)
    (check-within (candidate 69) 30 0.001)
    (check-within (candidate 70) 30 0.001)
    (check-within (candidate 71) 30 0.001)
    (check-within (candidate 72) 30 0.001)
    (check-within (candidate 73) 30 0.001)
    (check-within (candidate 74) 30 0.001)
    (check-within (candidate 75) 20 0.001)
    (check-within (candidate 76) 20 0.001)
    (check-within (candidate 77) 20 0.001)
    (check-within (candidate 78) 20 0.001)
    (check-within (candidate 79) 20 0.001)
    (check-within (candidate 80) 20 0.001)
    (check-within (candidate 81) 20 0.001)
    (check-within (candidate 82) 20 0.001)
    (check-within (candidate 83) 20 0.001)
    (check-within (candidate 84) 20 0.001)
    (check-within (candidate 85) 10 0.001)
    (check-within (candidate 86) 10 0.001)
    (check-within (candidate 87) 10 0.001)
    (check-within (candidate 88) 10 0.001)
    (check-within (candidate 89) 10 0.001)
    (check-within (candidate 90) 10 0.001)
    (check-within (candidate 91) 10 0.001)
    (check-within (candidate 92) 10 0.001)
    (check-within (candidate 93) 10 0.001)
    (check-within (candidate 94) 10 0.001)
    (check-within (candidate 95) 0 0.001)
    (check-within (candidate 96) 0 0.001)
    (check-within (candidate 97) 0 0.001)
    (check-within (candidate 98) 0 0.001)
    (check-within (candidate 99) 0 0.001)
    (check-within (candidate 100) 0 0.001)
))

(test-humaneval)