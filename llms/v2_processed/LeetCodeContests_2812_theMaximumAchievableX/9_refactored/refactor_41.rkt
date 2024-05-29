#lang racket

;; You are given two integers, num and t.
;; An integer x is called achievable if it can become equal to num after applying the following operation no more than t times:
;;  * Increase or decrease x by 1, and simultaneously increase or decrease num by 1.
;; Return the maximum possible achievable number. It can be proven that there exists at least one achievable number.
;; Example 1:
;; Input: num = 4, t = 1
;; Output: 6
;; Explanation: The maximum achievable number is x = 6; it can become equal to num after performing this operation:
;; 1- Decrease x by 1, and increase num by 1. Now, x = 5 and num = 5.
;; It can be proven that there is no achievable number larger than 6.
;; Example 2:
;; Input: num = 3, t = 2
;; Output: 7
;; Explanation: The maximum achievable number is x = 7; after performing these operations, x will equal num:
;; 1- Decrease x by 1, and increase num by 1. Now, x = 6 and num = 4.
;; 2- Decrease x by 1, and increase num by 1. Now, x = 5 and num = 5.
;; It can be proven that there is no achievable number larger than 7.
;; Constraints:
;;  * 1 <= num, t <= 50
(define (theMaximumAchievableX num t)
  ;; The maximum achievable x is calculated as `num + 2 * t`,
  ;; since each operation allows x to diverge by 1 while num compensates by 1 in the opposite direction.
  (+ num (* 2 t)))

;; Example 1:
;; Input: num = 4, t = 1
;; Output: 6
;; Explanation: The maximum achievable number is x = 6; it can become equal to num after performing this operation:
;; 1- Decrease x by 1, and increase num by 1. Now, x = 5 and num = 5.
;; It can be proven that there is no achievable number larger than 6.

;; Example 2:
;; Input: num = 3, t = 2
;; Output: 7
;; Explanation: The maximum achievable number is x = 7; after performing these operations, x will equal num:
;; 1- Decrease x by 1, and increase num by 1. Now, x = 6 and num = 4.
;; 2- Decrease x by 1, and increase num by 1. Now, x = 5 and num = 5.
;; It can be proven that there is no achievable number larger than 7.

;; Constraints:
;; * 1 <= num, t <= 50

;; Testing the function with provided examples
(theMaximumAchievableX 4 1) ;; Output should be 6
(theMaximumAchievableX 3 2) ;; Output should be 7

(require rackunit)


(define (test-humaneval) 

  (let (( candidate theMaximumAchievableX))
    (check-within (candidate 4 1) 6 0.001)
    (check-within (candidate 3 2) 7 0.001)
    (check-within (candidate 1 1) 3 0.001)
    (check-within (candidate 1 2) 5 0.001)
    (check-within (candidate 1 3) 7 0.001)
    (check-within (candidate 1 4) 9 0.001)
    (check-within (candidate 1 5) 11 0.001)
    (check-within (candidate 1 6) 13 0.001)
    (check-within (candidate 1 7) 15 0.001)
    (check-within (candidate 1 8) 17 0.001)
    (check-within (candidate 1 9) 19 0.001)
    (check-within (candidate 1 10) 21 0.001)
    (check-within (candidate 1 11) 23 0.001)
    (check-within (candidate 1 12) 25 0.001)
    (check-within (candidate 1 13) 27 0.001)
    (check-within (candidate 1 14) 29 0.001)
    (check-within (candidate 1 15) 31 0.001)
    (check-within (candidate 1 16) 33 0.001)
    (check-within (candidate 1 17) 35 0.001)
    (check-within (candidate 1 18) 37 0.001)
    (check-within (candidate 1 19) 39 0.001)
    (check-within (candidate 1 20) 41 0.001)
    (check-within (candidate 1 21) 43 0.001)
    (check-within (candidate 1 22) 45 0.001)
    (check-within (candidate 1 23) 47 0.001)
    (check-within (candidate 1 24) 49 0.001)
    (check-within (candidate 1 25) 51 0.001)
    (check-within (candidate 1 26) 53 0.001)
    (check-within (candidate 1 27) 55 0.001)
    (check-within (candidate 1 28) 57 0.001)
    (check-within (candidate 1 29) 59 0.001)
    (check-within (candidate 1 30) 61 0.001)
    (check-within (candidate 1 31) 63 0.001)
    (check-within (candidate 1 32) 65 0.001)
    (check-within (candidate 1 33) 67 0.001)
    (check-within (candidate 1 34) 69 0.001)
    (check-within (candidate 1 35) 71 0.001)
    (check-within (candidate 1 36) 73 0.001)
    (check-within (candidate 1 37) 75 0.001)
    (check-within (candidate 1 38) 77 0.001)
    (check-within (candidate 1 39) 79 0.001)
    (check-within (candidate 1 40) 81 0.001)
    (check-within (candidate 1 41) 83 0.001)
    (check-within (candidate 1 42) 85 0.001)
    (check-within (candidate 1 43) 87 0.001)
    (check-within (candidate 1 44) 89 0.001)
    (check-within (candidate 1 45) 91 0.001)
    (check-within (candidate 1 46) 93 0.001)
    (check-within (candidate 1 47) 95 0.001)
    (check-within (candidate 1 48) 97 0.001)
    (check-within (candidate 1 49) 99 0.001)
    (check-within (candidate 1 50) 101 0.001)
    (check-within (candidate 2 1) 4 0.001)
    (check-within (candidate 2 2) 6 0.001)
    (check-within (candidate 2 3) 8 0.001)
    (check-within (candidate 2 4) 10 0.001)
    (check-within (candidate 2 5) 12 0.001)
    (check-within (candidate 2 6) 14 0.001)
    (check-within (candidate 2 7) 16 0.001)
    (check-within (candidate 2 8) 18 0.001)
    (check-within (candidate 2 9) 20 0.001)
    (check-within (candidate 2 10) 22 0.001)
    (check-within (candidate 2 11) 24 0.001)
    (check-within (candidate 2 12) 26 0.001)
    (check-within (candidate 2 13) 28 0.001)
    (check-within (candidate 2 14) 30 0.001)
    (check-within (candidate 2 15) 32 0.001)
    (check-within (candidate 2 16) 34 0.001)
    (check-within (candidate 2 17) 36 0.001)
    (check-within (candidate 2 18) 38 0.001)
    (check-within (candidate 2 19) 40 0.001)
    (check-within (candidate 2 20) 42 0.001)
    (check-within (candidate 2 21) 44 0.001)
    (check-within (candidate 2 22) 46 0.001)
    (check-within (candidate 2 23) 48 0.001)
    (check-within (candidate 2 24) 50 0.001)
    (check-within (candidate 2 25) 52 0.001)
    (check-within (candidate 2 26) 54 0.001)
    (check-within (candidate 2 27) 56 0.001)
    (check-within (candidate 2 28) 58 0.001)
    (check-within (candidate 2 29) 60 0.001)
    (check-within (candidate 2 30) 62 0.001)
    (check-within (candidate 2 31) 64 0.001)
    (check-within (candidate 2 32) 66 0.001)
    (check-within (candidate 2 33) 68 0.001)
    (check-within (candidate 2 34) 70 0.001)
    (check-within (candidate 2 35) 72 0.001)
    (check-within (candidate 2 36) 74 0.001)
    (check-within (candidate 2 37) 76 0.001)
    (check-within (candidate 2 38) 78 0.001)
    (check-within (candidate 2 39) 80 0.001)
    (check-within (candidate 2 40) 82 0.001)
    (check-within (candidate 2 41) 84 0.001)
    (check-within (candidate 2 42) 86 0.001)
    (check-within (candidate 2 43) 88 0.001)
    (check-within (candidate 2 44) 90 0.001)
    (check-within (candidate 2 45) 92 0.001)
    (check-within (candidate 2 46) 94 0.001)
    (check-within (candidate 2 47) 96 0.001)
    (check-within (candidate 2 48) 98 0.001)
))

(test-humaneval)