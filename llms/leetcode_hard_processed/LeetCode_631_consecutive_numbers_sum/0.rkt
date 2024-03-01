#lang racket

;; Function to calculate the number of ways to write n as the sum of consecutive positive integers.
;; Given an integer n, return the number of ways you can write n as the sum of consecutive positive integers.
;; Example 1:
;; Input: n = 5
;; Output: 2
;; Explanation: 5 = 2 + 3
;; Example 2:
;; Input: n = 9
;; Output: 3
;; Explanation: 9 = 4 + 5 = 2 + 3 + 4
;; Example 3:
;; Input: n = 15
;; Output: 4
;; Explanation: 15 = 8 + 7 = 4 + 5 + 6 = 1 + 2 + 3 + 4 + 5
;; Constraints:
;; 1 <= n <= 109
(define (consecutive_numbers_sum n)
  ;; Initialize count to 0. This will keep track of the total ways found.
  (let loop ((count 0)
             (i 1)) ; Start with i = 1, which represents the starting number of the sequence.
    ;; Check if the sum of integers from 1 to i-1 is less than n, which is necessary to form a valid sequence.
    (if (< (* i (- i 1)) (* 2 n))
        ;; Check if subtracting the sum of integers from 1 to i-1 from n, and then dividing by i, gives a whole number.
        ;; This indicates that n can be expressed as a sum of i consecutive numbers starting from some number.
        (if (zero? (modulo (- n (* i (- i 1) 1/2)) i))
            ;; If yes, increment count and continue checking with the next value of i.
            (loop (+ count 1) (+ i 1))
            ;; Else, just continue checking with the next value of i without incrementing count.
            (loop count (+ i 1)))
        ;; If the sum of integers from 1 to i-1 is not less than n, return the count found so far.
        count)))

;; The function uses a named let loop for iteration, starting with i=1 and incrementing i in each iteration.
;; It checks whether n can be represented as a sum of i consecutive numbers for each i by using a mathematical approach.
;; When the condition `(< (* i (- i 1)) (* 2 n))` is no longer true, it means all possible sequences have been checked, and the function returns the count.
(require rackunit)

(define (test-humaneval) 

  (let (( candidate consecutive_numbers_sum))
    (check-within (candidate 674) 2 0.001)
    (check-within (candidate 1) 1 0.001)
    (check-within (candidate 21) 4 0.001)
    (check-within (candidate 33) 4 0.001)
    (check-within (candidate 54) 4 0.001)
    (check-within (candidate 12) 2 0.001)
    (check-within (candidate 100) 3 0.001)
    (check-within (candidate 998) 2 0.001)
    (check-within (candidate 22) 2 0.001)
    (check-within (candidate 41) 2 0.001)
    (check-within (candidate 17) 2 0.001)
    (check-within (candidate 30) 4 0.001)
    (check-within (candidate 2) 1 0.001)
    (check-within (candidate 15) 4 0.001)
    (check-within (candidate 62) 2 0.001)
    (check-within (candidate 26) 2 0.001)
    (check-within (candidate 37) 2 0.001)
    (check-within (candidate 43) 2 0.001)
    (check-within (candidate 427) 4 0.001)
    (check-within (candidate 25) 3 0.001)
    (check-within (candidate 16) 1 0.001)
    (check-within (candidate 31) 2 0.001)
    (check-within (candidate 7) 2 0.001)
    (check-within (candidate 102) 4 0.001)
    (check-within (candidate 11) 2 0.001)
    (check-within (candidate 19) 2 0.001)
    (check-within (candidate 23) 2 0.001)
    (check-within (candidate 42) 4 0.001)
    (check-within (candidate 3) 2 0.001)
    (check-within (candidate 40) 2 0.001)
    (check-within (candidate 133) 4 0.001)
    (check-within (candidate 35) 4 0.001)
    (check-within (candidate 49) 3 0.001)
    (check-within (candidate 9) 3 0.001)
    (check-within (candidate 132) 4 0.001)
    (check-within (candidate 32) 1 0.001)
    (check-within (candidate 20) 2 0.001)
    (check-within (candidate 103) 2 0.001)
    (check-within (candidate 18) 3 0.001)
    (check-within (candidate 10) 2 0.001)
    (check-within (candidate 5) 2 0.001)
    (check-within (candidate 6) 2 0.001)
    (check-within (candidate 74) 2 0.001)
    (check-within (candidate 34) 2 0.001)
    (check-within (candidate 88) 2 0.001)
    (check-within (candidate 53) 2 0.001)
    (check-within (candidate 13) 2 0.001)
    (check-within (candidate 8) 1 0.001)
    (check-within (candidate 14) 2 0.001)
    (check-within (candidate 36) 3 0.001)
    (check-within (candidate 388) 2 0.001)
    (check-within (candidate 64) 1 0.001)
    (check-within (candidate 38) 2 0.001)
    (check-within (candidate 29) 2 0.001)
    (check-within (candidate 28) 2 0.001)
    (check-within (candidate 52) 2 0.001)
    (check-within (candidate 4) 1 0.001)
    (check-within (candidate 45) 6 0.001)
))

(test-humaneval)