#lang racket

;; You are given two positive integers low and high.
;; An integer x consisting of 2 * n digits is symmetric if the sum of the first n digits of x is equal to the sum of the last n digits of x. Numbers with an odd number of digits are never symmetric.
;; Return the number of symmetric integers in the range [low, high].
;; Example 1:
;; Input: low = 1, high = 100
;; Output: 9
;; Explanation: There are 9 symmetric integers between 1 and 100: 11, 22, 33, 44, 55, 66, 77, 88, and 99.
;; Example 2:
;; Input: low = 1200, high = 1230
;; Output: 4
;; Explanation: There are 4 symmetric integers between 1200 and 1230: 1203, 1212, 1221, and 1230.
;; Constraints:
;;  * 1 <= low <= high <= 104
(define (countSymmetricIntegers low high)
  ;; Helper function to check if a number is symmetric
  (define (is-symmetric? num)
    (define s (number->string num))  ; Convert number to string
    (define n (string-length s))     ; Get the length of the string
    (if (odd? n)                     ; Check if the number of digits is odd
        #f                           ; Return false if odd
        (let* ((mid (/ n 2))         ; Calculate the midpoint
               (first-half (substring s 0 mid))  ; Get the first half of the digits
               (second-half (substring s mid n))) ; Get the second half of the digits
          (= (sum-digits first-half) (sum-digits second-half))))) ; Check if sums are equal

  ;; Helper function to sum the digits of a string representing a number
  (define (sum-digits str)
    (for/sum ([char (in-string str)])
      (- (char->integer char) (char->integer #\0)))) ; Sum the integer values of chars

  ;; Main computation: count symmetric numbers in the given range
  (for/sum ([num (in-range low (add1 high))])
    (if (is-symmetric? num) 1 0)))

;; Example usage:
(countSymmetricIntegers 1 100)     ; Output: 9
(countSymmetricIntegers 1200 1230)  ; Output: 4

(require rackunit)


(define (test-humaneval) 

  (let (( candidate countSymmetricIntegers))
    (check-within (candidate 1 100) 9 0.001)
    (check-within (candidate 1200 1230) 4 0.001)
    (check-within (candidate 1 1) 0 0.001)
    (check-within (candidate 1 2) 0 0.001)
    (check-within (candidate 1 3) 0 0.001)
    (check-within (candidate 1 4) 0 0.001)
    (check-within (candidate 1 5) 0 0.001)
    (check-within (candidate 1 6) 0 0.001)
    (check-within (candidate 1 7) 0 0.001)
    (check-within (candidate 1 8) 0 0.001)
    (check-within (candidate 1 9) 0 0.001)
    (check-within (candidate 1 10) 0 0.001)
    (check-within (candidate 1 11) 1 0.001)
    (check-within (candidate 1 12) 1 0.001)
    (check-within (candidate 1 13) 1 0.001)
    (check-within (candidate 1 14) 1 0.001)
    (check-within (candidate 1 15) 1 0.001)
    (check-within (candidate 1 16) 1 0.001)
    (check-within (candidate 1 17) 1 0.001)
    (check-within (candidate 1 18) 1 0.001)
    (check-within (candidate 1 19) 1 0.001)
    (check-within (candidate 1 20) 1 0.001)
    (check-within (candidate 1 21) 1 0.001)
    (check-within (candidate 1 22) 2 0.001)
    (check-within (candidate 1 23) 2 0.001)
    (check-within (candidate 1 24) 2 0.001)
    (check-within (candidate 1 25) 2 0.001)
    (check-within (candidate 1 26) 2 0.001)
    (check-within (candidate 1 27) 2 0.001)
    (check-within (candidate 1 28) 2 0.001)
    (check-within (candidate 1 29) 2 0.001)
    (check-within (candidate 1 30) 2 0.001)
    (check-within (candidate 1 31) 2 0.001)
    (check-within (candidate 1 32) 2 0.001)
    (check-within (candidate 1 33) 3 0.001)
    (check-within (candidate 1 34) 3 0.001)
    (check-within (candidate 1 35) 3 0.001)
    (check-within (candidate 1 36) 3 0.001)
    (check-within (candidate 1 37) 3 0.001)
    (check-within (candidate 1 38) 3 0.001)
    (check-within (candidate 1 39) 3 0.001)
    (check-within (candidate 1 40) 3 0.001)
    (check-within (candidate 1 41) 3 0.001)
    (check-within (candidate 1 42) 3 0.001)
    (check-within (candidate 1 43) 3 0.001)
    (check-within (candidate 1 44) 4 0.001)
    (check-within (candidate 1 45) 4 0.001)
    (check-within (candidate 1 46) 4 0.001)
    (check-within (candidate 1 47) 4 0.001)
    (check-within (candidate 1 48) 4 0.001)
    (check-within (candidate 100 1782) 44 0.001)
    (check-within (candidate 1 49) 4 0.001)
    (check-within (candidate 1 50) 4 0.001)
    (check-within (candidate 1 51) 4 0.001)
    (check-within (candidate 1 52) 4 0.001)
    (check-within (candidate 1 53) 4 0.001)
    (check-within (candidate 1 54) 4 0.001)
    (check-within (candidate 1 55) 5 0.001)
    (check-within (candidate 1 56) 5 0.001)
    (check-within (candidate 1 57) 5 0.001)
    (check-within (candidate 1 58) 5 0.001)
    (check-within (candidate 1 59) 5 0.001)
    (check-within (candidate 1 60) 5 0.001)
    (check-within (candidate 2 2) 0 0.001)
    (check-within (candidate 2 3) 0 0.001)
    (check-within (candidate 2 4) 0 0.001)
    (check-within (candidate 2 5) 0 0.001)
    (check-within (candidate 2 6) 0 0.001)
    (check-within (candidate 2 7) 0 0.001)
    (check-within (candidate 2 8) 0 0.001)
    (check-within (candidate 2 9) 0 0.001)
    (check-within (candidate 2 10) 0 0.001)
    (check-within (candidate 2 11) 1 0.001)
    (check-within (candidate 2 12) 1 0.001)
    (check-within (candidate 2 13) 1 0.001)
    (check-within (candidate 2 14) 1 0.001)
    (check-within (candidate 2 15) 1 0.001)
    (check-within (candidate 2 16) 1 0.001)
    (check-within (candidate 2 17) 1 0.001)
    (check-within (candidate 2 18) 1 0.001)
    (check-within (candidate 2 19) 1 0.001)
    (check-within (candidate 2 20) 1 0.001)
    (check-within (candidate 2 21) 1 0.001)
    (check-within (candidate 2 22) 2 0.001)
    (check-within (candidate 2 23) 2 0.001)
    (check-within (candidate 2 24) 2 0.001)
    (check-within (candidate 2 25) 2 0.001)
    (check-within (candidate 2 26) 2 0.001)
    (check-within (candidate 2 27) 2 0.001)
    (check-within (candidate 2 28) 2 0.001)
    (check-within (candidate 2 29) 2 0.001)
    (check-within (candidate 2 30) 2 0.001)
    (check-within (candidate 2 31) 2 0.001)
    (check-within (candidate 2 32) 2 0.001)
    (check-within (candidate 2 33) 3 0.001)
    (check-within (candidate 2 34) 3 0.001)
    (check-within (candidate 2 35) 3 0.001)
    (check-within (candidate 2 36) 3 0.001)
    (check-within (candidate 2 37) 3 0.001)
    (check-within (candidate 2 38) 3 0.001)
))

(test-humaneval)