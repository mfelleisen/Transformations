#lang racket

(require racket/list) ; Required for using the foldl function

;; Given a list nums of positive integers. Your task is to select some subset of nums, multiply each element by an integer and add all these numbers. The list is said to be good if you can obtain a sum of 1 from the list by any possible subset and multiplicand.
;; Return #t if the list is good otherwise return #f.
;; Example 1:
;; Input: nums = [12,5,7,23]
;; Output: true
;; Explanation: Pick numbers 5 and 7.
;; 5*3 + 7*(-2) = 1
;; Example 2:
;; Input: nums = [29,6,10]
;; Output: true
;; Explanation: Pick numbers 29, 6 and 10.
;; 29*1 + 6*(-3) + 10*(-1) = 1
;; Example 3:
;; Input: nums = [3,6]
;; Output: false
;; Constraints:
;; 1 <= nums.length <= 10^5
;; 1 <= nums[i] <= 10^9
(define (is_good_array nums)
  ;; This function checks if it's possible to achieve a sum of 1
  ;; by selecting some subset of nums and multiplying each by an integer.
  ;; This is equivalent to checking if the greatest common divisor (GCD) of
  ;; all numbers in the list is 1, because if we can form 1, then the GCD must be 1.
  (define (gcd a b)
    ;; Recursive function to calculate the GCD of two numbers.
    (if (= b 0)
        a
        (gcd b (modulo a b))))
  (define (gcd-list lst)
    ;; Function to calculate the GCD of a list of numbers.
    ;; It uses foldl to iteratively apply the gcd function to all elements.
    (foldl gcd (first lst) (rest lst)))
  ;; Check if the GCD of the entire list is 1.
  (= (gcd-list nums) 1))

;; Example usage:
;; (is_good_array '(12 5 7 23)) ; should return #t
;; (is_good_array '(29 6 10))   ; should return #t
;; (is_good_array '(3 6))       ; should return #f
(require rackunit)

(define (test-humaneval) 

  (let (( candidate is_good_array))
    (check-within (candidate (list 3 5 7 11 13 17)) #t 0.001)
    (check-within (candidate (list 1 3 5 7 9 11 13 15 17)) #t 0.001)
    (check-within (candidate (list 2 2 2 2)) #f 0.001)
    (check-within (candidate (list 22 24 26 28 30 32 34 36 38 40 42 44 46 48 50)) #f 0.001)
    (check-within (candidate (list 1 1 1 1 1 1 1 1 1)) #t 0.001)
    (check-within (candidate (list 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) #t 0.001)
    (check-within (candidate (list 2 2 2 2 2 2 2 2 2 2 2 2)) #f 0.001)
    (check-within (candidate (list 1 1 1 1 1 1 1 1)) #t 0.001)
    (check-within (candidate (list 3 5 7 11)) #t 0.001)
    (check-within (candidate (list 1 1 1 1 1 1 1)) #t 0.001)
    (check-within (candidate (list 2 4 6 8 10 12)) #f 0.001)
    (check-within (candidate (list 20 22 24 26 28 30 32 34 36 38 40 42 44 46 48 50)) #f 0.001)
    (check-within (candidate (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)) #t 0.001)
    (check-within (candidate (list 1 3 5 7 9)) #t 0.001)
    (check-within (candidate (list 2 4 6 8 10 12 14 16 18 20 22 24)) #f 0.001)
    (check-within (candidate (list 1 2 3 4 5 6 7 8 9 10 11)) #t 0.001)
    (check-within (candidate (list 1 1 1 1)) #t 0.001)
    (check-within (candidate (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)) #t 0.001)
    (check-within (candidate (list 3 5 7 11 13)) #t 0.001)
    (check-within (candidate (list 2 2 2 2 2 2)) #f 0.001)
    (check-within (candidate (list 1 1 1 1 1 1 1 1 1 1 1)) #t 0.001)
    (check-within (candidate (list 1 1 1 1 1)) #t 0.001)
    (check-within (candidate (list 2 2 2 2 2)) #f 0.001)
    (check-within (candidate (list 3 6 9 12 15 18)) #f 0.001)
    (check-within (candidate (list 10 20 30 40)) #f 0.001)
    (check-within (candidate (list 2 3 5 7)) #t 0.001)
    (check-within (candidate (list 1 3 5 7 9 11)) #t 0.001)
    (check-within (candidate (list 2 4 6 8 10 12 14)) #f 0.001)
    (check-within (candidate (list 1 2 3 4 5)) #t 0.001)
    (check-within (candidate (list 1 5 7 11)) #t 0.001)
    (check-within (candidate (list 3 6)) #f 0.001)
    (check-within (candidate (list 5 10 15 20)) #f 0.001)
    (check-within (candidate (list 1 3 5 7 9 11 13 15 17 19)) #t 0.001)
    (check-within (candidate (list 2 4 6 8 10)) #f 0.001)
    (check-within (candidate (list 2 4 6 8)) #f 0.001)
    (check-within (candidate (list 1 2 3 4)) #t 0.001)
    (check-within (candidate (list 1 1 1 1 1 1)) #t 0.001)
    (check-within (candidate (list 13 26 39 52)) #f 0.001)
    (check-within (candidate (list 1 1 1 1 1 1 1 1 1 1)) #t 0.001)
    (check-within (candidate (list 12 5 7 23)) #t 0.001)
    (check-within (candidate (list 2 3 4 5 6 7 8 9 10 11)) #t 0.001)
    (check-within (candidate (list 1 1 1 1 1 1 1 1 1 1 1 1)) #t 0.001)
))

(test-humaneval)