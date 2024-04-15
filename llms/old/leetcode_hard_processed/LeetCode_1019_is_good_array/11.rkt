#lang racket

(require racket/list) ; Required for foldl function

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
  ;; Define a helper function to compute the greatest common divisor (gcd) of two numbers.
  (define (gcd a b)
    (if (= b 0)
        a
        (gcd b (modulo a b))))
  
  ;; Define a function to compute the gcd of all elements in the list nums.
  ;; It uses foldl to iteratively apply the gcd function to elements of nums,
  ;; starting with the first element of nums as the initial value.
  (define (gcd-list lst)
    (foldl gcd (first lst) (rest lst)))
  
  ;; Check if the gcd of all numbers in nums is 1.
  ;; If it is, then it's possible to form a subset that sums to 1, hence the list is "good".
  (= 1 (gcd-list nums)))

;; Examples:
(is_good_array '(12 5 7 23)) ; #t
(is_good_array '(29 6 10))   ; #t
(is_good_array '(3 6))       ; #f
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