#lang racket

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
  ;; Using foldl to iterate through the list nums and calculate gcd iteratively.
  ;; The initial value for folding is the first element of nums, and for each element in nums,
  ;; the gcd function is applied between the accumulated result and the current element.
  (let loop ([nums nums] [acc (car nums)])
    (if (null? (cdr nums)) ;; Check if we have reached the end of the list
        (= acc 1) ;; If so, check if the accumulated gcd is 1
        (loop (cdr nums) (gcd acc (cadr nums))))) ;; Continue with the rest of the list
  )

;; The gcd function is not defined here, assuming it's either built-in or defined elsewhere.
;; In Racket, gcd is a built-in function that calculates the greatest common divisor.
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