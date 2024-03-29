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
(define (isGoodArray nums)
  ;; First, define a helper function to compute the greatest common divisor (gcd) of two numbers.
  (define (gcd a b)
    (if (= b 0)
        a
        (gcd b (modulo a b))))
  
  ;; Then, define a function to compute the gcd of a list of numbers.
  (define (gcd-list lst)
    ;; If the list has only one element, return that element.
    ;; Otherwise, recursively compute the gcd of the first element and the gcd of the rest of the list.
    (foldl (lambda (x acc) (gcd x acc)) (first lst) (rest lst)))
  
  ;; Check if the gcd of the entire list is 1.
  ;; If yes, it is possible to select a subset and multiplicands to get a sum of 1, so return #t.
  ;; Otherwise, return #f.
  (= 1 (gcd-list nums)))

;; Example usage:
;; (isGoodArray '(12 5 7 23)) ; Output: #t
;; (isGoodArray '(29 6 10))   ; Output: #t
;; (isGoodArray '(3 6))       ; Output: #f
(require rackunit)

(define (test-humaneval) 

  (let (( candidate isGoodArray))
    (check-within (candidate (list 27 54 81 108)) #f 0.001)
    (check-within (candidate (list 17 34 51 68)) #f 0.001)
    (check-within (candidate (list 13 26 39 52)) #f 0.001)
    (check-within (candidate (list 18 36 54 72)) #f 0.001)
    (check-within (candidate (range 1 10000)) #t 0.001)
    (check-within (candidate (list 29 58 87 116)) #f 0.001)
    (check-within (candidate (list 10 14 20 24 30 34 40 44 50 54)) #f 0.001)
    (check-within (candidate (list 24 48 72 96)) #f 0.001)
    (check-within (candidate (list 12 24 36 48 60 72 84 96 108 120)) #f 0.001)
    (check-within (candidate (list 12 15 18 21 24 27 30 33 36 39)) #f 0.001)
    (check-within (candidate (list 6 12 18 24 30 36 42 48 54 60)) #f 0.001)
    (check-within (candidate (list 11 22 33 44)) #f 0.001)
    (check-within (candidate (list 3 6)) #f 0.001)
    (check-within (candidate (list 29 6 10)) #t 0.001)
    (check-within (candidate (list 3 6 9 12)) #f 0.001)
    (check-within (candidate (list 34 68 102 136)) #f 0.001)
    (check-within (candidate (list 2 2 2 2 2 2 2)) #f 0.001)
    (check-within (candidate (list 22 44 66 88)) #f 0.001)
    (check-within (candidate (list 19 38 57 76)) #f 0.001)
    (check-within (candidate (list 1 2 3)) #t 0.001)
    (check-within (candidate (list 1000 1000 2 3)) #t 0.001)
    (check-within (candidate (list 25 50 75 100)) #f 0.001)
    (check-within (candidate (list 33 66 99 132 165 198 231 264 297 330)) #f 0.001)
    (check-within (candidate (list 21 42 63 84 1 3)) #t 0.001)
    (check-within (candidate (list 1000 1000 2 3 4 6)) #t 0.001)
    (check-within (candidate (list 12 5 7 23)) #t 0.001)
    (check-within (candidate (list 1)) #t 0.001)
    (check-within (candidate (list 10 20 30 40 50)) #f 0.001)
    (check-within (candidate (list 28 56 84 112)) #f 0.001)
    (check-within (candidate (list 30 60 90 120)) #f 0.001)
    (check-within (candidate (list 4 8 16)) #f 0.001)
    (check-within (candidate (list 9 18 27 36)) #f 0.001)
    (check-within (candidate (list 26 52 78 104)) #f 0.001)
    (check-within (candidate (list 21 42 63 84 2 6)) #t 0.001)
    (check-within (candidate (list 15 30 45 60)) #f 0.001)
    (check-within (candidate (list 46 92 138 184 230 276 322 368 414 460 506 552 600)) #f 0.001)
    (check-within (candidate (list 12 24 36 48)) #f 0.001)
    (check-within (candidate (list 20 40 60 80)) #f 0.001)
    (check-within (candidate (list 20 30 40 48)) #f 0.001)
    (check-within (candidate (list 2)) #f 0.001)
    (check-within (candidate (list 7 14 21)) #f 0.001)
    (check-within (candidate (list 21 42 63 84)) #f 0.001)
    (check-within (candidate (list 16 32 48 64)) #f 0.001)
    (check-within (candidate (list 3 6 9 12 15 18 21 24 27 30)) #f 0.001)
    (check-within (candidate (list 14 28 42 56)) #f 0.001)
    (check-within (candidate (list 10 20 30 40)) #f 0.001)
    (check-within (candidate (list 23 46 69 92)) #f 0.001)
))

(test-humaneval)