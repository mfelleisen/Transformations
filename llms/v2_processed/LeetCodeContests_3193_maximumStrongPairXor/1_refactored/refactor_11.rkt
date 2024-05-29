#lang racket

;; You are given a 0-indexed integer array nums. A pair of integers x and y is called a strong pair if it satisfies the condition:
;; |x - y| <= min(x, y)
;; You need to select two integers from nums such that they form a strong pair and their bitwise XOR is the maximum among all strong pairs in the array.
;; Return the maximum XOR value out of all possible strong pairs in the array nums.
;; Note that you can pick the same integer twice to form a pair.
;; Example 1:
;; Input: nums = [1,2,3,4,5]
;; Output: 7
;; Explanation: There are 11 strong pairs in the array nums: (1, 1), (1, 2), (2, 2), (2, 3), (2, 4), (3, 3), (3, 4), (3, 5), (4, 4), (4, 5) and (5, 5).
;; The maximum XOR possible from these pairs is 3 XOR 4 = 7.
;; Example 2:
;; Input: nums = [10,100]
;; Output: 0
;; Explanation: There are 2 strong pairs in the array nums: (10, 10) and (100, 100).
;; The maximum XOR possible from these pairs is 10 XOR 10 = 0 since the pair (100, 100) also gives 100 XOR 100 = 0.
;; Example 3:
;; Input: nums = [5,6,25,30]
;; Output: 7
;; Explanation: There are 6 strong pairs in the array nums: (5, 5), (5, 6), (6, 6), (25, 25), (25, 30) and (30, 30).
;; The maximum XOR possible from these pairs is 25 XOR 30 = 7 since the only other non-zero XOR value is 5 XOR 6 = 3.
;; Constraints:
;; 1 <= nums.length <= 50
;; 1 <= nums[i] <= 100
(define (maximumStrongPairXor nums)
  ;; This function finds the maximum XOR value out of all possible strong pairs in the list nums.
  ;; A strong pair (x, y) satisfies the condition: |x - y| <= min(x, y)
  (define (strong-pair? x y)
    ;; Helper function to check if the pair (x, y) is a strong pair.
    (<= (abs (- x y)) (min x y)))
  
  (define (pair-xor x y)
    ;; Helper function to compute the XOR of a pair (x, y).
    (bitwise-xor x y))

  ;; Generate all possible pairs from the list, including pairs of the same element.
  (define pairs
    (for*/list ([x (in-list nums)]
                [y (in-list nums)]
                #:when (strong-pair? x y))
      (pair-xor x y)))

  ;; Find the maximum XOR value from the list of all valid pairs' XOR results.
  (if (null? pairs)
      0
      (apply max pairs)))

;; Examples
(maximumStrongPairXor '(1 2 3 4 5))  ; Output: 7
(maximumStrongPairXor '(10 100))     ; Output: 0
(maximumStrongPairXor '(5 6 25 30))  ; Output: 7

(require rackunit)


(define (test-humaneval) 

  (let (( candidate maximumStrongPairXor))
    (check-within (candidate (list 1 2 3 4 5)) 7 0.001)
    (check-within (candidate (list 10 100)) 0 0.001)
    (check-within (candidate (list 5 6 25 30)) 7 0.001)
    (check-within (candidate (list 1)) 0 0.001)
    (check-within (candidate (list 100)) 0 0.001)
    (check-within (candidate (list 1 1 2 3 5)) 6 0.001)
    (check-within (candidate (list 1 1 3 8 7)) 15 0.001)
    (check-within (candidate (list 1 1 4 4 3)) 7 0.001)
    (check-within (candidate (list 1 1 6 6 9)) 15 0.001)
    (check-within (candidate (list 1 1 10 3 9)) 3 0.001)
    (check-within (candidate (list 1 2 1 5 3)) 6 0.001)
    (check-within (candidate (list 1 2 2 1 2)) 3 0.001)
    (check-within (candidate (list 1 2 3 8 1)) 3 0.001)
    (check-within (candidate (list 1 2 5 5 10)) 15 0.001)
    (check-within (candidate (list 1 2 8 3 2)) 3 0.001)
    (check-within (candidate (list 1 2 9 2 8)) 3 0.001)
    (check-within (candidate (list 1 3 3 2 1)) 3 0.001)
    (check-within (candidate (list 1 3 8 5 3)) 13 0.001)
    (check-within (candidate (list 1 3 9 6 5)) 15 0.001)
    (check-within (candidate (list 1 4 1 2 5)) 6 0.001)
    (check-within (candidate (list 1 4 3 9 7)) 14 0.001)
    (check-within (candidate (list 1 4 4 3 4)) 7 0.001)
    (check-within (candidate (list 1 4 5 2 1)) 6 0.001)
    (check-within (candidate (list 1 4 8 6 6)) 14 0.001)
    (check-within (candidate (list 1 5 1 9 1)) 12 0.001)
    (check-within (candidate (list 1 5 4 1 7)) 3 0.001)
    (check-within (candidate (list 1 5 5 2 7)) 3 0.001)
    (check-within (candidate (list 1 5 9 1 2)) 12 0.001)
    (check-within (candidate (list 1 5 9 10 4)) 15 0.001)
    (check-within (candidate (list 1 6 2 7 9)) 15 0.001)
    (check-within (candidate (list 1 6 3 3 10)) 12 0.001)
    (check-within (candidate (list 1 6 4 5 3)) 7 0.001)
    (check-within (candidate (list 1 6 4 6 1)) 2 0.001)
    (check-within (candidate (list 1 6 5 5 8)) 14 0.001)
    (check-within (candidate (list 1 6 6 1 9)) 15 0.001)
    (check-within (candidate (list 1 6 8 5 3)) 14 0.001)
    (check-within (candidate (list 1 6 8 10 3)) 14 0.001)
    (check-within (candidate (list 1 6 9 8 2)) 15 0.001)
    (check-within (candidate (list 1 6 10 1 2)) 12 0.001)
    (check-within (candidate (list 1 7 1 4 3)) 7 0.001)
    (check-within (candidate (list 1 7 2 1 10)) 13 0.001)
    (check-within (candidate (list 1 7 2 10 10)) 13 0.001)
    (check-within (candidate (list 1 7 6 1 4)) 3 0.001)
    (check-within (candidate (list 1 7 8 6 8)) 15 0.001)
    (check-within (candidate (list 1 8 1 1 8)) 0 0.001)
    (check-within (candidate (list 1 8 4 2 1)) 12 0.001)
    (check-within (candidate (list 1 8 4 5 6)) 14 0.001)
    (check-within (candidate (list 1 8 5 10 4)) 15 0.001)
    (check-within (candidate (list 1 8 8 8 3)) 0 0.001)
    (check-within (candidate (list 1 8 10 2 6)) 14 0.001)
    (check-within (candidate (list 1 9 4 4 2)) 6 0.001)
    (check-within (candidate (list 1 9 6 5 7)) 15 0.001)
    (check-within (candidate (list 1 9 6 8 1)) 15 0.001)
    (check-within (candidate (list 1 9 8 2 8)) 3 0.001)
    (check-within (candidate (list 1 9 8 9 6)) 15 0.001)
    (check-within (candidate (list 1 9 9 7 6)) 15 0.001)
    (check-within (candidate (list 1 10 1 1 1)) 0 0.001)
    (check-within (candidate (list 1 10 5 10 6)) 15 0.001)
    (check-within (candidate (list 1 10 8 7 2)) 15 0.001)
    (check-within (candidate (list 1 10 9 9 2)) 3 0.001)
    (check-within (candidate (list 2 1 1 5 5)) 3 0.001)
    (check-within (candidate (list 2 1 1 7 4)) 6 0.001)
    (check-within (candidate (list 2 1 8 3 2)) 3 0.001)
    (check-within (candidate (list 2 1 9 2 1)) 3 0.001)
    (check-within (candidate (list 2 2 4 1 4)) 6 0.001)
    (check-within (candidate (list 2 2 5 5 1)) 3 0.001)
    (check-within (candidate (list 2 2 5 10 6)) 15 0.001)
    (check-within (candidate (list 2 2 8 2 10)) 2 0.001)
    (check-within (candidate (list 2 2 10 5 9)) 15 0.001)
    (check-within (candidate (list 2 3 3 5 3)) 6 0.001)
    (check-within (candidate (list 2 3 8 8 10)) 2 0.001)
    (check-within (candidate (list 2 4 5 3 2)) 7 0.001)
    (check-within (candidate (list 2 4 6 8 8)) 14 0.001)
    (check-within (candidate (list 2 4 6 9 8)) 15 0.001)
    (check-within (candidate (list 2 4 8 7 5)) 15 0.001)
    (check-within (candidate (list 2 4 10 4 2)) 6 0.001)
    (check-within (candidate (list 2 4 10 6 2)) 12 0.001)
    (check-within (candidate (list 2 4 10 7 8)) 15 0.001)
    (check-within (candidate (list 2 4 10 9 3)) 7 0.001)
    (check-within (candidate (list 2 5 2 5 5)) 0 0.001)
    (check-within (candidate (list 2 5 3 10 10)) 15 0.001)
    (check-within (candidate (list 2 5 7 2 7)) 2 0.001)
    (check-within (candidate (list 2 5 9 8 9)) 13 0.001)
    (check-within (candidate (list 2 5 10 8 4)) 15 0.001)
    (check-within (candidate (list 2 6 1 6 4)) 6 0.001)
    (check-within (candidate (list 2 6 1 8 7)) 15 0.001)
    (check-within (candidate (list 2 6 7 4 2)) 6 0.001)
    (check-within (candidate (list 2 6 8 6 10)) 14 0.001)
    (check-within (candidate (list 2 6 10 5 4)) 15 0.001)
    (check-within (candidate (list 2 7 1 8 9)) 15 0.001)
    (check-within (candidate (list 2 7 2 8 10)) 15 0.001)
    (check-within (candidate (list 2 7 3 3 7)) 1 0.001)
    (check-within (candidate (list 2 7 4 8 1)) 15 0.001)
    (check-within (candidate (list 2 7 9 2 6)) 15 0.001)
    (check-within (candidate (list 2 7 10 7 1)) 13 0.001)
    (check-within (candidate (list 2 8 1 9 5)) 13 0.001)
    (check-within (candidate (list 2 8 7 10 3)) 15 0.001)
    (check-within (candidate (list 2 9 1 7 7)) 14 0.001)
    (check-within (candidate (list 2 9 2 8 2)) 1 0.001)
    (check-within (candidate (list 2 9 5 10 10)) 15 0.001)
))

(test-humaneval)