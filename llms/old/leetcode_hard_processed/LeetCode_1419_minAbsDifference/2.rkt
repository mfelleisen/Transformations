#lang racket

;; You are given an integer list nums and an integer goal.
;; You want to choose a subsequence of nums such that the sum of its elements is the closest possible to goal. That is, if the sum of the subsequence's elements is sum, then you want to minimize the absolute difference abs(sum - goal).
;; Return the minimum possible value of abs(sum - goal).
;; Note that a subsequence of a list is a list formed by removing some elements (possibly all or none) of the original list.
;; Example 1:
;; Input: nums = [5,-7,3,5], goal = 6
;; Output: 0
;; Explanation: Choose the whole list as a subsequence, with a sum of 6.
;; This is equal to the goal, so the absolute difference is 0.
;; Example 2:
;; Input: nums = [7,-9,15,-2], goal = -5
;; Output: 1
;; Explanation: Choose the subsequence [7,-9,-2], with a sum of -4.
;; The absolute difference is abs(-4 - (-5)) = abs(1) = 1, which is the minimum.
;; Example 3:
;; Input: nums = [1,2,3], goal = -7
;; Output: 7
;; Constraints:
;; 1 <= nums.length <= 40
;; -107 <= nums[i] <= 107
;; -109 <= goal <= 109
(define (minAbsDifference nums goal)
  ;; Helper function to generate all possible sums from a list of numbers.
  (define (allSums lst)
    (foldl (lambda (item acc)
             (append acc (map (lambda (s) (+ s item)) acc)))
           '(0)
           lst))
  
  ;; Split the list into two halves for manageability.
  (define left (take nums (quotient (length nums) 2)))
  (define right (drop nums (quotient (length nums) 2)))
  
  ;; Generate all possible sums from each half.
  (define sums-left (allSums left))
  (define sums-right (allSums right))
  
  ;; Initialize the answer with the absolute difference between goal and 0.
  (define ans (abs goal))
  
  ;; Iterate through all sums from the right list to find the closest sum
  ;; from the left list to the goal - sum_right.
  (for* ([sum-right sums-right])
    (let* ([target (- goal sum-right)]
           [closest (argmin (lambda (sum-left) (abs (- sum-left target))) sums-left)])
      ;; Update the answer if a closer sum to the goal is found.
      (set! ans (min ans (abs (- (+ closest sum-right) goal))))))
  
  ans)

;; Utility function to find the argument with the minimum value based on a given function.
(define (argmin f lst)
  (first (foldl (lambda (item acc)
                  (if (< (f item) (f (first acc)))
                      (list item (f item))
                      acc))
                (list (first lst) (f (first lst)))
                (rest lst))))
(require rackunit)

(define (test-humaneval) 

  (let (( candidate minAbsDifference))
    (check-within (candidate (list 5) 5) 0 0.001)
    (check-within (candidate (list 5) 10) 5 0.001)
    (check-within (candidate (list 1 2 3) 2) 0 0.001)
    (check-within (candidate (list 1 2 3 4 5) 7) 0 0.001)
    (check-within (candidate (list 1 2 3 4 5 6 7 8 9 10) 33) 0 0.001)
    (check-within (candidate (list 1 -1 1 -1 1 -1 1 -1) 2) 0 0.001)
    (check-within (candidate (list -3 7 -15 -20 5) 5) 0 0.001)
    (check-within (candidate (list 1 2 3 4 5) 10) 0 0.001)
    (check-within (candidate (list 4 7 -1 2 -2 5 6 1 8 3) 10) 0 0.001)
    (check-within (candidate (list 1 -1 1 -1 1 -1 1 -1) 1) 0 0.001)
    (check-within (candidate (list 1 2 3 4 5 6 7 8 9 10) 11) 0 0.001)
    (check-within (candidate (list -12 -8 -8 4 -12 10 -12 -3 -14 -12 -8 -8) -3) 0 0.001)
    (check-within (candidate (list -3 7 -15 -20 5) 6) 1 0.001)
    (check-within (candidate (list 1 2 3 4 5 6 7 8 9 10) 5) 0 0.001)
    (check-within (candidate (list -2 4 -4 6 8 -9) 3) 0 0.001)
    (check-within (candidate (list -7 -13 5 13 2 8 13 -14 -2 -15 8 10 -14 -12 -16 -13 -15 10 -14) -35) 0 0.001)
    (check-within (candidate (list 1 2 3) 5) 0 0.001)
    (check-within (candidate (list 1 2 3) 3) 0 0.001)
    (check-within (candidate (list -20 -30 -100 60 5 40 10 80) 10) 0 0.001)
    (check-within (candidate (list 1 2 3) 4) 0 0.001)
    (check-within (candidate (list 1 2 3) 1) 0 0.001)
    (check-within (candidate (list 1 -1 1 -1 1 -1 1 -1) 0) 0 0.001)
    (check-within (candidate (list -7 15 -2 -10 17 3 11 12 -12 5 -2 -4 11 -4 12) 12) 0 0.001)
    (check-within (candidate (list 1 2 3 4 5 6 7 8 9 10) 55) 0 0.001)
    (check-within (candidate (list 1 2 3) 6) 0 0.001)
    (check-within (candidate (list 1 -1 1 -1 1 -1 1 -1) 3) 0 0.001)
    (check-within (candidate (list -3 7 -15 -20 5) 11) 1 0.001)
    (check-within (candidate (list -78 50 86 92 -42 98 -65 -46 90 -12 34) 70) 0 0.001)
    (check-within (candidate (list 1 -1 1 -1 1 -1 1 -1) -1) 0 0.001)
))

(test-humaneval)