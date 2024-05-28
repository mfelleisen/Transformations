#lang racket

;; Define the function minimumPossibleSum that calculates the minimum possible sum of a beautiful array
;; given the constraints on length and pairwise sum not equaling the target.
;; You are given positive integers n and target.
;; An array nums is beautiful if it meets the following conditions:
;;  * nums.length == n.
;;  * nums consists of pairwise distinct positive integers.
;;  * There doesn't exist two distinct indices, i and j, in the range [0, n - 1], such that nums[i] + nums[j] == target.
;; Return the minimum possible sum that a beautiful array could have modulo 109 + 7.
;; Example 1:
;; Input: n = 2, target = 3
;; Output: 4
;; Explanation: We can see that nums = [1,3] is beautiful.
;; - The array nums has length n = 2.
;; - The array nums consists of pairwise distinct positive integers.
;; - There doesn't exist two distinct indices, i and j, with nums[i] + nums[j] == 3.
;; It can be proven that 4 is the minimum possible sum that a beautiful array could have.
;; Example 2:
;; Input: n = 3, target = 3
;; Output: 8
;; Explanation: We can see that nums = [1,3,4] is beautiful.
;; - The array nums has length n = 3.
;; - The array nums consists of pairwise distinct positive integers.
;; - There doesn't exist two distinct indices, i and j, with nums[i] + nums[j] == 3.
;; It can be proven that 8 is the minimum possible sum that a beautiful array could have.
;; Example 3:
;; Input: n = 1, target = 1
;; Output: 1
;; Explanation: We can see, that nums = [1] is beautiful.
;; Constraints:
;;  * 1 <= n <= 109
;;  * 1 <= target <= 109
(define (minimumPossibleSum n target)
  (define MOD (+ (expt 10 9) 7))

  ;; Helper function to recursively build the beautiful array and calculate the sum.
  (define (build-beautiful-array n target candidate acc sum)
    (if (= (length acc) n)
        (modulo sum MOD)
        (if (ormap (lambda (num) (= (+ num candidate) target)) acc)
            (build-beautiful-array n target (+ candidate 1) acc sum)
            (build-beautiful-array n target (+ candidate 1) (cons candidate acc) (+ sum candidate)))))

  ;; Start building the beautiful array from candidate 1 with an empty accumulator and sum 0.
  (build-beautiful-array n target 1 '() 0))

;; Example test cases
(minimumPossibleSum 2 3)   ; Expected: 4 (nums = [1, 3])
(minimumPossibleSum 3 3)   ; Expected: 8 (nums = [1, 3, 4])
(minimumPossibleSum 1 1)   ; Expected: 1 (nums = [1])

(require rackunit)


(define (test-humaneval) 

  (let (( candidate minimumPossibleSum))
    (check-within (candidate 2 3) 4 0.001)
    (check-within (candidate 3 3) 8 0.001)
    (check-within (candidate 1 1) 1 0.001)
    (check-within (candidate 16 6) 162 0.001)
    (check-within (candidate 16 32) 136 0.001)
    (check-within (candidate 13 50) 91 0.001)
    (check-within (candidate 36 21) 926 0.001)
    (check-within (candidate 40 17) 1076 0.001)
    (check-within (candidate 37 46) 1011 0.001)
    (check-within (candidate 33 7) 651 0.001)
    (check-within (candidate 42 46) 1321 0.001)
    (check-within (candidate 46 29) 1529 0.001)
    (check-within (candidate 9 43) 45 0.001)
    (check-within (candidate 30 31) 690 0.001)
    (check-within (candidate 14 47) 105 0.001)
    (check-within (candidate 5 3) 19 0.001)
    (check-within (candidate 41 23) 1191 0.001)
    (check-within (candidate 17 13) 219 0.001)
    (check-within (candidate 9 13) 63 0.001)
    (check-within (candidate 29 18) 595 0.001)
    (check-within (candidate 21 14) 315 0.001)
    (check-within (candidate 4 6) 12 0.001)
    (check-within (candidate 38 15) 958 0.001)
    (check-within (candidate 14 7) 138 0.001)
    (check-within (candidate 18 26) 231 0.001)
    (check-within (candidate 11 15) 94 0.001)
    (check-within (candidate 7 8) 37 0.001)
    (check-within (candidate 11 22) 66 0.001)
    (check-within (candidate 36 11) 821 0.001)
    (check-within (candidate 18 29) 227 0.001)
    (check-within (candidate 26 17) 495 0.001)
    (check-within (candidate 37 13) 889 0.001)
    (check-within (candidate 46 38) 1567 0.001)
    (check-within (candidate 13 7) 121 0.001)
    (check-within (candidate 33 34) 817 0.001)
    (check-within (candidate 39 12) 945 0.001)
    (check-within (candidate 1 45) 1 0.001)
    (check-within (candidate 37 36) 1026 0.001)
    (check-within (candidate 16 19) 199 0.001)
    (check-within (candidate 22 15) 358 0.001)
    (check-within (candidate 34 42) 855 0.001)
    (check-within (candidate 50 22) 1665 0.001)
    (check-within (candidate 42 44) 1323 0.001)
    (check-within (candidate 40 8) 928 0.001)
    (check-within (candidate 7 19) 28 0.001)
    (check-within (candidate 44 10) 1146 0.001)
    (check-within (candidate 21 6) 267 0.001)
    (check-within (candidate 27 26) 546 0.001)
    (check-within (candidate 49 4) 1272 0.001)
    (check-within (candidate 35 2) 630 0.001)
    (check-within (candidate 32 29) 780 0.001)
    (check-within (candidate 20 41) 210 0.001)
    (check-within (candidate 30 48) 603 0.001)
    (check-within (candidate 12 34) 78 0.001)
    (check-within (candidate 50 44) 1863 0.001)
    (check-within (candidate 42 26) 1251 0.001)
    (check-within (candidate 3 18) 6 0.001)
    (check-within (candidate 11 3) 76 0.001)
    (check-within (candidate 38 29) 1077 0.001)
    (check-within (candidate 17 24) 208 0.001)
    (check-within (candidate 50 31) 1800 0.001)
    (check-within (candidate 32 41) 768 0.001)
    (check-within (candidate 12 24) 78 0.001)
    (check-within (candidate 35 43) 924 0.001)
    (check-within (candidate 9 47) 45 0.001)
    (check-within (candidate 32 26) 756 0.001)
    (check-within (candidate 6 42) 21 0.001)
    (check-within (candidate 11 1) 66 0.001)
    (check-within (candidate 11 24) 66 0.001)
    (check-within (candidate 39 38) 1140 0.001)
    (check-within (candidate 18 8) 213 0.001)
    (check-within (candidate 29 5) 489 0.001)
    (check-within (candidate 44 6) 1072 0.001)
    (check-within (candidate 29 30) 631 0.001)
    (check-within (candidate 13 47) 91 0.001)
    (check-within (candidate 46 21) 1441 0.001)
    (check-within (candidate 46 6) 1167 0.001)
    (check-within (candidate 27 30) 546 0.001)
    (check-within (candidate 5 35) 15 0.001)
    (check-within (candidate 12 32) 78 0.001)
    (check-within (candidate 13 39) 91 0.001)
    (check-within (candidate 10 11) 80 0.001)
    (check-within (candidate 46 14) 1315 0.001)
    (check-within (candidate 37 18) 927 0.001)
    (check-within (candidate 32 8) 612 0.001)
    (check-within (candidate 26 14) 465 0.001)
    (check-within (candidate 33 41) 821 0.001)
    (check-within (candidate 44 39) 1465 0.001)
    (check-within (candidate 3 21) 6 0.001)
    (check-within (candidate 9 11) 65 0.001)
    (check-within (candidate 16 43) 136 0.001)
    (check-within (candidate 7 22) 28 0.001)
    (check-within (candidate 21 49) 231 0.001)
    (check-within (candidate 23 16) 381 0.001)
    (check-within (candidate 33 32) 816 0.001)
    (check-within (candidate 22 1) 253 0.001)
    (check-within (candidate 5 47) 15 0.001)
    (check-within (candidate 38 7) 846 0.001)
    (check-within (candidate 38 24) 1027 0.001)
    (check-within (candidate 13 36) 91 0.001)
))

(test-humaneval)