#lang racket

;; You are given a 0-indexed integer array nums, and an integer k.
;; The K-or of nums is a non-negative integer that satisfies the following:
;;  * The ith bit is set in the K-or if and only if there are at least k elements of nums in which bit i is set.
;; Return the K-or of nums.
;; Note that a bit i is set in x if (2i AND x) == 2i, where AND is the bitwise AND operator.
;; Example 1:
;; Input: nums = [7,12,9,8,9,15], k = 4
;; Output: 9
;; Explanation: Bit 0 is set at nums[0], nums[2], nums[4], and nums[5].
;; Bit 1 is set at nums[0], and nums[5].
;; Bit 2 is set at nums[0], nums[1], and nums[5].
;; Bit 3 is set at nums[1], nums[2], nums[3], nums[4], and nums[5].
;; Only bits 0 and 3 are set in at least k elements of the array, and bits i >= 4 are not set in any of the array's elements. Hence, the answer is 2^0 + 2^3 = 9.
;; Example 2:
;; Input: nums = [2,12,1,11,4,5], k = 6
;; Output: 0
;; Explanation: Since k == 6 == nums.length, the 6-or of the array is equal to the bitwise AND of all its elements. Hence, the answer is 2 AND 12 AND 1 AND 11 AND 4 AND 5 = 0.
;; Example 3:
;; Input: nums = [10,8,5,9,11,6,8], k = 1
;; Output: 15
;; Explanation: Since k == 1, the 1-or of the array is equal to the bitwise OR of all its elements. Hence, the answer is 10 OR 8 OR 5 OR 9 OR 11 OR 6 OR 8 = 15.
;; Constraints:
;;  * 1 <= nums.length <= 50
;;  * 0 <= nums[i] < 231
;;  * 1 <= k <= nums.length
(define (findKOr nums k)
  ;; Define the maximum number of bits we are going to check.
  (define max-bits 31)
  
  ;; Function to check if a specific bit is set in a number.
  (define (bit-set? num bit)
    (not (zero? (bitwise-and num (arithmetic-shift 1 bit)))))
  
  ;; Count how many numbers have the bit set at each position.
  (define (count-bit-set bit)
    (length (filter (lambda (num) (bit-set? num bit)) nums)))
  
  ;; Calculate the K-or by iterating over each bit position and combining the results.
  (define (calculate-k-or bits)
    (for/fold ([result 0]) ([bit (in-list bits)])
      (if (>= (count-bit-set bit) k)
          (bitwise-ior result (arithmetic-shift 1 bit))
          result)))

  ;; Generate a list of bit positions from 0 to max-bits-1 and compute the K-or.
  (calculate-k-or (range 0 max-bits)))

;; Example usage:
(findKOr '(7 12 9 8 9 15) 4)   ; Output: 9
(findKOr '(2 12 1 11 4 5) 6)   ; Output: 0
(findKOr '(10 8 5 9 11 6 8) 1) ; Output: 15

(require rackunit)


(define (test-humaneval) 

  (let (( candidate findKOr))
    (check-within (candidate (list 7 12 9 8 9 15) 4) 9 0.001)
    (check-within (candidate (list 2 12 1 11 4 5) 6) 0 0.001)
    (check-within (candidate (list 10 8 5 9 11 6 8) 1) 15 0.001)
    (check-within (candidate (list 14 7 12 9 8 9 1 15) 4) 13 0.001)
    (check-within (candidate (list 2 12 1 11 4 5) 3) 5 0.001)
    (check-within (candidate (list 10 8 5 10 11 11 6 8) 1) 15 0.001)
    (check-within (candidate (list 0) 1) 0 0.001)
    (check-within (candidate (list 1) 1) 1 0.001)
    (check-within (candidate (list 2) 1) 2 0.001)
    (check-within (candidate (list 3) 1) 3 0.001)
    (check-within (candidate (list 4) 1) 4 0.001)
    (check-within (candidate (list 5) 1) 5 0.001)
    (check-within (candidate (list 6) 1) 6 0.001)
    (check-within (candidate (list 7) 1) 7 0.001)
    (check-within (candidate (list 8) 1) 8 0.001)
    (check-within (candidate (list 9) 1) 9 0.001)
    (check-within (candidate (list 10) 1) 10 0.001)
    (check-within (candidate (list 11) 1) 11 0.001)
    (check-within (candidate (list 12) 1) 12 0.001)
    (check-within (candidate (list 13) 1) 13 0.001)
    (check-within (candidate (list 14) 1) 14 0.001)
    (check-within (candidate (list 15) 1) 15 0.001)
    (check-within (candidate (list 16) 1) 16 0.001)
    (check-within (candidate (list 17) 1) 17 0.001)
    (check-within (candidate (list 18) 1) 18 0.001)
    (check-within (candidate (list 19) 1) 19 0.001)
    (check-within (candidate (list 20) 1) 20 0.001)
    (check-within (candidate (list 21) 1) 21 0.001)
    (check-within (candidate (list 22) 1) 22 0.001)
    (check-within (candidate (list 23) 1) 23 0.001)
    (check-within (candidate (list 24) 1) 24 0.001)
    (check-within (candidate (list 25) 1) 25 0.001)
    (check-within (candidate (list 26) 1) 26 0.001)
    (check-within (candidate (list 27) 1) 27 0.001)
    (check-within (candidate (list 28) 1) 28 0.001)
    (check-within (candidate (list 29) 1) 29 0.001)
    (check-within (candidate (list 30) 1) 30 0.001)
    (check-within (candidate (list 31) 1) 31 0.001)
    (check-within (candidate (list 22 7 27 30 15 30 28) 4) 30 0.001)
    (check-within (candidate (list 24 18 3 23 16 11 27 18 5 29) 6) 19 0.001)
    (check-within (candidate (list 14 1 2 28 4 15 3 12) 2) 15 0.001)
    (check-within (candidate (list 7 18 25 11 2) 5) 0 0.001)
    (check-within (candidate (list 0 4) 2) 0 0.001)
    (check-within (candidate (list 17 5 14 16 24 30 3 19 31) 1) 31 0.001)
    (check-within (candidate (list 14 20 23 7 1 12 24 19) 7) 0 0.001)
    (check-within (candidate (list 5 31 29 22 8 6 23) 4) 23 0.001)
    (check-within (candidate (list 9 10 30 0 7 19 14 19 20 3) 4) 31 0.001)
    (check-within (candidate (list 25 6 5 30 27 11 10 30) 2) 31 0.001)
    (check-within (candidate (list 0 15 16 6 19 5 24 17) 3) 23 0.001)
    (check-within (candidate (list 19 8 2 28 4 5) 5) 0 0.001)
    (check-within (candidate (list 13 9 1 15 9 2 19 19) 3) 11 0.001)
    (check-within (candidate (list 16 6 16 22 8 2 25 30) 1) 31 0.001)
    (check-within (candidate (list 14 28 23 22) 2) 30 0.001)
    (check-within (candidate (list 6 26) 2) 2 0.001)
    (check-within (candidate (list 14 9 22 30 15) 4) 14 0.001)
    (check-within (candidate (list 12 13 16 25 12 4 8 29) 6) 8 0.001)
    (check-within (candidate (list 27 29) 2) 25 0.001)
    (check-within (candidate (list 9 27 27 20 24 13 25 8) 6) 8 0.001)
    (check-within (candidate (list 22 26 18 26 1) 1) 31 0.001)
    (check-within (candidate (list 20 20 31 19 29 19) 2) 31 0.001)
    (check-within (candidate (list 5 8 27 23 3) 2) 31 0.001)
    (check-within (candidate (list 4 23 0 20 4 19 14 22 26 2) 9) 0 0.001)
    (check-within (candidate (list 31 26 21 4 9 11 13 24 23 5) 10) 0 0.001)
    (check-within (candidate (list 4 22) 2) 4 0.001)
    (check-within (candidate (list 22 17 20 3 21 5 20 25 16) 4) 21 0.001)
    (check-within (candidate (list 16 15 13 26 15 23 0 12) 4) 15 0.001)
    (check-within (candidate (list 4 11 14) 1) 15 0.001)
    (check-within (candidate (list 10 26 27 25 3 21 9 3 22) 8) 0 0.001)
    (check-within (candidate (list 4 11 16) 2) 0 0.001)
    (check-within (candidate (list 9 27 19 9 24 11) 3) 27 0.001)
    (check-within (candidate (list 29 19 27 14) 1) 31 0.001)
    (check-within (candidate (list 27 31 21 8 25) 2) 31 0.001)
    (check-within (candidate (list 14 1 13 22 27) 5) 0 0.001)
    (check-within (candidate (list 14 15 17 23 29) 1) 31 0.001)
    (check-within (candidate (list 19 21) 2) 17 0.001)
    (check-within (candidate (list 29 9 18 0 30 5 1 9) 1) 31 0.001)
    (check-within (candidate (list 6 31 11 7 6 2 26 19 17 13) 10) 0 0.001)
    (check-within (candidate (list 15 8 27 28) 1) 31 0.001)
    (check-within (candidate (list 28 24 20 31 23 1) 2) 31 0.001)
    (check-within (candidate (list 8 13 27 24 20 28 15 21 23 6) 9) 0 0.001)
    (check-within (candidate (list 31 6) 2) 6 0.001)
    (check-within (candidate (list 3 14 11 17 9) 1) 31 0.001)
    (check-within (candidate (list 14 11) 2) 10 0.001)
    (check-within (candidate (list 2 9 11 25 3 2 26 21 13 11) 1) 31 0.001)
    (check-within (candidate (list 26 7 23 0 16 31 12 18 24) 4) 30 0.001)
    (check-within (candidate (list 11 30 30 17 10 27 6 31 0) 4) 31 0.001)
    (check-within (candidate (list 11 9 18 30 27 20 2 17 18 4) 2) 31 0.001)
    (check-within (candidate (list 2 1 0 30 29 14 13 26 10 22) 10) 0 0.001)
    (check-within (candidate (list 21 30 30 17 23 8 26 9) 6) 16 0.001)
    (check-within (candidate (list 16 10) 1) 26 0.001)
    (check-within (candidate (list 26 12 19 22 5 6 19 30 24 11) 10) 0 0.001)
    (check-within (candidate (list 20 10 14) 2) 14 0.001)
    (check-within (candidate (list 23 17 18 30 3) 1) 31 0.001)
    (check-within (candidate (list 13 16 12) 1) 29 0.001)
    (check-within (candidate (list 17 12 5 13 23 19) 3) 21 0.001)
    (check-within (candidate (list 5 4 3) 2) 5 0.001)
    (check-within (candidate (list 6 28 1 3 2) 1) 31 0.001)
    (check-within (candidate (list 28 3 15 30 10 29) 4) 14 0.001)
    (check-within (candidate (list 0 31 13 24 16 21) 3) 29 0.001)
    (check-within (candidate (list 11 20 28 29 3 4) 2) 31 0.001)
))

(test-humaneval)