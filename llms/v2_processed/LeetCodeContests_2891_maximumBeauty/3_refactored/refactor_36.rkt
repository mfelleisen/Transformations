#lang racket

;; You are given a 0-indexed array nums and a non-negative integer k.
;; In one operation, you can do the following:
;;  * Choose an index i that hasn't been chosen before from the range [0, nums.length - 1].
;;  * Replace nums[i] with any integer from the range [nums[i] - k, nums[i] + k].
;; The beauty of the array is the length of the longest subsequence consisting of equal elements.
;; Return the maximum possible beauty of the array nums after applying the operation any number of times.
;; Note that you can apply the operation to each index only once.
;; A subsequence of an array is a new array generated from the original array by deleting some elements (possibly none) without changing the order of the remaining elements.
;; Example 1:
;; Input: nums = [4,6,1,2], k = 2
;; Output: 3
;; Explanation: In this example, we apply the following operations:
;; - Choose index 1, replace it with 4 (from range [4,8]), nums = [4,4,1,2].
;; - Choose index 3, replace it with 4 (from range [0,4]), nums = [4,4,1,4].
;; After the applied operations, the beauty of the array nums is 3 (subsequence consisting of indices 0, 1, and 3).
;; It can be proven that 3 is the maximum possible length we can achieve.
;; Example 2:
;; Input: nums = [1,1,1,1], k = 10
;; Output: 4
;; Explanation: In this example we don't have to apply any operations.
;; The beauty of the array nums is 4 (whole array).
;; Constraints:
;;  * 1 <= nums.length <= 105
;;  * 0 <= nums[i], k <= 105
(define (maximumBeauty nums k)
  ;; Use a hash table to count frequencies of each potential target value
  (define freq-map (make-hash))

  ;; Helper function to update the frequency map for a given number within the range [num-k, num+k]
  (define (update-freq num)
    (for ([possible-value (in-range (- num k) (+ num k 1))])
      (hash-update! freq-map possible-value add1 0)))

  ;; Populate the frequency map by iterating over each number in nums
  (for-each update-freq nums)

  ;; Return the maximum frequency found in the map
  (apply max (hash-values freq-map)))

;; Example usage
(maximumBeauty '(4 6 1 2) 2)  ; Output: 3
(maximumBeauty '(1 1 1 1) 10) ; Output: 4

(require rackunit)


(define (test-humaneval) 

  (let (( candidate maximumBeauty))
    (check-within (candidate (list 4 6 1 2) 2) 3 0.001)
    (check-within (candidate (list 1 1 1 1) 10) 4 0.001)
    (check-within (candidate (list 12 71) 10) 1 0.001)
    (check-within (candidate (list 27 55) 1) 1 0.001)
    (check-within (candidate (list 52 34) 21) 2 0.001)
    (check-within (candidate (list 76 0) 16) 1 0.001)
    (check-within (candidate (list 56 40) 26) 2 0.001)
    (check-within (candidate (list 49 26) 12) 2 0.001)
    (check-within (candidate (list 69 66) 14) 2 0.001)
    (check-within (candidate (list 64 98) 12) 1 0.001)
    (check-within (candidate (list 83 81) 7) 2 0.001)
    (check-within (candidate (list 44 93) 15) 1 0.001)
    (check-within (candidate (list 44 31) 26) 2 0.001)
    (check-within (candidate (list 70 60) 15) 2 0.001)
    (check-within (candidate (list 60 22) 11) 1 0.001)
    (check-within (candidate (list 33 20) 1) 1 0.001)
    (check-within (candidate (list 64 24) 4) 1 0.001)
    (check-within (candidate (list 59 20) 28) 2 0.001)
    (check-within (candidate (list 10 98) 27) 1 0.001)
    (check-within (candidate (list 54 21) 20) 2 0.001)
    (check-within (candidate (list 61 11) 15) 1 0.001)
    (check-within (candidate (list 99 40) 27) 1 0.001)
    (check-within (candidate (list 32 91) 3) 1 0.001)
    (check-within (candidate (list 91 57) 21) 2 0.001)
    (check-within (candidate (list 60 92) 26) 2 0.001)
    (check-within (candidate (list 4 45) 6) 1 0.001)
    (check-within (candidate (list 24 35) 6) 2 0.001)
    (check-within (candidate (list 11 29) 3) 1 0.001)
    (check-within (candidate (list 51 29) 3) 1 0.001)
    (check-within (candidate (list 43 21) 14) 2 0.001)
    (check-within (candidate (list 32 25) 18) 2 0.001)
    (check-within (candidate (list 13 66) 5) 1 0.001)
    (check-within (candidate (list 89 71) 28) 2 0.001)
    (check-within (candidate (list 36 29) 20) 2 0.001)
    (check-within (candidate (list 11 43) 21) 2 0.001)
    (check-within (candidate (list 15 36) 4) 1 0.001)
    (check-within (candidate (list 11 51) 1) 1 0.001)
    (check-within (candidate (list 2 57) 20) 1 0.001)
    (check-within (candidate (list 94 66) 26) 2 0.001)
    (check-within (candidate (list 87 51) 8) 1 0.001)
    (check-within (candidate (list 5 57 46) 15) 2 0.001)
    (check-within (candidate (list 81 46 85) 23) 3 0.001)
    (check-within (candidate (list 51 83 0) 11) 1 0.001)
    (check-within (candidate (list 75 15 9) 28) 2 0.001)
    (check-within (candidate (list 10 59 86) 23) 2 0.001)
    (check-within (candidate (list 41 11 59) 17) 2 0.001)
    (check-within (candidate (list 62 77 100) 5) 1 0.001)
    (check-within (candidate (list 27 35 15) 6) 2 0.001)
    (check-within (candidate (list 81 76 40) 5) 2 0.001)
    (check-within (candidate (list 84 43 96) 7) 2 0.001)
    (check-within (candidate (list 62 1 93) 30) 2 0.001)
    (check-within (candidate (list 13 46 71) 29) 3 0.001)
    (check-within (candidate (list 92 99 44) 28) 3 0.001)
    (check-within (candidate (list 73 30 40) 26) 3 0.001)
    (check-within (candidate (list 83 89 17) 5) 2 0.001)
    (check-within (candidate (list 38 20 11) 9) 2 0.001)
    (check-within (candidate (list 63 56 23) 14) 2 0.001)
    (check-within (candidate (list 32 16 98) 0) 1 0.001)
    (check-within (candidate (list 57 58 71) 2) 2 0.001)
    (check-within (candidate (list 61 50 35) 2) 1 0.001)
    (check-within (candidate (list 22 97 13) 22) 2 0.001)
    (check-within (candidate (list 89 52 33) 14) 2 0.001)
    (check-within (candidate (list 89 4 77) 20) 2 0.001)
    (check-within (candidate (list 50 26 72) 30) 3 0.001)
    (check-within (candidate (list 72 75 47) 7) 2 0.001)
    (check-within (candidate (list 23 1 73) 25) 2 0.001)
    (check-within (candidate (list 36 74 20) 20) 2 0.001)
    (check-within (candidate (list 34 64 11) 18) 2 0.001)
    (check-within (candidate (list 29 94 45) 27) 2 0.001)
    (check-within (candidate (list 22 80 34) 28) 2 0.001)
    (check-within (candidate (list 52 63 75) 11) 2 0.001)
    (check-within (candidate (list 53 63 93 89) 23) 4 0.001)
    (check-within (candidate (list 47 76 100 51) 27) 4 0.001)
    (check-within (candidate (list 73 83 46 88) 13) 3 0.001)
    (check-within (candidate (list 50 28 30 51) 2) 2 0.001)
    (check-within (candidate (list 88 87 9 17) 10) 2 0.001)
    (check-within (candidate (list 27 56 27 40) 6) 2 0.001)
    (check-within (candidate (list 88 19 2 30) 6) 2 0.001)
    (check-within (candidate (list 58 50 0 97) 18) 2 0.001)
    (check-within (candidate (list 83 10 99 99) 18) 3 0.001)
    (check-within (candidate (list 58 75 1 25) 12) 2 0.001)
    (check-within (candidate (list 77 35 1 69) 15) 2 0.001)
    (check-within (candidate (list 23 33 62 20) 12) 3 0.001)
    (check-within (candidate (list 42 34 18 0) 5) 2 0.001)
    (check-within (candidate (list 10 58 37 46) 0) 1 0.001)
    (check-within (candidate (list 34 73 57 55) 27) 4 0.001)
    (check-within (candidate (list 53 100 74 5) 4) 1 0.001)
    (check-within (candidate (list 48 93 96 19) 24) 3 0.001)
    (check-within (candidate (list 91 12 29 31) 22) 3 0.001)
    (check-within (candidate (list 48 9 35 36) 12) 3 0.001)
    (check-within (candidate (list 24 64 40 30) 3) 2 0.001)
    (check-within (candidate (list 19 58 41 42) 14) 3 0.001)
    (check-within (candidate (list 72 44 29 76) 4) 2 0.001)
    (check-within (candidate (list 37 19 10 16) 16) 4 0.001)
    (check-within (candidate (list 54 84 73 31) 30) 4 0.001)
    (check-within (candidate (list 83 92 30 60) 19) 3 0.001)
    (check-within (candidate (list 14 51 99 64) 15) 2 0.001)
    (check-within (candidate (list 7 60 16 2) 17) 3 0.001)
    (check-within (candidate (list 7 89 54 54) 5) 2 0.001)
    (check-within (candidate (list 43 86 33 18) 23) 3 0.001)
))

(test-humaneval)