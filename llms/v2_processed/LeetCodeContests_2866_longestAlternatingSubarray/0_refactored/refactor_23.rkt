#lang racket

;; You are given a 0-indexed integer array nums and an integer threshold.
;; Find the length of the longest subarray of nums starting at index l and ending at index r (0 <= l <= r < nums.length) that satisfies the following conditions:
;;  * nums[l] % 2 == 0
;;  * For all indices i in the range [l, r - 1], nums[i] % 2 != nums[i + 1] % 2
;;  * For all indices i in the range [l, r], nums[i] <= threshold
;; Return an integer denoting the length of the longest such subarray.
;; Note: A subarray is a contiguous non-empty sequence of elements within an array.
;; Example 1:
;; Input: nums = [3,2,5,4], threshold = 5
;; Output: 3
;; Explanation: In this example, we can select the subarray that starts at l = 1 and ends at r = 3 => [2,5,4]. This subarray satisfies the conditions.
;; Hence, the answer is the length of the subarray, 3. We can show that 3 is the maximum possible achievable length.
;; Example 2:
;; Input: nums = [1,2], threshold = 2
;; Output: 1
;; Explanation: In this example, we can select the subarray that starts at l = 1 and ends at r = 1 => [2].
;; It satisfies all the conditions and we can show that 1 is the maximum possible achievable length.
;; Example 3:
;; Input: nums = [2,3,4,5], threshold = 4
;; Output: 3
;; Explanation: In this example, we can select the subarray that starts at l = 0 and ends at r = 2 => [2,3,4].
;; It satisfies all the conditions.
;; Hence, the answer is the length of the subarray, 3. We can show that 3 is the maximum possible achievable length.
;; Constraints:
;;  * 1 <= nums.length <= 100
;;  * 1 <= nums[i] <= 100
;;  * 1 <= threshold <= 100
(define (longestAlternatingSubarray nums threshold)
  (define n (length nums))

  (define (valid-start? index)
    (and (even? (list-ref nums index))
         (<= (list-ref nums index) threshold)))

  (define (max-subarray-length starting-index)
    (let loop ([current-index (+ starting-index 1)]
               [current-length 1]
               [last-modulo (modulo (list-ref nums starting-index) 2)])
      (if (and (< current-index n)
               (<= (list-ref nums current-index) threshold)
               (not (= (modulo (list-ref nums current-index) 2) last-modulo)))
          (loop (+ current-index 1)
                (+ current-length 1)
                (modulo (list-ref nums current-index) 2))
          current-length)))

  (for/fold ([max-length 0]) ([i (in-range n)])
    (if (valid-start? i)
        (max max-length (max-subarray-length i))
        max-length)))

;; Example usage:
(longestAlternatingSubarray '(3 2 5 4) 5)  ; Output: 3
(longestAlternatingSubarray '(1 2) 2)       ; Output: 1
(longestAlternatingSubarray '(2 3 4 5) 4)  ; Output: 3

(require rackunit)


(define (test-humaneval) 

  (let (( candidate longestAlternatingSubarray))
    (check-within (candidate (list 3 2 5 4) 5) 3 0.001)
    (check-within (candidate (list 1 2) 2) 1 0.001)
    (check-within (candidate (list 2 3 4 5) 4) 3 0.001)
    (check-within (candidate (list 1) 1) 0 0.001)
    (check-within (candidate (list 2) 2) 1 0.001)
    (check-within (candidate (list 3) 3) 0 0.001)
    (check-within (candidate (list 4) 1) 0 0.001)
    (check-within (candidate (list 5) 3) 0 0.001)
    (check-within (candidate (list 6) 5) 0 0.001)
    (check-within (candidate (list 7) 2) 0 0.001)
    (check-within (candidate (list 8) 1) 0 0.001)
    (check-within (candidate (list 9) 7) 0 0.001)
    (check-within (candidate (list 10) 7) 0 0.001)
    (check-within (candidate (list 1 3) 16) 0 0.001)
    (check-within (candidate (list 1 6) 2) 0 0.001)
    (check-within (candidate (list 1 10) 6) 0 0.001)
    (check-within (candidate (list 1 10) 7) 0 0.001)
    (check-within (candidate (list 2 2) 18) 1 0.001)
    (check-within (candidate (list 2 4) 7) 1 0.001)
    (check-within (candidate (list 2 5) 2) 1 0.001)
    (check-within (candidate (list 2 6) 15) 1 0.001)
    (check-within (candidate (list 2 7) 9) 2 0.001)
    (check-within (candidate (list 2 8) 4) 1 0.001)
    (check-within (candidate (list 2 8) 8) 1 0.001)
    (check-within (candidate (list 2 8) 16) 1 0.001)
    (check-within (candidate (list 2 9) 14) 2 0.001)
    (check-within (candidate (list 3 1) 9) 0 0.001)
    (check-within (candidate (list 3 4) 10) 1 0.001)
    (check-within (candidate (list 3 10) 3) 0 0.001)
    (check-within (candidate (list 4 2) 11) 1 0.001)
    (check-within (candidate (list 4 2) 15) 1 0.001)
    (check-within (candidate (list 4 4) 9) 1 0.001)
    (check-within (candidate (list 4 7) 8) 2 0.001)
    (check-within (candidate (list 4 9) 17) 2 0.001)
    (check-within (candidate (list 5 8) 5) 0 0.001)
    (check-within (candidate (list 5 8) 15) 1 0.001)
    (check-within (candidate (list 6 2) 10) 1 0.001)
    (check-within (candidate (list 6 4) 14) 1 0.001)
    (check-within (candidate (list 6 4) 16) 1 0.001)
    (check-within (candidate (list 6 5) 10) 2 0.001)
    (check-within (candidate (list 7 3) 8) 0 0.001)
    (check-within (candidate (list 7 4) 7) 1 0.001)
    (check-within (candidate (list 7 5) 1) 0 0.001)
    (check-within (candidate (list 7 10) 52) 1 0.001)
    (check-within (candidate (list 7 17) 31) 0 0.001)
    (check-within (candidate (list 8 4) 6) 1 0.001)
    (check-within (candidate (list 8 8) 20) 1 0.001)
    (check-within (candidate (list 9 2) 11) 1 0.001)
    (check-within (candidate (list 9 4) 15) 1 0.001)
    (check-within (candidate (list 10 3) 11) 2 0.001)
    (check-within (candidate (list 10 4) 7) 1 0.001)
    (check-within (candidate (list 10 5) 20) 2 0.001)
    (check-within (candidate (list 10 7) 11) 2 0.001)
    (check-within (candidate (list 10 8) 4) 0 0.001)
    (check-within (candidate (list 10 18) 43) 1 0.001)
    (check-within (candidate (list 12 34) 7) 0 0.001)
    (check-within (candidate (list 12 35) 8) 0 0.001)
    (check-within (candidate (list 13 9) 53) 0 0.001)
    (check-within (candidate (list 15 13) 23) 0 0.001)
    (check-within (candidate (list 15 15) 18) 0 0.001)
    (check-within (candidate (list 17 2) 17) 1 0.001)
    (check-within (candidate (list 23 37) 35) 0 0.001)
    (check-within (candidate (list 24 11) 54) 2 0.001)
    (check-within (candidate (list 27 9) 55) 0 0.001)
    (check-within (candidate (list 27 17) 40) 0 0.001)
    (check-within (candidate (list 33 4) 43) 1 0.001)
    (check-within (candidate (list 41 16) 9) 0 0.001)
    (check-within (candidate (list 47 44) 20) 0 0.001)
    (check-within (candidate (list 49 39) 52) 0 0.001)
    (check-within (candidate (list 50 8) 19) 1 0.001)
    (check-within (candidate (list 76 46) 91) 1 0.001)
    (check-within (candidate (list 1 1 7) 4) 0 0.001)
    (check-within (candidate (list 1 3 1) 18) 0 0.001)
    (check-within (candidate (list 1 4 3) 1) 0 0.001)
    (check-within (candidate (list 1 5 3) 8) 0 0.001)
    (check-within (candidate (list 1 10 5) 9) 0 0.001)
    (check-within (candidate (list 2 1 8) 6) 2 0.001)
    (check-within (candidate (list 2 10 5) 7) 1 0.001)
    (check-within (candidate (list 3 2 8) 18) 1 0.001)
    (check-within (candidate (list 3 3 10) 20) 1 0.001)
    (check-within (candidate (list 3 4 2) 19) 1 0.001)
    (check-within (candidate (list 3 6 10) 6) 1 0.001)
    (check-within (candidate (list 3 8 9) 19) 2 0.001)
    (check-within (candidate (list 4 3 1) 4) 2 0.001)
    (check-within (candidate (list 4 4 4) 8) 1 0.001)
    (check-within (candidate (list 4 5 10) 3) 0 0.001)
    (check-within (candidate (list 4 10 2) 4) 1 0.001)
    (check-within (candidate (list 4 10 3) 8) 1 0.001)
    (check-within (candidate (list 4 10 3) 10) 2 0.001)
    (check-within (candidate (list 4 40 8) 45) 1 0.001)
    (check-within (candidate (list 5 3 9) 7) 0 0.001)
    (check-within (candidate (list 5 5 6) 7) 1 0.001)
    (check-within (candidate (list 6 2 2) 16) 1 0.001)
    (check-within (candidate (list 6 2 4) 17) 1 0.001)
    (check-within (candidate (list 6 3 4) 6) 3 0.001)
    (check-within (candidate (list 6 5 2) 17) 3 0.001)
    (check-within (candidate (list 6 5 3) 17) 2 0.001)
    (check-within (candidate (list 6 7 2) 14) 3 0.001)
    (check-within (candidate (list 7 1 10) 9) 0 0.001)
    (check-within (candidate (list 7 5 6) 8) 1 0.001)
))

(test-humaneval)