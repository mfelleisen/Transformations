#lang racket

;; Function to calculate the minimum number of increment operations needed to make the array beautiful
;; You are given a 0-indexed integer array nums having length n, and an integer k.
;; You can perform the following increment operation any number of times (including zero):
;;  * Choose an index i in the range [0, n - 1], and increase nums[i] by 1.
;; An array is considered beautiful if, for any subarray with a size of 3 or more, its maximum element is greater than or equal to k.
;; Return an integer denoting the minimum number of increment operations needed to make nums beautiful.
;; A subarray is a contiguous non-empty sequence of elements within an array.
;; Example 1:
;; Input: nums = [2,3,0,0,2], k = 4
;; Output: 3
;; Explanation: We can perform the following increment operations to make nums beautiful:
;; Choose index i = 1 and increase nums[1] by 1 -> [2,4,0,0,2].
;; Choose index i = 4 and increase nums[4] by 1 -> [2,4,0,0,3].
;; Choose index i = 4 and increase nums[4] by 1 -> [2,4,0,0,4].
;; The subarrays with a size of 3 or more are: [2,4,0], [4,0,0], [0,0,4], [2,4,0,0], [4,0,0,4], [2,4,0,0,4].
;; In all the subarrays, the maximum element is equal to k = 4, so nums is now beautiful.
;; It can be shown that nums cannot be made beautiful with fewer than 3 increment operations.
;; Hence, the answer is 3.
;; Example 2:
;; Input: nums = [0,1,3,3], k = 5
;; Output: 2
;; Explanation: We can perform the following increment operations to make nums beautiful:
;; Choose index i = 2 and increase nums[2] by 1 -> [0,1,4,3].
;; Choose index i = 2 and increase nums[2] by 1 -> [0,1,5,3].
;; The subarrays with a size of 3 or more are: [0,1,5], [1,5,3], [0,1,5,3].
;; In all the subarrays, the maximum element is equal to k = 5, so nums is now beautiful.
;; It can be shown that nums cannot be made beautiful with fewer than 2 increment operations.
;; Hence, the answer is 2.
;; Example 3:
;; Input: nums = [1,1,2], k = 1
;; Output: 0
;; Explanation: The only subarray with a size of 3 or more in this example is [1,1,2].
;; The maximum element, 2, is already greater than k = 1, so we don't need any increment operation.
;; Hence, the answer is 0.
;; Constraints:
;;  * 3 <= n == nums.length <= 105
;;  * 0 <= nums[i] <= 109
;;  * 0 <= k <= 109
(define (minIncrementOperations nums k)
  ;; Helper function to process each window of size 3 and calculate the required increments
  (define (process-window nums increments)
    (for/fold ([increments increments]) ([i (in-range (- (length nums) 2))])
      (let* ([window (take (drop nums i) 3)]
             [max-in-window (apply max window)]
             [needed (if (< max-in-window k) (- k max-in-window) 0)])
        (+ increments needed))))
  
  ;; Start processing the list of nums with an initial increment count of 0
  (process-window nums 0))

;; Example usage:
(minIncrementOperations '(2 3 0 0 2) 4)  ;; Output: 3
(minIncrementOperations '(0 1 3 3) 5)    ;; Output: 2
(minIncrementOperations '(1 1 2) 1)      ;; Output: 0

(require rackunit)


(define (test-humaneval) 

  (let (( candidate minIncrementOperations))
    (check-within (candidate (list 2 3 0 0 2) 4) 3 0.001)
    (check-within (candidate (list 0 1 3 3) 5) 2 0.001)
    (check-within (candidate (list 1 1 2) 1) 0 0.001)
    (check-within (candidate (list 0 5 5) 8) 3 0.001)
    (check-within (candidate (list 0 18 28) 93) 65 0.001)
    (check-within (candidate (list 0 24 14) 7) 0 0.001)
    (check-within (candidate (list 2 3 4) 3) 0 0.001)
    (check-within (candidate (list 3 5 9) 6) 0 0.001)
    (check-within (candidate (list 4 3 0) 2) 0 0.001)
    (check-within (candidate (list 5 6 5) 9) 3 0.001)
    (check-within (candidate (list 6 9 6) 3) 0 0.001)
    (check-within (candidate (list 7 9 0) 6) 0 0.001)
    (check-within (candidate (list 7 47 16) 39) 0 0.001)
    (check-within (candidate (list 9 6 1) 6) 0 0.001)
    (check-within (candidate (list 41 44 37) 55) 11 0.001)
    (check-within (candidate (list 48 3 13) 1) 0 0.001)
    (check-within (candidate (list 1 2 6 9) 8) 2 0.001)
    (check-within (candidate (list 1 3 1 6) 6) 3 0.001)
    (check-within (candidate (list 2 35 41 20) 4) 0 0.001)
    (check-within (candidate (list 3 9 9 7) 6) 0 0.001)
    (check-within (candidate (list 7 7 2 7) 9) 2 0.001)
    (check-within (candidate (list 10 2 0 2) 6) 4 0.001)
    (check-within (candidate (list 20 2 22 30) 67) 45 0.001)
    (check-within (candidate (list 22 49 0 20) 52) 3 0.001)
    (check-within (candidate (list 25 2 1 41) 9) 0 0.001)
    (check-within (candidate (list 31 86 1 2) 354) 268 0.001)
    (check-within (candidate (list 43 31 14 4) 73) 42 0.001)
    (check-within (candidate (list 44 24 28 47) 16) 0 0.001)
    (check-within (candidate (list 1 9 5 2 0) 2) 0 0.001)
    (check-within (candidate (list 3 7 9 6 0) 7) 0 0.001)
    (check-within (candidate (list 5 9 0 10 3) 3) 0 0.001)
    (check-within (candidate (list 6 2 8 10 6) 9) 1 0.001)
    (check-within (candidate (list 6 14 17 4 7) 22) 5 0.001)
    (check-within (candidate (list 10 9 5 2 4) 1) 0 0.001)
    (check-within (candidate (list 20 38 29 34 6) 95) 66 0.001)
    (check-within (candidate (list 21 45 33 14 22) 58) 25 0.001)
    (check-within (candidate (list 32 14 31 43 29) 46) 15 0.001)
    (check-within (candidate (list 39 21 10 46 40) 81) 71 0.001)
    (check-within (candidate (list 42 7 32 19 4) 66) 34 0.001)
    (check-within (candidate (list 74 91 93 96 12) 964) 871 0.001)
    (check-within (candidate (list 84 17 58 61 72) 432) 374 0.001)
    (check-within (candidate (list 4 0 10 2 10 6) 8) 0 0.001)
    (check-within (candidate (list 4 0 22 41 29 28) 30) 8 0.001)
    (check-within (candidate (list 4 1 8 0 3 9) 2) 0 0.001)
    (check-within (candidate (list 4 7 6 9 2 6) 1) 0 0.001)
    (check-within (candidate (list 5 1 3 9 8 8) 4) 0 0.001)
    (check-within (candidate (list 6 5 8 6 0 9) 9) 1 0.001)
    (check-within (candidate (list 7 4 10 2 0 8) 7) 0 0.001)
    (check-within (candidate (list 8 10 7 1 9 6) 1) 0 0.001)
    (check-within (candidate (list 9 5 0 10 9 0) 8) 0 0.001)
    (check-within (candidate (list 13 34 0 13 9 19) 82) 117 0.001)
    (check-within (candidate (list 16 50 23 35 38 13) 34) 0 0.001)
    (check-within (candidate (list 20 1 40 48 32 24) 38) 0 0.001)
    (check-within (candidate (list 28 5 10 26 38 6) 17) 0 0.001)
    (check-within (candidate (list 30 42 46 45 23 31) 13) 0 0.001)
    (check-within (candidate (list 3 8 0 6 46 24 31) 30) 22 0.001)
    (check-within (candidate (list 4 7 2 10 4 10 5) 10) 3 0.001)
    (check-within (candidate (list 8 10 1 5 8 9 7) 4) 0 0.001)
    (check-within (candidate (list 10 7 2 5 9 6 3) 2) 0 0.001)
    (check-within (candidate (list 10 24 21 42 6 10 43) 55) 56 0.001)
    (check-within (candidate (list 18 48 1 19 43 25 49) 21) 0 0.001)
    (check-within (candidate (list 37 82 95 82 77 87 93) 239) 296 0.001)
    (check-within (candidate (list 40 17 28 38 41 32 9) 97) 125 0.001)
    (check-within (candidate (list 41 22 4 41 4 47 36) 25) 0 0.001)
    (check-within (candidate (list 42 19 52 36 8 5 9) 656) 1252 0.001)
    (check-within (candidate (list 45 58 6 16 70 69 87) 26) 0 0.001)
    (check-within (candidate (list 48 24 41 16 4 20 38) 97) 133 0.001)
    (check-within (candidate (list 50 3 17 36 16 10 2) 19) 3 0.001)
    (check-within (candidate (list 57 41 90 16 41 25 2) 934) 1737 0.001)
    (check-within (candidate (list 74 33 13 74 75 95 11) 426) 744 0.001)
    (check-within (candidate (list 83 54 75 22 32 59 30) 298) 462 0.001)
    (check-within (candidate (list 1 9 3 6 3 1 10 4) 2) 0 0.001)
    (check-within (candidate (list 1 29 18 3 38 4 7 47) 67) 87 0.001)
    (check-within (candidate (list 2 1 1 7 2 3 5 6) 9) 13 0.001)
    (check-within (candidate (list 2 3 6 3 0 0 7 4) 8) 8 0.001)
    (check-within (candidate (list 2 9 6 9 1 9 4 0) 6) 0 0.001)
    (check-within (candidate (list 5 1 6 9 5 4 9 2) 10) 6 0.001)
    (check-within (candidate (list 5 10 8 7 6 7 1 10) 3) 0 0.001)
    (check-within (candidate (list 6 7 9 5 0 2 7 7) 1) 0 0.001)
    (check-within (candidate (list 7 2 2 6 7 5 0 2) 3) 0 0.001)
    (check-within (candidate (list 7 12 36 8 27 48 39 35) 36) 0 0.001)
    (check-within (candidate (list 15 47 34 46 42 26 23 11) 15) 0 0.001)
    (check-within (candidate (list 19 40 43 32 15 7 43 5) 85) 120 0.001)
    (check-within (candidate (list 22 45 6 7 7 23 6 3) 43) 56 0.001)
    (check-within (candidate (list 25 1 70 71 54 96 46 77) 549) 932 0.001)
    (check-within (candidate (list 33 41 14 18 43 20 49 23) 25) 0 0.001)
    (check-within (candidate (list 39 26 16 36 19 5 6 28) 98) 175 0.001)
    (check-within (candidate (list 44 44 31 36 1 8 39 46) 45) 16 0.001)
    (check-within (candidate (list 2 17 43 26 33 12 37 28 34) 3) 0 0.001)
    (check-within (candidate (list 3 1 6 1 0 5 4 5 7) 10) 12 0.001)
    (check-within (candidate (list 3 10 4 2 9 8 2 1 4) 7) 3 0.001)
    (check-within (candidate (list 7 5 4 7 6 5 10 8 8) 8) 2 0.001)
    (check-within (candidate (list 7 40 36 45 42 23 10 33 17) 25) 0 0.001)
    (check-within (candidate (list 10 4 7 4 8 7 8 4 6) 3) 0 0.001)
    (check-within (candidate (list 12 32 12 13 18 38 21 15 13) 95) 214 0.001)
    (check-within (candidate (list 17 50 14 27 10 37 24 35 23) 45) 36 0.001)
    (check-within (candidate (list 20 17 29 44 18 20 17 26 2) 1) 0 0.001)
    (check-within (candidate (list 28 4 38 38 37 13 47 48 49) 61) 60 0.001)
    (check-within (candidate (list 29 0 34 5 5 24 43 23 27) 98) 193 0.001)
    (check-within (candidate (list 41 53 77 44 79 66 2 46 64) 204) 405 0.001)
))

(test-humaneval)