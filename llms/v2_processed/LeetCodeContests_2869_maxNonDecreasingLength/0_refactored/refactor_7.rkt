#lang racket

;; Function to calculate the maximum length of a non-decreasing subarray
;; that can be formed by choosing elements from either of two given lists.
;; You are given two 0-indexed integer arrays nums1 and nums2 of length n.
;; Let's define another 0-indexed integer array, nums3, of length n. For each index i in the range [0, n - 1], you can assign either nums1[i] or nums2[i] to nums3[i].
;; Your task is to maximize the length of the longest non-decreasing subarray in nums3 by choosing its values optimally.
;; Return an integer representing the length of the longest non-decreasing subarray in nums3.
;; Note: A subarray is a contiguous non-empty sequence of elements within an array.
;; Example 1:
;; Input: nums1 = [2,3,1], nums2 = [1,2,1]
;; Output: 2
;; Explanation: One way to construct nums3 is:
;; nums3 = [nums1[0], nums2[1], nums2[2]] => [2,2,1].
;; The subarray starting from index 0 and ending at index 1, [2,2], forms a non-decreasing subarray of length 2.
;; We can show that 2 is the maximum achievable length.
;; Example 2:
;; Input: nums1 = [1,3,2,1], nums2 = [2,2,3,4]
;; Output: 4
;; Explanation: One way to construct nums3 is:
;; nums3 = [nums1[0], nums2[1], nums2[2], nums2[3]] => [1,2,3,4].
;; The entire array forms a non-decreasing subarray of length 4, making it the maximum achievable length.
;; Example 3:
;; Input: nums1 = [1,1], nums2 = [2,2]
;; Output: 2
;; Explanation: One way to construct nums3 is:
;; nums3 = [nums1[0], nums1[1]] => [1,1].
;; The entire array forms a non-decreasing subarray of length 2, making it the maximum achievable length.
;; Constraints:
;;  * 1 <= nums1.length == nums2.length == n <= 105
;;  * 1 <= nums1[i], nums2[i] <= 109
(define (maxNonDecreasingLength nums1 nums2)
  (define n (length nums1))

  ;; Helper function to compute the maximum length of non-decreasing subarray
  (define (compute-max-length nums1 nums2)
    (for/fold ([dp1 1] [dp2 1] [max-len 1] #:result max-len)
              ([i (in-range 1 n)])
      (define val1 (list-ref nums1 i))
      (define val2 (list-ref nums2 i))
      (define prev-val1 (list-ref nums1 (sub1 i)))
      (define prev-val2 (list-ref nums2 (sub1 i)))

      (define new-dp1
        (max (if (>= val1 prev-val1) (+ dp1 1) 1)
             (if (>= val1 prev-val2) (+ dp2 1) 1)))

      (define new-dp2
        (max (if (>= val2 prev-val1) (+ dp1 1) 1)
             (if (>= val2 prev-val2) (+ dp2 1) 1)))

      (values new-dp1 new-dp2 (max max-len new-dp1 new-dp2))))

  ;; Compute and return the maximum length
  (compute-max-length nums1 nums2))

(require rackunit)


(define (test-humaneval) 

  (let (( candidate maxNonDecreasingLength))
    (check-within (candidate (list 2 3 1) (list 1 2 1)) 2 0.001)
    (check-within (candidate (list 1 3 2 1) (list 2 2 3 4)) 4 0.001)
    (check-within (candidate (list 1 1) (list 2 2)) 2 0.001)
    (check-within (candidate (list 1) (list 1)) 1 0.001)
    (check-within (candidate (list 1) (list 2)) 1 0.001)
    (check-within (candidate (list 1 4) (list 4 19)) 2 0.001)
    (check-within (candidate (list 1 8) (list 10 1)) 2 0.001)
    (check-within (candidate (list 1 11) (list 9 1)) 2 0.001)
    (check-within (candidate (list 1 13) (list 18 1)) 2 0.001)
    (check-within (candidate (list 1 19) (list 12 20)) 2 0.001)
    (check-within (candidate (list 1 19) (list 18 9)) 2 0.001)
    (check-within (candidate (list 2 20) (list 1 18)) 2 0.001)
    (check-within (candidate (list 3 5) (list 13 3)) 2 0.001)
    (check-within (candidate (list 3 6) (list 10 12)) 2 0.001)
    (check-within (candidate (list 3 7) (list 8 3)) 2 0.001)
    (check-within (candidate (list 3 8) (list 15 2)) 2 0.001)
    (check-within (candidate (list 3 9) (list 11 3)) 2 0.001)
    (check-within (candidate (list 3 12) (list 7 3)) 2 0.001)
    (check-within (candidate (list 3 12) (list 20 3)) 2 0.001)
    (check-within (candidate (list 3 20) (list 5 17)) 2 0.001)
    (check-within (candidate (list 4 2) (list 10 4)) 2 0.001)
    (check-within (candidate (list 4 12) (list 6 4)) 2 0.001)
    (check-within (candidate (list 4 15) (list 3 3)) 2 0.001)
    (check-within (candidate (list 5 5) (list 19 8)) 2 0.001)
    (check-within (candidate (list 5 7) (list 19 5)) 2 0.001)
    (check-within (candidate (list 5 11) (list 2 5)) 2 0.001)
    (check-within (candidate (list 5 14) (list 8 3)) 2 0.001)
    (check-within (candidate (list 5 15) (list 16 5)) 2 0.001)
    (check-within (candidate (list 5 20) (list 4 5)) 2 0.001)
    (check-within (candidate (list 5 20) (list 10 17)) 2 0.001)
    (check-within (candidate (list 6 7) (list 3 2)) 2 0.001)
    (check-within (candidate (list 6 14) (list 5 5)) 2 0.001)
    (check-within (candidate (list 6 14) (list 18 6)) 2 0.001)
    (check-within (candidate (list 6 16) (list 16 20)) 2 0.001)
    (check-within (candidate (list 6 17) (list 4 20)) 2 0.001)
    (check-within (candidate (list 7 3) (list 16 7)) 2 0.001)
    (check-within (candidate (list 7 4) (list 15 7)) 2 0.001)
    (check-within (candidate (list 7 9) (list 3 18)) 2 0.001)
    (check-within (candidate (list 7 10) (list 10 14)) 2 0.001)
    (check-within (candidate (list 7 11) (list 5 7)) 2 0.001)
    (check-within (candidate (list 7 12) (list 20 5)) 2 0.001)
    (check-within (candidate (list 7 20) (list 12 8)) 2 0.001)
    (check-within (candidate (list 8 5) (list 13 8)) 2 0.001)
    (check-within (candidate (list 8 11) (list 9 3)) 2 0.001)
    (check-within (candidate (list 8 16) (list 9 8)) 2 0.001)
    (check-within (candidate (list 8 17) (list 2 8)) 2 0.001)
    (check-within (candidate (list 8 18) (list 16 12)) 2 0.001)
    (check-within (candidate (list 4 2) (list 10 4)) 2 0.001)
    (check-within (candidate (list 9 1) (list 11 9)) 2 0.001)
    (check-within (candidate (list 9 6) (list 8 14)) 2 0.001)
    (check-within (candidate (list 9 9) (list 11 8)) 2 0.001)
    (check-within (candidate (list 9 12) (list 20 9)) 2 0.001)
    (check-within (candidate (list 9 15) (list 20 9)) 2 0.001)
    (check-within (candidate (list 9 16) (list 11 15)) 2 0.001)
    (check-within (candidate (list 9 19) (list 17 9)) 2 0.001)
    (check-within (candidate (list 10 19) (list 17 10)) 2 0.001)
    (check-within (candidate (list 11 1) (list 3 11)) 2 0.001)
    (check-within (candidate (list 11 3) (list 9 17)) 2 0.001)
    (check-within (candidate (list 11 6) (list 9 17)) 2 0.001)
    (check-within (candidate (list 11 19) (list 17 11)) 2 0.001)
    (check-within (candidate (list 11 69) (list 26 62)) 2 0.001)
    (check-within (candidate (list 12 1) (list 10 12)) 2 0.001)
    (check-within (candidate (list 12 10) (list 16 2)) 1 0.001)
    (check-within (candidate (list 13 6) (list 20 13)) 2 0.001)
    (check-within (candidate (list 13 16) (list 5 13)) 2 0.001)
    (check-within (candidate (list 14 2) (list 2 14)) 2 0.001)
    (check-within (candidate (list 14 4) (list 2 13)) 2 0.001)
    (check-within (candidate (list 15 10) (list 17 15)) 2 0.001)
    (check-within (candidate (list 15 11) (list 19 2)) 1 0.001)
    (check-within (candidate (list 16 9) (list 5 16)) 2 0.001)
    (check-within (candidate (list 16 17) (list 9 16)) 2 0.001)
    (check-within (candidate (list 17 8) (list 11 10)) 1 0.001)
    (check-within (candidate (list 17 10) (list 9 17)) 2 0.001)
    (check-within (candidate (list 17 10) (list 18 7)) 1 0.001)
    (check-within (candidate (list 17 11) (list 19 17)) 2 0.001)
    (check-within (candidate (list 17 14) (list 17 17)) 2 0.001)
    (check-within (candidate (list 17 17) (list 15 17)) 2 0.001)
    (check-within (candidate (list 17 17) (list 16 1)) 2 0.001)
    (check-within (candidate (list 18 4) (list 1 6)) 2 0.001)
    (check-within (candidate (list 18 9) (list 10 18)) 2 0.001)
    (check-within (candidate (list 18 9) (list 17 18)) 2 0.001)
    (check-within (candidate (list 18 10) (list 1 18)) 2 0.001)
    (check-within (candidate (list 18 104) (list 117 18)) 2 0.001)
    (check-within (candidate (list 19 2) (list 1 19)) 2 0.001)
    (check-within (candidate (list 19 5) (list 15 5)) 1 0.001)
    (check-within (candidate (list 19 5) (list 52 10)) 1 0.001)
    (check-within (candidate (list 19 15) (list 12 19)) 2 0.001)
    (check-within (candidate (list 19 15) (list 18 4)) 1 0.001)
    (check-within (candidate (list 20 1) (list 1 20)) 2 0.001)
    (check-within (candidate (list 20 5) (list 2 3)) 2 0.001)
    (check-within (candidate (list 20 5) (list 14 8)) 1 0.001)
    (check-within (candidate (list 20 7) (list 12 20)) 2 0.001)
    (check-within (candidate (list 20 12) (list 2 20)) 2 0.001)
    (check-within (candidate (list 20 16) (list 8 5)) 2 0.001)
    (check-within (candidate (list 20 18) (list 18 20)) 2 0.001)
    (check-within (candidate (list 25 83) (list 28 18)) 2 0.001)
    (check-within (candidate (list 26 47) (list 87 26)) 2 0.001)
    (check-within (candidate (list 28 41) (list 87 3)) 2 0.001)
    (check-within (candidate (list 36 53) (list 66 3)) 2 0.001)
    (check-within (candidate (list 44 34) (list 53 44)) 2 0.001)
))

(test-humaneval)