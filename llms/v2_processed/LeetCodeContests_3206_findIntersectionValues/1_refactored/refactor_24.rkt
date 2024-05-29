#lang racket

;; You are given two 0-indexed integer arrays nums1 and nums2 of sizes n and m, respectively.
;; Consider calculating the following values:
;; The number of indices i such that 0 <= i < n and nums1[i] occurs at least once in nums2.
;; The number of indices i such that 0 <= i < m and nums2[i] occurs at least once in nums1.
;; Return an integer array answer of size 2 containing the two values in the above order.
;; Example 1:
;; Input: nums1 = [4,3,2,3,1], nums2 = [2,2,5,2,3,6]
;; Output: [3,4]
;; Explanation: We calculate the values as follows:
;; - The elements at indices 1, 2, and 3 in nums1 occur at least once in nums2. So the first value is 3.
;; - The elements at indices 0, 1, 3, and 4 in nums2 occur at least once in nums1. So the second value is 4.
;; Example 2:
;; Input: nums1 = [3,4,2,3], nums2 = [1,5]
;; Output: [0,0]
;; Explanation: There are no common elements between the two arrays, so the two values will be 0.
;; Constraints:
;; n == nums1.length
;; m == nums2.length
;; 1 <= n, m <= 100
;; 1 <= nums1[i], nums2[i] <= 100
(define (findIntersectionValues nums1 nums2)
  ;; Convert lists to sets to find unique elements
  (define set-nums1 (set (in-list nums1)))
  (define set-nums2 (set (in-list nums2)))

  ;; Find the intersection of the two sets
  (define intersection (set-intersect set-nums1 set-nums2))

  ;; Count the number of elements in nums1 that are in the intersection
  (define count1 (for/sum ([x (in-list nums1)] #:when (set-member? intersection x)) 1))

  ;; Count the number of elements in nums2 that are in the intersection
  (define count2 (for/sum ([x (in-list nums2)] #:when (set-member? intersection x)) 1))

  ;; Return the counts as a list
  (list count1 count2))

;; Example usage:
(displayln (findIntersectionValues '(4 3 2 3 1) '(2 2 5 2 3 6)))  ; Output: '(3 4)
(displayln (findIntersectionValues '(3 4 2 3) '(1 5)))            ; Output: '(0 0)

(require rackunit)


(define (test-humaneval) 

  (let (( candidate findIntersectionValues))
    (check-within (candidate (list 4 3 2 3 1) (list 2 2 5 2 3 6)) (list 3 4) 0.001)
    (check-within (candidate (list 3 4 2 3) (list 1 5)) (list 0 0) 0.001)
    (check-within (candidate (list 24 28 7 27 7 27 9 24 9 10) (list 12 29 9 7 5)) (list 4 2) 0.001)
    (check-within (candidate (list 10 30 16 18) (list 23 30 30 6 10 26 9 27 6 16 18 10 27 2 20 7 16)) (list 4 7) 0.001)
    (check-within (candidate (list 7 23 27 20 21 29 7 27 27 18 7 6 20 10) (list 27 27 28 24 20 4 6 17 9 29 20 14 20)) (list 7 7) 0.001)
    (check-within (candidate (list 15 30 6 6) (list 15 4 16 10 7 23 24 3 4 6 14 8 18 1 29 27 2 17)) (list 3 2) 0.001)
    (check-within (candidate (list 24 7 8 6 22 28 22 28 7 19) (list 3 7 28 7 3 3)) (list 4 3) 0.001)
    (check-within (candidate (list 23 4 26 17 23 13) (list 24 17 20 16 1 13 17 28 17)) (list 2 4) 0.001)
    (check-within (candidate (list 5 8 18 27 16 29 27 12 1 29 16 27 22 19 14 12 11 25) (list 24 8 16)) (list 3 2) 0.001)
    (check-within (candidate (list 29 17 30 17 15 30 11 2 24 28 28 30 30 27 30 2 30 9 1 7) (list 12 12 11 21 2 28 5 24 12 17 24 29 22 19 11 17 1 23)) (list 10 10) 0.001)
    (check-within (candidate (list 4 27 12 16 16 21 26 7 19 21 24 26 12 24 22 12 16) (list 1 25 8 27 23 27 27 24)) (list 3 4) 0.001)
    (check-within (candidate (list 27 19 20 16 24 27 27 24) (list 30 21 21 6 17 16)) (list 1 1) 0.001)
    (check-within (candidate (list 3 19 21 5 24 26 22 22 5) (list 23 26 20 14 30 9 10 24 19 22 19 6 3 20 22 22 5 24 24)) (list 8 11) 0.001)
    (check-within (candidate (list 13 13 29 12) (list 29 29 13 7 30 22)) (list 3 3) 0.001)
    (check-within (candidate (list 30 4 16 14 14 14 20 15 20 30 6 10 14) (list 30 16 20 2 18 10 5 6 30 20 22 18 14 23 15)) (list 12 9) 0.001)
    (check-within (candidate (list 22 1 22 4 11 22 4 20 11 29 11 11 4 26 20 12 20 8 26 17) (list 4 17 7 15)) (list 4 2) 0.001)
    (check-within (candidate (list 30 15 16 15 11 16 26 15 21) (list 22 25 27 2 26 20 18 15 26 20 16)) (list 6 4) 0.001)
    (check-within (candidate (list 5 6) (list 13 12 8 5 19 13 27)) (list 1 1) 0.001)
    (check-within (candidate (list 27 28 15 20 5 13 28 29 24 29 20 15 5 20 20 25 9 20 24 20) (list 16 20 13 24 11)) (list 9 3) 0.001)
    (check-within (candidate (list 25 7 18) (list 28 1 14 22 24 8 25 17)) (list 1 1) 0.001)
    (check-within (candidate (list 10 15) (list 4 10 15 28)) (list 2 2) 0.001)
    (check-within (candidate (list 11 11 25) (list 11 28 25 13 23 11)) (list 3 3) 0.001)
    (check-within (candidate (list 10 30 27 8 8 5 11 12 17 13 14 27 17 19 13 20 27) (list 10 14 25 2 17 29 10 9 5 30 15 27)) (list 9 7) 0.001)
    (check-within (candidate (list 19 22 22 22 22 29 22 28 29) (list 7 28 29 22 16 22 22 4 17 11 22 22 22 25 25)) (list 8 8) 0.001)
    (check-within (candidate (list 18 1 23 1 1) (list 16 9 1 4 15 11)) (list 3 1) 0.001)
    (check-within (candidate (list 30 11 15 1 15 6 5 26 15 15) (list 1 20 19 30 17 10 6 15)) (list 7 4) 0.001)
    (check-within (candidate (list 17 6 30 30 15 30 22 2 18 22 21 21 17 19 25 30 18 30 1) (list 2 16 25 5 25 1 14 11)) (list 3 4) 0.001)
    (check-within (candidate (list 3 21 21 23 14) (list 1 28 1 3 27 15 28 29 22 14 8 24)) (list 2 2) 0.001)
    (check-within (candidate (list 8 20 29 23 29 2 2 2 20) (list 2 24 20 28 11 8 6 25)) (list 6 3) 0.001)
    (check-within (candidate (list 22 27 4 27 30 22 25 8 8 30 1 16 1) (list 9 21 8 12)) (list 2 1) 0.001)
    (check-within (candidate (list 19 11 13 1 26 25 19 24 3 10 1 11 1 15 20 20 26 13 13) (list 13 23)) (list 3 1) 0.001)
    (check-within (candidate (list 21 16 11 21) (list 21 11 21 2 2 8 16 29 16 16 18 14 18 16 29 10 2)) (list 4 7) 0.001)
    (check-within (candidate (list 15 7 23 12 23 16 18 1 16 28 28 19 7 30 19) (list 9 1 10 15 23 8 8 24 30)) (list 5 4) 0.001)
    (check-within (candidate (list 2 2 22 24 20 22 1 27 27 10 8 26 22 22 22 10 13 29) (list 8 11 1 11)) (list 2 2) 0.001)
    (check-within (candidate (list 25 29 15 15 21 14 10 23 10 18 11 30 28 16 29) (list 1 16 10 2 25 1 15)) (list 6 4) 0.001)
    (check-within (candidate (list 18 18 11 27 18 20 20) (list 16 28 25 28 20 15 8 21 4 6 19 20 20 20 29)) (list 2 4) 0.001)
    (check-within (candidate (list 1 25 15 20 25 11 4 1 1 21 17 1 19) (list 19 19 9 23 1 5 28 28 17 28 3 9 8)) (list 6 4) 0.001)
    (check-within (candidate (list 7 30 7 7 30 2 7 7) (list 19 7 1 7 17 17 20 7 21 30 8 21 10 30 14)) (list 7 5) 0.001)
    (check-within (candidate (list 7 18 13 27 13 9 22 30) (list 27 29 21 30 16 13 29 5 9 16 29 27)) (list 5 5) 0.001)
    (check-within (candidate (list 19 19 25 24 24 3 19 24 3) (list 16 19 19 17 19 24 5 19)) (list 6 5) 0.001)
    (check-within (candidate (list 19 11 3 11 22 12 23 12 29 19 25 15 23 23) (list 4 29 19 23 23 10 2 10 10 15 19 20 19 12 2 19 15 29)) (list 9 11) 0.001)
    (check-within (candidate (list 25 21) (list 20 12 5 13 21 25 9 30 21 7 21 12 20 7)) (list 2 4) 0.001)
    (check-within (candidate (list 16 17 16 20 29 16 30 24) (list 1 30 24)) (list 2 2) 0.001)
    (check-within (candidate (list 10 6 7 24 17 24 3 24) (list 24 27 26 8 7 3 19 24 6 7 30 6)) (list 6 7) 0.001)
    (check-within (candidate (list 3 26 7 6 23 22 26 8 11 23 17 26 7 2) (list 13 11 10 8 4 23)) (list 4 3) 0.001)
    (check-within (candidate (list 29 10 9 26 30 21 11 26 30) (list 2 9 12 9 30 9 30 21 8 3 17 15 25 26 9 15)) (list 6 8) 0.001)
    (check-within (candidate (list 14 29 15 12 20 27 24 29 4 29 12 6 12 4 7) (list 2 19 6 29 10 20 26 11 11 19 4 12 30 22 13 4 24)) (list 11 7) 0.001)
    (check-within (candidate (list 11 5 3 4 15 30 25 25 30 6 3 28 25 6 30 17 15) (list 4 25 17 2 24 28 25 15 4 25 8 6 15)) (list 10 10) 0.001)
    (check-within (candidate (list 5 23 17 6 5 15 29 2 7 27 5) (list 28 14 1 1 27 26 23 20 6 17 11)) (list 4 4) 0.001)
    (check-within (candidate (list 26 20 12 2 11 23 8 28 28 2 28 20 2 13 13 28 22) (list 8 7 12 15 20)) (list 4 3) 0.001)
    (check-within (candidate (list 15 6 14 24 6 22 6 24 6 6 6 16 24 3 7 6) (list 11 6 18 20 12 14 17 3 11 6 2 3 17 19 3)) (list 9 6) 0.001)
    (check-within (candidate (list 21 10 13 2 3 29 2 29 12 21 16 7 21 26) (list 26 16 18 29 16 15 2 16 23 24 26 21 26 13 4 29 13 17 10)) (list 11 13) 0.001)
    (check-within (candidate (list 5 18 7 30 16 1 24 5 1 15 28 24 25) (list 20 29 16 14)) (list 1 1) 0.001)
    (check-within (candidate (list 1 11 11 28 28 10 15 28 6) (list 27 21 28 18 7 7 20 26 4 28 11 22 16 30 11 9 9)) (list 5 4) 0.001)
    (check-within (candidate (list 27 3) (list 29 29 27 1 26 21 27 1 8 3 7 24 19)) (list 2 3) 0.001)
    (check-within (candidate (list 19 20 25 16 22 23 25 16 23 16 23 14) (list 16 5)) (list 3 1) 0.001)
    (check-within (candidate (list 9 9 5 28 22 15 11 28 5 3 15 6 16 13 29 30) (list 18 12 3 5 24 15)) (list 5 3) 0.001)
    (check-within (candidate (list 21 19 11 24 7 5 10) (list 19 19 14 3 4 14 27 18 14 10)) (list 2 3) 0.001)
    (check-within (candidate (list 6 18 18 20 5 18 1 15 18 26 28 26) (list 13 12 2 24 20 28 27 20 11)) (list 2 3) 0.001)
    (check-within (candidate (list 18 14 14 15 10 14 7 1 28 15) (list 11 18 15 18 27 12)) (list 3 3) 0.001)
    (check-within (candidate (list 29 18 29 18 27 11 11 8 4 18 11 14 5 21 21 29 11) (list 25 29 15 17 27 20 9 23 11 13 26 8 11 6)) (list 9 5) 0.001)
    (check-within (candidate (list 14 5 8 21 24 5 21 19 29) (list 15 10 9 13 24 4 9 10 3 6 5 20 24 26 14 27 14 10 22)) (list 4 5) 0.001)
    (check-within (candidate (list 2 11 11 9 25 11 27 16 28 10 18 3 22 15 16 11) (list 11 3 21 9 3 13 23 9 28 25 8 28 29 2 23 12 13 14 14)) (list 9 9) 0.001)
    (check-within (candidate (list 12 11 23 17 23 3 17) (list 18 20 8 29 28 27 14 28 13 25 24 2 11 23)) (list 3 2) 0.001)
    (check-within (candidate (list 8 18 7 7 7 24 16 8 23 23 16 16 3 16 22 18 8) (list 29 3 14 22 17 22 25 25 1 23 6 23 7 12 16)) (list 11 7) 0.001)
    (check-within (candidate (list 25 9 11 13 21 3 7 24 29 14 2 7 18 30 18) (list 2 3 28 3 25 25 21 10 4 19 23 11 27)) (list 5 7) 0.001)
    (check-within (candidate (list 5 8 12 18 5 8) (list 12 19 30 16 13)) (list 1 1) 0.001)
    (check-within (candidate (list 14 22 29 29 3 22 4 29 28 27) (list 14 29)) (list 4 2) 0.001)
    (check-within (candidate (list 28 28 11 5 18 5 18 17 21 4 9 4) (list 19 6 12 17 13)) (list 1 1) 0.001)
    (check-within (candidate (list 24 29 19 25 7 26 7 25 7 25 2) (list 9 4 2 20 29 1 27)) (list 2 2) 0.001)
    (check-within (candidate (list 19 14 14 21 14 11 21 18 11 14 18 28 4) (list 25 30 1)) (list 0 0) 0.001)
    (check-within (candidate (list 9 17 21 21 18 9 9 16 9 3 17 9 3) (list 9 10 20 7 3 13 13 22 15)) (list 7 2) 0.001)
    (check-within (candidate (list 21 14 14 14 5 11 8 7 9 3 7 3) (list 2 24 28 8 15 5 3 6 14 3 19 25 5)) (list 7 6) 0.001)
    (check-within (candidate (list 3 17 13 18 18 12 5 12 27 6 3 13 7 3 12 27 6) (list 17 28 13 26 12 27 20 12 27 7 10 24)) (list 9 7) 0.001)
    (check-within (candidate (list 18 9 30 9 3 13 25 5 30 25 13 19 25 3 28 29 9 9 9 12) (list 17 20 28 30 27 1 22)) (list 3 2) 0.001)
    (check-within (candidate (list 18 19 13 20 26 26 13 13 26 25 22 20 17) (list 3 21 12 12 18 20 26 17 30 6 22 13)) (list 11 6) 0.001)
    (check-within (candidate (list 19 10 2 18 15 24 4 11 12 24 10 10 9 12 6 10 17 22 11 12) (list 10 16 7 2 27 22 19 17 11 15 27 24)) (list 13 8) 0.001)
    (check-within (candidate (list 8 8) (list 8 24 8 8 19 27 7 21 8 8)) (list 2 5) 0.001)
    (check-within (candidate (list 22 23 22) (list 22 21 3 22 17 27)) (list 2 2) 0.001)
    (check-within (candidate (list 20 10) (list 10 20 12)) (list 2 2) 0.001)
    (check-within (candidate (list 15 28 15 17 3 6) (list 3 15 17 30 18 22 4)) (list 4 3) 0.001)
    (check-within (candidate (list 30 15) (list 15 25 23 26 14 30 8 19 15 8 10 14 26 15 28 30)) (list 2 5) 0.001)
    (check-within (candidate (list 16 11 16 24 7 9 9 24) (list 19 2 9 18 25 11 1 16 24 18 20 9 24 7 9 29 16 22 15)) (list 8 9) 0.001)
    (check-within (candidate (list 13 25 8 8 1 14 8 4 8 8 25 8 22) (list 17 8 13 8 8 20 26 20 8 22 17 14 8 16 26 2 23 18 18 4)) (list 10 9) 0.001)
    (check-within (candidate (list 9 9 9 29 11 9 18 23 9 9 26 9 23 9 9 2 28 7) (list 17 6 2 11 10 23 11 30 11 24 1 11 13 9 23 25)) (list 13 8) 0.001)
    (check-within (candidate (list 27 16 16 15) (list 3 16 7 16 23 16 3 26 27 30 4 28 26 24 7)) (list 3 4) 0.001)
    (check-within (candidate (list 19 1 26 15 15) (list 6 25)) (list 0 0) 0.001)
    (check-within (candidate (list 13 29 29 12 25 22 2 25 11 3 22 13 23 19 24 24 8 30) (list 20 25 12 5 2 28 14 27 24 3 21 15 25 2 12 28 19 7 5)) (list 8 9) 0.001)
    (check-within (candidate (list 14 14 26 25 28 26) (list 4 23 9 3 1 2 27 8 22 6 24)) (list 0 0) 0.001)
    (check-within (candidate (list 14 29 2 26 14 10 1 23 28 5 17 1 21 5 28 14 6 4 18) (list 1 20 7 15 18 26 5 10 8 6 27)) (list 8 6) 0.001)
    (check-within (candidate (list 29 3 15 7 2 20 30 15 7 29 2 21) (list 15 23 22 19 21 5 7 29 23 2 17 27 21 15 6 7)) (list 9 8) 0.001)
    (check-within (candidate (list 7 23 23 15 23 10 30 23 30 10 30 17 30 10 3 7 10) (list 21 21)) (list 0 0) 0.001)
    (check-within (candidate (list 8 13 1 13 13 12 27 21 4 4 17) (list 12 13 1 27 4 9 12 8 25 29 4 8 4 29 21 28 1 8 6 6)) (list 10 13) 0.001)
    (check-within (candidate (list 6 15 7 1 7 14 21 3 30 23 22 29) (list 30 1 7 29 3 4)) (list 6 5) 0.001)
    (check-within (candidate (list 15 10 22 22 6 8 15 8 10) (list 10 4 8 15 29 6 9 22 3 3 23 3 13 8 5 8 3)) (list 9 7) 0.001)
    (check-within (candidate (list 14 4 1 27 22 14 7 22 15 3 22 8) (list 30 4 4 27 6 4 16 11 23 14 4 7 21 22 9 14 4 27 17 27)) (list 8 12) 0.001)
    (check-within (candidate (list 23 15 15 15) (list 23 17 12 15 21)) (list 4 2) 0.001)
    (check-within (candidate (list 28 29 15 19 1 23 25 9 29 25 19 11 9 19) (list 9 4 11 23 13 8 24 9 23)) (list 4 5) 0.001)
    (check-within (candidate (list 19 24 7 2 3 10 27 10 4 4 9 29 10 7) (list 23 4 7 4 27 13 2 9 23)) (list 7 6) 0.001)
    (check-within (candidate (list 24 22 17 24 22 16 1 5) (list 1 27 7 22 27 13 4 5 12 8 22 18 5)) (list 4 5) 0.001)
))

(test-humaneval)