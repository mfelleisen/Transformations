#lang racket

;; You are given an integer list nums and a positive integer k. You can choose any subsequence of the list and sum all of its elements together.
;; We define the K-Sum of the list as the kth largest subsequence sum that can be obtained (not necessarily distinct).
;; Return the K-Sum of the list.
;; A subsequence is a list that can be derived from another list by deleting some or no elements without changing the order of the remaining elements.
;; Note that the empty subsequence is considered to have a sum of 0.
;; Example 1:
;; Input: nums = [2,4,-2], k = 5
;; Output: 2
;; Explanation: All the possible subsequence sums that we can obtain are the following sorted in decreasing order:
;; - 6, 4, 4, 2, 2, 0, 0, -2.
;; The 5-Sum of the list is 2.
;; Example 2:
;; Input: nums = [1,-2,3,4,-10,12], k = 16
;; Output: 10
;; Explanation: The 16-Sum of the list is 10.
;; Constraints:
;; n == nums.length
;; 1 <= n <= 105
;; -109 <= nums[i] <= 109
;; 1 <= k <= min(2000, 2n)
(define (kSum nums k)
  ;; Initialize the list of sums with 0 as the starting point
  (let ([sums (list 0)])
    ;; Iterate over each number in the input list
    (for ([num nums])
      (if (> num 0)
          ;; If the number is positive, create new sums by adding this number to each existing sum,
          ;; then merge this with the existing sums and sort in descending order
          (set! sums (sort (append sums (map (lambda (s) (+ s num)) sums)) >))
          ;; If the number is non-positive, simply consider adding it as a new sum
          (set! sums (cons num sums))))
    ;; Return the k-th largest sum, adjusting for 0-based indexing
    (list-ref sums (- k 1))))

;; Example usage:
;; (kSum [2 4 -2] 5) should return 2
;; (kSum [1 -2 3 4 -10 12] 16) should return 10
(require rackunit)

(define (test-humaneval) 

  (let (( candidate kSum))
    (check-within (candidate (list -1 4) 2) 3 0.001)
    (check-within (candidate (list 0 0 0 0 0) 1) 0 0.001)
    (check-within (candidate (list 0 0) 1) 0 0.001)
    (check-within (candidate (list 1 1 1 1 1 1 1 1 1 1) 10) 9 0.001)
    (check-within (candidate (list 1 2 3 4 5) 1) 15 0.001)
    (check-within (candidate (list 0 0 0 0 0) 1) 0 0.001)
))

(test-humaneval)