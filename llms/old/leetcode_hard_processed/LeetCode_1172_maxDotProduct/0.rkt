#lang racket

;; Dynamic programming approach to find the maximum dot product between non-empty subsequences of two lists
;; Given two lists nums1 and nums2.
;; Return the maximum dot product between non-empty subsequences of nums1 and nums2 with the same length.
;; A subsequence of a list is a new list which is formed from the original list by deleting some (can be none) of the characters without disturbing the relative positions of the remaining characters. (ie, [2,3,5] is a subsequence of [1,2,3,4,5] while [1,5,3] is not).
;; Example 1:
;; Input: nums1 = [2,1,-2,5], nums2 = [3,0,-6]
;; Output: 18
;; Explanation: Take subsequence [2,-2] from nums1 and subsequence [3,-6] from nums2.
;; Their dot product is (2*3 + (-2)*(-6)) = 18.
;; Example 2:
;; Input: nums1 = [3,-2], nums2 = [2,-6,7]
;; Output: 21
;; Explanation: Take subsequence [3] from nums1 and subsequence [7] from nums2.
;; Their dot product is (3*7) = 21.
;; Example 3:
;; Input: nums1 = [-1,-1], nums2 = [1,1]
;; Output: -1
;; Explanation: Take subsequence [-1] from nums1 and subsequence [1] from nums2.
;; Their dot product is -1.
;; Constraints:
;; 1 <= nums1.length, nums2.length <= 500
;; -1000 <= nums1[i], nums2[i] <= 1000
(define (maxDotProduct nums1 nums2)
  (let* ((n (length nums1))
         (m (length nums2))
         ;; Initialize the DP table with negative infinity. The table size is (n+1) x (m+1)
         ;; to account for the case where no element is chosen from either list
         (dp (make-matrix (+ n 1) (+ m 1) -inf.0)))
    ;; Populate the DP table
    (for ([i (in-range 1 (+ n 1))])
      (for ([j (in-range 1 (+ m 1))])
        ;; Update the DP table with the maximum of four options:
        ;; 1. Ignoring the current element of nums1
        ;; 2. Ignoring the current element of nums2
        ;; 3. Ignoring both current elements
        ;; 4. Taking the product of the current elements and adding it to the best score of the subproblem without these elements
        (matrix-set! dp i j (max (matrix-ref dp i j)
                                 (matrix-ref dp (sub1 i) j)
                                 (matrix-ref dp i (sub1 j))
                                 (+ (matrix-ref dp (sub1 i) (sub1 j)) (* (list-ref nums1 (sub1 i)) (list-ref nums2 (sub1 j))))
                                 (* (list-ref nums1 (sub1 i)) (list-ref nums2 (sub1 j)))))))
    ;; The answer is in dp[n][m], the bottom-right corner of the matrix
    (matrix-ref dp n m)))

;; Helper function to create a 2D matrix with default value
(define (make-matrix rows cols fill)
  (for/list ([i (in-range rows)])
    (make-vector cols fill)))

;; Helper function to set a value in a 2D matrix
(define (matrix-set! matrix row col val)
  (vector-set! (list-ref matrix row) col val))

;; Helper function to get a value from a 2D matrix
(define (matrix-ref matrix row col)
  (vector-ref (list-ref matrix row) col))
(require rackunit)

(define (test-humaneval) 

  (let (( candidate maxDotProduct))
    (check-within (candidate (list 1 2 3 4 5) (list 1 2 3 4 5)) 55 0.001)
    (check-within (candidate (list -4 4) (list 2 -2)) 8 0.001)
    (check-within (candidate (list -1 -1 -1) (list 1 1 1)) -1 0.001)
    (check-within (candidate (list -4 4) (list 0 0)) 0 0.001)
    (check-within (candidate (list 1 0 1) (list 1 0 1)) 2 0.001)
    (check-within (candidate (list -1 2 3) (list -1 0 1)) 4 0.001)
    (check-within (candidate (list 1 2 3 4 5) (list 2 3 4 5 6)) 70 0.001)
    (check-within (candidate (list 1 2 3) (list 4 5)) 23 0.001)
    (check-within (candidate (list 1) (list 1)) 1 0.001)
    (check-within (candidate (list 1 2 3) (list 4 5 6)) 32 0.001)
    (check-within (candidate (list -1 2 3) (list 4 -5 6)) 26 0.001)
    (check-within (candidate (list 1 1 1) (list 2 2 2)) 6 0.001)
    (check-within (candidate (list 1 0 1 0 -1 0) (list -1 0 1 1 1 0)) 2 0.001)
    (check-within (candidate (list 0 0 0) (list 0 0 0)) 0 0.001)
    (check-within (candidate (list -1 -1) (list 1 1)) -1 0.001)
    (check-within (candidate (list 1 1 1) (list 1 2 3)) 6 0.001)
    (check-within (candidate (list 2 3 4) (list 5 6 7)) 56 0.001)
    (check-within (candidate (list -1 -1) (list 1 1)) -1 0.001)
    (check-within (candidate (list -2 -1 -1 -1) (list 1 0 1 1)) 0 0.001)
    (check-within (candidate (list 1 2 3) (list 1 1 1)) 6 0.001)
))

(test-humaneval)