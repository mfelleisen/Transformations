#lang racket

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
  (let* ([n (length nums1)]
         [m (length nums2)]
         ;; Initialize the dp matrix with negative infinity. 
         ;; The matrix has n+1 rows and m+1 columns.
         [dp (for/list ([i (in-range (add1 n))])
               (make-vector (add1 m) -inf.0))])
    ;; Loop through each cell in the dp matrix to fill it.
    (for ([i (in-range 1 (add1 n))])
      (for ([j (in-range 1 (add1 m))])
        (vector-set! (list-ref dp i) j
                     (max (vector-ref (list-ref dp i) j)
                          (vector-ref (list-ref dp (sub1 i)) j)
                          (vector-ref (list-ref dp i) (sub1 j))
                          (+ (vector-ref (list-ref dp (sub1 i)) (sub1 j))
                             (* (list-ref nums1 (sub1 i))
                                (list-ref nums2 (sub1 j))))
                          (* (list-ref nums1 (sub1 i))
                             (list-ref nums2 (sub1 j)))))))
    ;; Return the value in the bottom-right corner of the dp matrix, 
    ;; which represents the maximum dot product.
    (vector-ref (list-ref dp n) m)))

;; The function 'maxDotProduct' calculates the maximum dot product between non-empty subsequences
;; of two lists nums1 and nums2 of the same length. It utilizes dynamic programming to efficiently
;; solve the problem, by constructing a 2D matrix 'dp' where each cell (i, j) represents the maximum
;; dot product between subsequences of the first i elements of nums1 and the first j elements of nums2.
;; The solution is found by iteratively filling this matrix based on the optimal substructure property.
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