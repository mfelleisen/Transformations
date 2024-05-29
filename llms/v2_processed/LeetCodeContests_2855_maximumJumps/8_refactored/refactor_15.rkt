#lang racket

;; Define the function maximumJumps that calculates the maximum number of jumps
;; from the start to the end of the list nums, with the jump condition based on the target.
;; You are given a 0-indexed array nums of n integers and an integer target.
;; You are initially positioned at index 0. In one step, you can jump from index i to any index j such that:
;;  * 0 <= i < j < n
;;  * -target <= nums[j] - nums[i] <= target
;; Return the maximum number of jumps you can make to reach index n - 1.
;; If there is no way to reach index n - 1, return -1.
;; Example 1:
;; Input: nums = [1,3,6,4,1,2], target = 2
;; Output: 3
;; Explanation: To go from index 0 to index n - 1 with the maximum number of jumps, you can perform the following jumping sequence:
;; - Jump from index 0 to index 1.
;; - Jump from index 1 to index 3.
;; - Jump from index 3 to index 5.
;; It can be proven that there is no other jumping sequence that goes from 0 to n - 1 with more than 3 jumps. Hence, the answer is 3.
;; Example 2:
;; Input: nums = [1,3,6,4,1,2], target = 3
;; Output: 5
;; Explanation: To go from index 0 to index n - 1 with the maximum number of jumps, you can perform the following jumping sequence:
;; - Jump from index 0 to index 1.
;; - Jump from index 1 to index 2.
;; - Jump from index 2 to index 3.
;; - Jump from index 3 to index 4.
;; - Jump from index 4 to index 5.
;; It can be proven that there is no other jumping sequence that goes from 0 to n - 1 with more than 5 jumps. Hence, the answer is 5.
;; Example 3:
;; Input: nums = [1,3,6,4,1,2], target = 0
;; Output: -1
;; Explanation: It can be proven that there is no jumping sequence that goes from 0 to n - 1. Hence, the answer is -1.
;; Constraints:
;;  * 2 <= nums.length == n <= 1000
;;  * -109 <= nums[i] <= 109
;;  * 0 <= target <= 2 * 109
(define (maximumJumps nums target)
  (define n (length nums))
  (define inf -inf.0)
  
  ;; Helper function to determine if a jump is possible
  (define (jump-possible? i j)
    (<= (abs (- (list-ref nums j) (list-ref nums i))) target))
  
  ;; Recursive function to update the dp list
  (define (update-dp i dp)
    (if (or (>= i n) (= (list-ref dp i) inf))
        dp
        (foldl (lambda (j dp)
                 (if (jump-possible? i j)
                     (list-set dp j (max (list-ref dp j) (add1 (list-ref dp i))))
                     dp))
               dp
               (range (add1 i) n))))
  
  ;; Initialize the dp list
  (define dp (cons 0 (make-list (sub1 n) inf)))
  
  ;; Update the dp list for each index
  (for ([i (in-range n)])
    (set! dp (update-dp i dp)))
  
  ;; Check the final value in the dp list
  (define final-value (list-ref dp (sub1 n)))
  (if (= final-value inf) -1 final-value))

;; Example usage:
(maximumJumps '(1 3 6 4 1 2) 2)  ;; Output: 3
(maximumJumps '(1 3 6 4 1 2) 3)  ;; Output: 5
(maximumJumps '(1 3 6 4 1 2) 0)  ;; Output: -1

(require rackunit)


(define (test-humaneval) 

  (let (( candidate maximumJumps))
    (check-within (candidate (list 1 3 6 4 1 2) 2) 3 0.001)
    (check-within (candidate (list 1 3 6 4 1 2) 3) 5 0.001)
    (check-within (candidate (list 1 3 6 4 1 2) 0) -1 0.001)
    (check-within (candidate (list 0 1) 0) -1 0.001)
    (check-within (candidate (list 0 1) 1) 1 0.001)
    (check-within (candidate (list 0 1) 2) 1 0.001)
    (check-within (candidate (list 1 0) 0) -1 0.001)
    (check-within (candidate (list 1 0) 1) 1 0.001)
    (check-within (candidate (list 1 0) 2) 1 0.001)
    (check-within (candidate (list 0 1 2) 0) -1 0.001)
    (check-within (candidate (list 0 1 2) 1) 2 0.001)
    (check-within (candidate (list 0 1 2) 2) 2 0.001)
    (check-within (candidate (list 0 1 2) 3) 2 0.001)
    (check-within (candidate (list 0 2 1) 0) -1 0.001)
    (check-within (candidate (list 0 2 1) 1) 1 0.001)
    (check-within (candidate (list 0 2 1) 2) 2 0.001)
    (check-within (candidate (list 0 2 1) 3) 2 0.001)
    (check-within (candidate (list 1 0 2) 0) -1 0.001)
    (check-within (candidate (list 1 0 2) 1) 1 0.001)
    (check-within (candidate (list 1 0 2) 2) 2 0.001)
    (check-within (candidate (list 1 0 2) 3) 2 0.001)
    (check-within (candidate (list 1 2 0) 0) -1 0.001)
    (check-within (candidate (list 1 2 0) 1) 1 0.001)
    (check-within (candidate (list 1 2 0) 2) 2 0.001)
    (check-within (candidate (list 1 2 0) 3) 2 0.001)
    (check-within (candidate (list 2 0 1) 0) -1 0.001)
    (check-within (candidate (list 2 0 1) 1) 1 0.001)
    (check-within (candidate (list 2 0 1) 2) 2 0.001)
    (check-within (candidate (list 2 0 1) 3) 2 0.001)
    (check-within (candidate (list 2 1 0) 0) -1 0.001)
    (check-within (candidate (list 2 1 0) 1) 2 0.001)
    (check-within (candidate (list 2 1 0) 2) 2 0.001)
    (check-within (candidate (list 2 1 0) 3) 2 0.001)
    (check-within (candidate (list 0 1 2 3) 0) -1 0.001)
    (check-within (candidate (list 0 1 2 3) 1) 3 0.001)
    (check-within (candidate (list 0 1 2 3) 2) 3 0.001)
    (check-within (candidate (list 0 1 2 3) 3) 3 0.001)
    (check-within (candidate (list 0 1 2 3) 4) 3 0.001)
    (check-within (candidate (list 0 1 3 2) 0) -1 0.001)
    (check-within (candidate (list 0 1 3 2) 1) 2 0.001)
    (check-within (candidate (list 0 1 3 2) 2) 3 0.001)
    (check-within (candidate (list 0 1 3 2) 3) 3 0.001)
    (check-within (candidate (list 0 1 3 2) 4) 3 0.001)
    (check-within (candidate (list 0 2 1 3) 0) -1 0.001)
    (check-within (candidate (list 0 2 1 3) 1) -1 0.001)
    (check-within (candidate (list 0 2 1 3) 2) 3 0.001)
    (check-within (candidate (list 0 2 1 3) 3) 3 0.001)
    (check-within (candidate (list 0 2 1 3) 4) 3 0.001)
    (check-within (candidate (list 0 2 3 1) 0) -1 0.001)
    (check-within (candidate (list 0 2 3 1) 1) 1 0.001)
    (check-within (candidate (list 0 2 3 1) 2) 3 0.001)
    (check-within (candidate (list 0 2 3 1) 3) 3 0.001)
    (check-within (candidate (list 0 2 3 1) 4) 3 0.001)
    (check-within (candidate (list 0 3 1 2) 0) -1 0.001)
    (check-within (candidate (list 0 3 1 2) 1) 2 0.001)
    (check-within (candidate (list 0 3 1 2) 2) 2 0.001)
    (check-within (candidate (list 0 3 1 2) 3) 3 0.001)
    (check-within (candidate (list 0 3 1 2) 4) 3 0.001)
    (check-within (candidate (list 0 3 2 1) 0) -1 0.001)
    (check-within (candidate (list 0 3 2 1) 1) 1 0.001)
    (check-within (candidate (list 0 3 2 1) 2) 2 0.001)
    (check-within (candidate (list 0 3 2 1) 3) 3 0.001)
    (check-within (candidate (list 0 3 2 1) 4) 3 0.001)
    (check-within (candidate (list 1 0 2 3) 0) -1 0.001)
    (check-within (candidate (list 1 0 2 3) 1) 2 0.001)
    (check-within (candidate (list 1 0 2 3) 2) 3 0.001)
    (check-within (candidate (list 1 0 2 3) 3) 3 0.001)
    (check-within (candidate (list 1 0 2 3) 4) 3 0.001)
    (check-within (candidate (list 1 0 3 2) 0) -1 0.001)
    (check-within (candidate (list 1 0 3 2) 1) 1 0.001)
    (check-within (candidate (list 1 0 3 2) 2) 2 0.001)
    (check-within (candidate (list 1 0 3 2) 3) 3 0.001)
    (check-within (candidate (list 1 0 3 2) 4) 3 0.001)
    (check-within (candidate (list 1 2 0 3) 0) -1 0.001)
    (check-within (candidate (list 1 2 0 3) 1) 2 0.001)
    (check-within (candidate (list 1 2 0 3) 2) 2 0.001)
    (check-within (candidate (list 1 2 0 3) 3) 3 0.001)
    (check-within (candidate (list 1 2 0 3) 4) 3 0.001)
    (check-within (candidate (list 1 2 3 0) 0) -1 0.001)
    (check-within (candidate (list 1 2 3 0) 1) 1 0.001)
    (check-within (candidate (list 1 2 3 0) 2) 2 0.001)
    (check-within (candidate (list 1 2 3 0) 3) 3 0.001)
    (check-within (candidate (list 1 2 3 0) 4) 3 0.001)
    (check-within (candidate (list 1 3 0 2) 0) -1 0.001)
    (check-within (candidate (list 1 3 0 2) 1) 1 0.001)
    (check-within (candidate (list 1 3 0 2) 2) 2 0.001)
    (check-within (candidate (list 1 3 0 2) 3) 3 0.001)
    (check-within (candidate (list 1 3 0 2) 4) 3 0.001)
    (check-within (candidate (list 1 3 2 0) 0) -1 0.001)
    (check-within (candidate (list 1 3 2 0) 1) 1 0.001)
    (check-within (candidate (list 1 3 2 0) 2) 3 0.001)
    (check-within (candidate (list 1 3 2 0) 3) 3 0.001)
    (check-within (candidate (list 1 3 2 0) 4) 3 0.001)
    (check-within (candidate (list 2 0 1 3) 0) -1 0.001)
    (check-within (candidate (list 2 0 1 3) 1) 1 0.001)
    (check-within (candidate (list 2 0 1 3) 2) 3 0.001)
    (check-within (candidate (list 2 0 1 3) 3) 3 0.001)
    (check-within (candidate (list 2 0 1 3) 4) 3 0.001)
    (check-within (candidate (list 2 0 3 1) 0) -1 0.001)
    (check-within (candidate (list 2 0 3 1) 1) 1 0.001)
))

(test-humaneval)