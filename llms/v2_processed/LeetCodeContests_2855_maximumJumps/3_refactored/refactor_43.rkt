#lang racket

;; Function to calculate the maximum number of jumps to reach the last index of the list
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

  ;; Define a helper function to check if a jump from i to j is valid
  (define (valid-jump? i j)
    (<= (abs (- (list-ref nums j) (list-ref nums i))) target))

  ;; Define a recursive function to calculate the maximum jumps from a given index
  (define (max-jumps-from i)
    (cond
      [(= i (- n 1)) 0]  ; If we are at the last index, no more jumps needed
      [else
       (define valid-jumps
         (for/list ([j (in-range (+ i 1) n)]
                    #:when (valid-jump? i j))
           (max-jumps-from j)))
       (if (empty? valid-jumps)
           -inf.0  ; If no valid jumps, return -inf
           (add1 (apply max valid-jumps)))]))

  ;; Calculate the maximum jumps from index 0
  (define result (max-jumps-from 0))
  (if (= result -inf.0)
      -1
      result))

;; Example usage:
(maximumJumps '(1 3 6 4 1 2) 2)  ; Output: 3
(maximumJumps '(1 3 6 4 1 2) 3)  ; Output: 5
(maximumJumps '(1 3 6 4 1 2) 0)  ; Output: -1

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