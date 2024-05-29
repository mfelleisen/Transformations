#lang racket

;; You are given a 0-indexed integer array nums and an integer k.
;; A subarray is called equal if all of its elements are equal. Note that the empty subarray is an equal subarray.
;; Return the length of the longest possible equal subarray after deleting at most k elements from nums.
;; A subarray is a contiguous, possibly empty sequence of elements within an array.
;; Example 1:
;; Input: nums = [1,3,2,3,1,3], k = 3
;; Output: 3
;; Explanation: It's optimal to delete the elements at index 2 and index 4.
;; After deleting them, nums becomes equal to [1, 3, 3, 3].
;; The longest equal subarray starts at i = 1 and ends at j = 3 with length equal to 3.
;; It can be proven that no longer equal subarrays can be created.
;; Example 2:
;; Input: nums = [1,1,2,2,1,1], k = 2
;; Output: 4
;; Explanation: It's optimal to delete the elements at index 2 and index 3.
;; After deleting them, nums becomes equal to [1, 1, 1, 1].
;; The array itself is an equal subarray, so the answer is 4.
;; It can be proven that no longer equal subarrays can be created.
;; Constraints:
;;  * 1 <= nums.length <= 105
;;  * 1 <= nums[i] <= nums.length
;;  * 0 <= k <= nums.length
(define (longestEqualSubarray nums k)
  ;; Define a helper function to update frequency counts
  (define (update-counts counts num increment)
    (hash-update counts num (λ (v) (+ v increment)) 0))
  
  ;; Initialize variables for tracking the sliding window
  (define-values (max-length left count) (values 0 0 (make-hash)))
  
  ;; Iterate through nums using a right pointer for the sliding window
  (for ([right (in-list (range (length nums)))])
    (define num-right (list-ref nums right))
    (update-counts count num-right 1)
    
    ;; Calculate the maximum frequency in the current window
    (define max-freq (apply max (hash-values count)))
    
    ;; Calculate the current window length
    (define window-length (+ 1 (- right left)))
    
    ;; Check if the window needs to be shrunk
    (when (> (- window-length max-freq) k)
      (define num-left (list-ref nums left))
      (update-counts count num-left -1)
      (set! left (+ left 1)))
    
    ;; Update the maximum length found
    (set! max-length (max max-length (- window-length (- window-length max-freq)))))
  
  ;; Return the maximum length of an equal subarray that can be achieved
  max-length)

(require rackunit)


(define (test-humaneval) 

  (let (( candidate longestEqualSubarray))
    (check-within (candidate (list 1 3 2 3 1 3) 3) 3 0.001)
    (check-within (candidate (list 1 1 2 2 1 1) 2) 4 0.001)
    (check-within (candidate (list 1) 0) 1 0.001)
    (check-within (candidate (list 1) 1) 1 0.001)
    (check-within (candidate (list 2 1) 1) 1 0.001)
    (check-within (candidate (list 2 2) 1) 2 0.001)
    (check-within (candidate (list 1 1) 0) 2 0.001)
    (check-within (candidate (list 2 1) 0) 1 0.001)
    (check-within (candidate (list 1 2) 1) 1 0.001)
    (check-within (candidate (list 2 2) 2) 2 0.001)
    (check-within (candidate (list 2 3 2) 1) 2 0.001)
    (check-within (candidate (list 3 2 2) 1) 2 0.001)
    (check-within (candidate (list 3 1 1) 2) 2 0.001)
    (check-within (candidate (list 1 2 3) 2) 1 0.001)
    (check-within (candidate (list 1 2 3) 3) 1 0.001)
    (check-within (candidate (list 2 3 1) 2) 1 0.001)
    (check-within (candidate (list 2 2 1) 1) 2 0.001)
    (check-within (candidate (list 1 3 2) 3) 1 0.001)
    (check-within (candidate (list 2 3 3) 3) 2 0.001)
    (check-within (candidate (list 3 2 3) 3) 2 0.001)
    (check-within (candidate (list 2 2 3) 0) 2 0.001)
    (check-within (candidate (list 2 2 2) 1) 3 0.001)
    (check-within (candidate (list 1 2 1) 0) 1 0.001)
    (check-within (candidate (list 3 3 3) 2) 3 0.001)
    (check-within (candidate (list 1 2 2) 1) 2 0.001)
    (check-within (candidate (list 1 1 3) 1) 2 0.001)
    (check-within (candidate (list 1 2 3) 1) 1 0.001)
    (check-within (candidate (list 1 2 2) 3) 2 0.001)
    (check-within (candidate (list 3 2 4 2) 1) 2 0.001)
    (check-within (candidate (list 3 4 2 1) 0) 1 0.001)
    (check-within (candidate (list 1 3 4 2) 0) 1 0.001)
    (check-within (candidate (list 2 4 2 2) 3) 3 0.001)
    (check-within (candidate (list 4 2 2 3) 1) 2 0.001)
    (check-within (candidate (list 4 1 3 2) 4) 1 0.001)
    (check-within (candidate (list 1 1 2 1) 4) 3 0.001)
    (check-within (candidate (list 2 4 1 3) 3) 1 0.001)
    (check-within (candidate (list 1 1 1 2) 3) 3 0.001)
    (check-within (candidate (list 4 2 1 4) 4) 2 0.001)
    (check-within (candidate (list 1 1 4 1) 4) 3 0.001)
    (check-within (candidate (list 2 1 4 1) 4) 2 0.001)
    (check-within (candidate (list 2 4 3 3) 2) 2 0.001)
    (check-within (candidate (list 1 2 2 4) 3) 2 0.001)
    (check-within (candidate (list 2 2 2 4) 3) 3 0.001)
    (check-within (candidate (list 2 3 3 1) 3) 2 0.001)
    (check-within (candidate (list 2 4 1 4) 1) 2 0.001)
    (check-within (candidate (list 3 4 1 4) 2) 2 0.001)
    (check-within (candidate (list 3 2 3 1) 3) 2 0.001)
    (check-within (candidate (list 3 1 1 3) 3) 2 0.001)
    (check-within (candidate (list 2 3 2 5 1) 0) 1 0.001)
    (check-within (candidate (list 5 3 3 1 3) 3) 3 0.001)
    (check-within (candidate (list 4 4 2 2 4) 1) 2 0.001)
    (check-within (candidate (list 5 4 2 3 3) 5) 2 0.001)
    (check-within (candidate (list 4 4 4 3 4) 1) 4 0.001)
    (check-within (candidate (list 1 5 5 5 3) 2) 3 0.001)
    (check-within (candidate (list 2 1 4 5 2) 1) 1 0.001)
    (check-within (candidate (list 5 1 5 2 3) 3) 2 0.001)
    (check-within (candidate (list 5 3 2 3 4) 1) 2 0.001)
    (check-within (candidate (list 3 2 1 4 1) 0) 1 0.001)
    (check-within (candidate (list 3 2 2 5 3) 5) 2 0.001)
    (check-within (candidate (list 2 2 4 4 2) 1) 2 0.001)
    (check-within (candidate (list 4 3 3 4 3) 2) 3 0.001)
    (check-within (candidate (list 1 5 4 3 4) 0) 1 0.001)
    (check-within (candidate (list 4 4 2 5 3) 0) 2 0.001)
    (check-within (candidate (list 3 3 5 2 3) 4) 3 0.001)
    (check-within (candidate (list 1 1 1 1 3) 5) 4 0.001)
    (check-within (candidate (list 1 3 2 5 1) 1) 1 0.001)
    (check-within (candidate (list 3 3 1 4 5) 2) 2 0.001)
    (check-within (candidate (list 1 2 5 5 4) 3) 2 0.001)
    (check-within (candidate (list 3 1 5 3 1 1) 0) 2 0.001)
    (check-within (candidate (list 6 4 1 5 5 3) 0) 2 0.001)
    (check-within (candidate (list 2 2 2 1 2 4) 4) 4 0.001)
    (check-within (candidate (list 1 1 2 2 6 2) 1) 3 0.001)
    (check-within (candidate (list 1 2 3 6 6 2) 4) 2 0.001)
    (check-within (candidate (list 2 4 5 1 4 1) 2) 2 0.001)
    (check-within (candidate (list 1 5 6 4 6 3) 3) 2 0.001)
    (check-within (candidate (list 5 4 2 4 1 3) 2) 2 0.001)
    (check-within (candidate (list 2 1 1 3 1 3) 2) 3 0.001)
    (check-within (candidate (list 2 4 6 6 6 4) 3) 3 0.001)
    (check-within (candidate (list 3 6 2 5 4 5) 1) 2 0.001)
    (check-within (candidate (list 1 1 5 6 1 4) 4) 3 0.001)
    (check-within (candidate (list 4 6 6 6 2 4) 3) 3 0.001)
    (check-within (candidate (list 2 5 5 5 6 4) 1) 3 0.001)
    (check-within (candidate (list 2 3 3 2 3 3) 2) 4 0.001)
    (check-within (candidate (list 5 2 5 3 3 2) 6) 2 0.001)
    (check-within (candidate (list 3 5 3 2 3 6) 6) 3 0.001)
    (check-within (candidate (list 4 3 2 5 4 2) 4) 2 0.001)
    (check-within (candidate (list 1 2 4 2 1 3) 3) 2 0.001)
    (check-within (candidate (list 3 1 3 4 1 6) 6) 2 0.001)
    (check-within (candidate (list 2 3 7 7 3 2 2) 7) 3 0.001)
    (check-within (candidate (list 5 6 7 7 4 4 2) 5) 2 0.001)
    (check-within (candidate (list 7 2 4 1 3 3 4) 4) 2 0.001)
    (check-within (candidate (list 5 3 1 7 5 5 7) 6) 3 0.001)
    (check-within (candidate (list 4 2 6 2 3 4 6) 6) 2 0.001)
    (check-within (candidate (list 2 7 6 7 3 7 4) 5) 3 0.001)
    (check-within (candidate (list 3 2 5 1 4 3 4) 4) 2 0.001)
    (check-within (candidate (list 2 3 3 7 2 5 1) 2) 2 0.001)
    (check-within (candidate (list 4 1 6 7 5 3 5) 1) 2 0.001)
    (check-within (candidate (list 3 1 4 1 5 5 6) 6) 2 0.001)
    (check-within (candidate (list 1 5 5 7 7 7 4) 0) 3 0.001)
    (check-within (candidate (list 5 7 4 4 1 6 7) 5) 2 0.001)
))

(test-humaneval)