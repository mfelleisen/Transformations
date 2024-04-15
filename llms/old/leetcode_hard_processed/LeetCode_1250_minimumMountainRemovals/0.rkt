#lang racket

;; Function to compute the minimum number of elements to remove to make a list a mountain list.
;; You may recall that a list arr is a mountain list if and only if:
;; arr.length >= 3
;; There exists some index i (0-indexed) with 0 < i < arr.length - 1 such that:
;; arr[0] < arr[1] < ... < arr[i - 1] < arr[i]
;; arr[i] > arr[i + 1] > ... > arr[arr.length - 1]
;; Given an integer list nums​​​, return the minimum number of elements to remove to make nums​​​ a mountain list.
;; Example 1:
;; Input: nums = [1,3,1]
;; Output: 0
;; Explanation: The list itself is a mountain list so we do not need to remove any elements.
;; Example 2:
;; Input: nums = [2,1,1,5,6,2,3,1]
;; Output: 3
;; Explanation: One solution is to remove the elements at indices 0, 1, and 5, making the list nums = [1,5,6,3,1].
;; Constraints:
;; 3 <= nums.length <= 1000
;; 1 <= nums[i] <= 109
;; It is guaranteed that you can make a mountain list out of nums.
(define (minimumMountainRemovals nums)
  ;; Determine the length of the input list.
  (define n (length nums))
  ;; Initialize left and right lists to track increasing sequences from both ends.
  (define left (make-vector n 0))
  (define right (make-vector n 0))
  
  ;; Fill in the left and right lists.
  (for ([i (in-range 1 n)])
    (vector-set! left i (+ (vector-ref left (- i 1)) (if (> (list-ref nums i) (list-ref nums (- i 1))) 1 0)))
    (vector-set! right (- n i 1) (+ (vector-ref right (- n i)) (if (> (list-ref nums (- n i 1)) (list-ref nums (- n i))) 1 0))))
  
  ;; Initialize ans with the maximum possible value based on the problem constraints.
  (define ans (- n 3))
  
  ;; Iterate through the list to find the minimum number of removals required.
  (for ([i (in-range 1 (- n 1))])
    (set! ans (min ans (- n 1 (vector-ref left i) (vector-ref right i)))))
  
  ;; Return the minimum number of removals.
  ans)

;; Example Usage:
;; (minimumMountainRemovals '(1 3 1)) returns 0
;; (minimumMountainRemovals '(2 1 1 5 6 2 3 1)) returns 3
(require rackunit)

(define (test-humaneval) 

  (let (( candidate minimumMountainRemovals))
    (check-within (candidate (list 1 1 1 1 1 1 1 1 1 1)) 7 0.001)
    (check-within (candidate (list 1 5 6 3 1)) 0 0.001)
    (check-within (candidate (list 1 2 3 2 1)) 0 0.001)
    (check-within (candidate (list 1 3 1)) 0 0.001)
    (check-within (candidate (list 1 2 3 4 4 3 2 1)) 1 0.001)
    (check-within (candidate (list 2 1 1 5 6 2 3 1)) 3 0.001)
    (check-within (candidate (list 1 2 3 4 4 3 2 1 2 1)) 2 0.001)
    (check-within (candidate (list 1 2 3 4 5 6 7 8 9 8 7 6 5 4 3 2 1)) 0 0.001)
    (check-within (candidate (list 1 2 3 4 4 3 2 1 3)) 2 0.001)
    (check-within (candidate (list 3 2 1 4 3 2 4 3 2 1 4 3)) 4 0.001)
    (check-within (candidate (list 1 2 3 4 4 4 3 1)) 2 0.001)
))

(test-humaneval)