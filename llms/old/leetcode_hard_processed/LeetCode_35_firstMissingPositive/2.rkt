#lang racket

;; Given an unsorted integer list nums, return the smallest missing positive integer.
;; You must implement an algorithm that runs in O(n) time and uses O(1) auxiliary space.
;; Example 1:
;; Input: nums = [1,2,0]
;; Output: 3
;; Explanation: The numbers in the range [1,2] are all in the list.
;; Example 2:
;; Input: nums = [3,4,-1,1]
;; Output: 2
;; Explanation: 1 is in the list but 2 is missing.
;; Example 3:
;; Input: nums = [7,8,9,11,12]
;; Output: 1
;; Explanation: The smallest positive integer 1 is missing.
;; Constraints:
;; 1 <= nums.length <= 105
;; -231 <= nums[i] <= 231 - 1
(define (firstMissingPositive nums)
  ;; Calculate the length of the list
  (define n (length nums))
  (define (swap lst i j)
    ;; Swap elements in a list at indices i and j
    (let* ((elem-i (list-ref lst i))
           (elem-j (list-ref lst j))
           (lst (list-set lst i elem-j))
           (lst (list-set lst j elem-i)))
      lst))
  (define (fix-positions lst n)
    ;; Position each number in its 'correct' position (i.e., value 1 at index 0, value 2 at index 1, etc.)
    (let loop ((i 0) (lst lst))
      (if (< i n)
          (let* ((val (list-ref lst i))
                 (correct-pos (- val 1)))
            (if (and (<= 1 val) (<= val n) (not (= val (list-ref lst correct-pos))))
                (loop i (swap lst i correct-pos)) ; Swap until current element is in its correct position
                (loop (+ i 1) lst)))
          lst)))
  (define (find-missing lst n)
    ;; Find the first missing positive number
    (let loop ((i 0))
      (if (< i n)
          (if (not (= (+ i 1) (list-ref lst i)))
              (+ i 1)
              (loop (+ i 1)))
          (+ n 1))))
  ;; Perform the operations
  (find-missing (fix-positions nums n) n))

;; Example usage:
(firstMissingPositive '(1 2 0))
(firstMissingPositive '(3 4 -1 1))
(firstMissingPositive '(7 8 9 11 12))
(require rackunit)

(define (test-humaneval) 

  (let (( candidate firstMissingPositive))
    (check-within (candidate (list 3 4 -1 1)) 2 0.001)
    (check-within (candidate (list 5 4 3 2 1)) 6 0.001)
    (check-within (candidate (list 1 1 1 1)) 2 0.001)
    (check-within (candidate (list 1)) 2 0.001)
    (check-within (candidate (list 2)) 1 0.001)
    (check-within (candidate (list 1 2 5 7 9 11)) 3 0.001)
    (check-within (candidate (list 7 8 9 11 12)) 1 0.001)
    (check-within (candidate (list 1 2 3 4 5)) 6 0.001)
    (check-within (candidate (list 1 2 3 5 5 6 6 7 8 9 10)) 4 0.001)
    (check-within (candidate (list 0 2 1 4 3 6 5 -2 7 9 8 11 10 12 13 14 15 16 17 18)) 19 0.001)
    (check-within (candidate (list 3 4 -1 1)) 2 0.001)
    (check-within (candidate (list 1 1 1 1 1 2 2 2 2 2 3 3 3 3 3)) 4 0.001)
    (check-within (candidate (list 1 1 1 1)) 2 0.001)
    (check-within (candidate (list -1 -2 -3 -4)) 1 0.001)
    (check-within (candidate (list 7 8 9 11 12)) 1 0.001)
    (check-within (candidate (list -1)) 1 0.001)
    (check-within (candidate (list -2 -3 -5 -1 -7 -10 -11 -12 -13 -15 -16 -17 -18 -19 -20 2 1 6 7 3)) 4 0.001)
    (check-within (candidate (list 0)) 1 0.001)
    (check-within (candidate (list 1 2 3 4 5 7 8 9 10)) 6 0.001)
    (check-within (candidate (list 1 2 0)) 3 0.001)
    (check-within (candidate (list 2 3)) 1 0.001)
    (check-within (candidate (list 2 2 2 2 3 3 3 3)) 1 0.001)
    (check-within (candidate (list 1 1 1 1 1 2 2 2 2 2 3 3 3 3 3)) 4 0.001)
    (check-within (candidate (list 1 2 0)) 3 0.001)
    (check-within (candidate (list 0)) 1 0.001)
    (check-within (candidate (list 100 101 102 103 104)) 1 0.001)
    (check-within (candidate (list 2 5 6 8 11 15)) 1 0.001)
    (check-within (candidate (list 2)) 1 0.001)
    (check-within (candidate (list 0 0 0 0)) 1 0.001)
    (check-within (candidate (list 1 1 2 2 3 3)) 4 0.001)
    (check-within (candidate (list 1 2 3 5 5 6 6 7 8 9 10)) 4 0.001)
    (check-within (candidate (list -1)) 1 0.001)
    (check-within (candidate (list 1 2 5 7 9 11)) 3 0.001)
    (check-within (candidate (list 5 4 3 2 1)) 6 0.001)
))

(test-humaneval)