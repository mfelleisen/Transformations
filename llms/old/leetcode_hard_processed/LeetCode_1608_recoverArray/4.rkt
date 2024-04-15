#lang racket

;; This function recovers an unknown list given the list of all subset sums.
;; You are given an integer n representing the length of an unknown list that you are trying to recover. You are also given a list sums containing the values of all 2n subset sums of the unknown list (in no particular order).
;; Return the list ans of length n representing the unknown list. If multiple answers exist, return any of them.
;; An list sub is a subset of a list arr if sub can be obtained from arr by deleting some (possibly zero or all) elements of arr. The sum of the elements in sub is one possible subset sum of arr. The sum of an empty list is considered to be 0.
;; Note: Test cases are generated such that there will always be at least one correct answer.
;; Example 1:
;; Input: n = 3, sums = [-3,-2,-1,0,0,1,2,3]
;; Output: [1,2,-3]
;; Explanation: [1,2,-3] is able to achieve the given subset sums:
;; - []: sum is 0
;; - [1]: sum is 1
;; - [2]: sum is 2
;; - [1,2]: sum is 3
;; - [-3]: sum is -3
;; - [1,-3]: sum is -2
;; - [2,-3]: sum is -1
;; - [1,2,-3]: sum is 0
;; Note that any permutation of [1,2,-3] and also any permutation of [-1,-2,3] will also be accepted.
;; Example 2:
;; Input: n = 2, sums = [0,0,0,0]
;; Output: [0,0]
;; Explanation: The only correct answer is [0,0].
;; Example 3:
;; Input: n = 4, sums = [0,0,5,5,4,-1,4,9,9,-1,4,3,4,8,3,8]
;; Output: [0,-1,4,5]
;; Explanation: [0,-1,4,5] is able to achieve the given subset sums.
;; Constraints:
;; 1 <= n <= 15
;; sums.length == 2n
;; -104 <= sums[i] <= 104
(define (recoverArray n sums)
  ;; Helper function to decrease the count of a value in a counter (hash table)
  (define (decrease-count counter value)
    (hash-update counter value sub1 0))
  
  ;; Converts a list of numbers into a counter (hash table) of number frequencies.
  (define (list->counter lst)
    (foldl (lambda (num acc) (hash-update acc num add1 0)) (hash) lst))
  
  ;; Main recursive function to recover the array
  (define (helper n sums acc)
    (if (= n 0)
        acc
        (let* ((counter (list->counter sums)) ; Convert the sums list to a counter
               (num (car (hash-keys counter))) ; Take the most common number (first key in the hash table)
               (new-sums (map (lambda (prevNum) (- prevNum num)) acc))) ; Calculate new sums based on the current element
          ;; Decrease the count of the current number and its related sums
          (for-each (lambda (s) (decrease-count counter s)) (cons num new-sums))
          ;; Recursively call the helper with updated parameters
          (helper (- n 1) (hash-keys counter) (cons num acc)))))

  ;; Call the helper function with initial parameters
  (helper n sums '()))

;; Test the function
(recoverArray 3 '(-3 -2 -1 0 0 1 2 3)) ; Output: [1 2 -3] (or any permutation)
(recoverArray 2 '(0 0 0 0)) ; Output: [0 0]
(recoverArray 4 '(0 0 5 5 4 -1 4 9 9 -1 4 3 4 8 3 8)) ; Output: [0 -1 4 5] (or any permutation)
(require rackunit)

(define (test-humaneval) 

  (let (( candidate recoverArray))
    (check-within (candidate 2 (list 0 0 0)) (list 0 0) 0.001)
    (check-within (candidate 3 (list 0 0 0 0 0)) (list 0 0 0) 0.001)
    (check-within (candidate 1 (list 0)) (list 0) 0.001)
    (check-within (candidate 2 (list 0 0 0 0)) (list 0 0) 0.001)
    (check-within (candidate 3 (list 0 0 0 0 0 0)) (list 0 0 0) 0.001)
    (check-within (candidate 3 (list 1 1 1 1 1 1 1 1)) (list 1 1 1) 0.001)
))

(test-humaneval)