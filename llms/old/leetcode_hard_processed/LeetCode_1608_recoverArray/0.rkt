#lang racket

(require racket/list)

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
  ;; Define a helper function to subtract one list from another
  (define (list-subtract l1 l2)
    (foldl (lambda (item acc)
             (let ((pos (member item acc)))
               (if pos
                   (remove item acc)
                   acc)))
           l1
           l2))
  ;; Define a helper function to count elements in a list
  (define (count-elements lst)
    (foldl (lambda (item acc)
             (dict-update acc item (lambda (v) (add1 v)) 0))
           (make-immutable-hash)
           lst))
  ;; Initialize an empty list to store the answer
  (define ans '())
  ;; Loop n times to find each element of the unknown array
  (for ([_ (in-range n)])
    (define counter (count-elements sums))
    ;; Find the most common element in sums (the first element in the sorted list of unique elements)
    (define num (first (sort (hash-keys counter) > #:key (lambda (key) (hash-ref counter key)))))
    ;; Subtract the set of {num} + {previous numbers - num} from the counter
    (set! sums (list-subtract sums (cons num (map (lambda (prevNum) (- prevNum num)) ans))))
    ;; Append num to the answer list
    (set! ans (cons num ans)))
  ;; Return the reversed answer list to maintain order
  (reverse ans))

;; Example usage:
(recoverArray 3 '(-3 -2 -1 0 0 1 2 3))
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