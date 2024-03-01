#lang racket

;; You are given a list target that consists of distinct integers and another integer list arr that can have duplicates.
;; In one operation, you can insert any integer at any position in arr. For example, if arr = [1,4,1,2], you can add 3 in the middle and make it [1,4,3,1,2]. Note that you can insert the integer at the very beginning or end of the list.
;; Return the minimum number of operations needed to make target a subsequence of arr.
;; A subsequence of a list is a new list generated from the original list by deleting some elements (possibly none) without changing the remaining elements' relative order. For example, [2,7,4] is a subsequence of [4,2,3,7,2,1,4] (the underlined elements), while [2,4,2] is not.
;; Example 1:
;; Input: target = [5,1,3], arr = [9,4,2,3,4]
;; Output: 2
;; Explanation: You can add 5 and 1 in such a way that makes arr = [5,9,4,1,2,3,4], then target will be a subsequence of arr.
;; Example 2:
;; Input: target = [6,4,8,1,3,2], arr = [4,7,6,2,3,8,6,1]
;; Output: 3
;; Constraints:
;; 1 <= target.length, arr.length <= 105
;; 1 <= target[i], arr[i] <= 109
;; target contains no duplicates.
(define (minOperations target arr)
  ;; Create a hash map to store the positions of elements in the target list
  (define pos (for/hash ([i (in-range (length target))])
                (values (list-ref target i) i)))
  
  ;; Initialize an empty list to represent the dynamic programming table
  (define dp '())
  
  ;; Iterate through each element in arr
  (for ([a (in-list arr)])
    ;; Check if the element is in the target list by looking it up in the pos hash map
    (when (hash-has-key? pos a)
      ;; Use bisect-left, implemented as a helper function, to find the insertion point in dp
      (define idx (bisect-left dp (hash-ref pos a)))
      ;; If the index is equal to the length of dp, append the position to dp
      (if (= idx (length dp))
          (set! dp (append dp (list (hash-ref pos a))))
          ;; Otherwise, replace the element at idx in dp with the new position
          (set! dp (update-list dp idx (hash-ref pos a))))))
  
  ;; Subtract the length of the longest increasing subsequence (dp) from the length of the target list
  (- (length target) (length dp)))

;; Helper function to perform binary search (bisect left) on a list
(define (bisect-left lst x)
  (let loop ([low 0] [high (length lst)])
    (if (= low high)
        low
        (let ([mid (+ low (quotient (- high low) 2))])
          (if (< (list-ref lst mid) x)
              (loop (add1 mid) high)
              (loop low mid))))))

;; Helper function to update an element at a specific index in a list
(define (update-list lst idx val)
  (for/list ([i (in-range (length lst))])
    (if (= i idx) val (list-ref lst i))))
(require rackunit)

(define (test-humaneval) 

  (let (( candidate minOperations))
    (check-within (candidate (list 1 2 3 4 5) (list 5 4 3 2 1)) 4 0.001)
    (check-within (candidate (list 1 2 3 4 5 6 7 8 9 10) (list 10 9 8 7 6 5 4 3 2 1)) 9 0.001)
    (check-within (candidate (list 1 2 3) (list 1 2 3)) 0 0.001)
    (check-within (candidate (list 100 200 300 400 500) (list 500 400 300 200 100)) 4 0.001)
    (check-within (candidate (list 5 1 3) (list 9 4 2 3 4)) 2 0.001)
    (check-within (candidate (list 1 2 3) (list 2 3 1)) 1 0.001)
    (check-within (candidate (list 1 2 3 4 5) (list 3 4 5 1 2)) 2 0.001)
    (check-within (candidate (list 1 2 3 4 5) (list 2 3 4 5 1)) 1 0.001)
    (check-within (candidate (list 1 2 3 4 5 6 7 8 9 10) (list 2 4 6 8 10 1 3 5 7 9)) 5 0.001)
    (check-within (candidate (list 1 2 3) (list 3 2 1)) 2 0.001)
    (check-within (candidate (list 6 4 8 1 3 2) (list 4 7 6 2 3 8 6 1)) 3 0.001)
    (check-within (candidate (list 1 2 3 4 5 6 7 8 9 10) (list 5 6 7 8 9 10 4 3 2 1)) 4 0.001)
))

(test-humaneval)