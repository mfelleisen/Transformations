#lang racket

;; Given two lists target and arr, computes the minimum number of operations needed to make target a subsequence of arr.
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
(define (min_operations target arr)
  ;; Create a hash map (index_map) from target elements to their indices for quick lookup.
  (define index_map (for/hash ([num (in-list target)] [i (in-naturals)])
                      (values num i)))
  
  ;; Function to perform binary search to find the correct position to insert or replace in the lis.
  ;; It returns the index where the target should be placed.
  (define (binary-search lst value)
    (let loop ([low 0] [high (length lst)])
      (if (= low high)
          low
          (let ([mid (quotient (+ low high) 2)])
            (if (< (list-ref lst mid) value)
                (loop (add1 mid) high)
                (loop low mid))))))
  
  ;; Initialize an empty list to build the Longest Increasing Subsequence (LIS) based on indices in target.
  (define lis '())
  
  ;; Iterate over each number in arr.
  (for ([num (in-list arr)])
    ;; Check if the current number is in target.
    (when (hash-has-key? index_map num)
      ;; Find the position in lis where this number's index in target should be placed.
      (let ([pos (binary-search lis (hash-ref index_map num))])
        (if (= pos (length lis))
            ;; If pos is equal to the length of lis, append the index.
            (set! lis (append lis (list (hash-ref index_map num))))
            ;; Otherwise, replace the element at pos with the new index.
            (set! lis (update-list lis pos (hash-ref index_map num)))))))
  
  ;; The minimum operations is the difference between the length of target and the length of LIS.
  (- (length target) (length lis)))

;; Helper function to update an element at a specific position in a list.
(define (update-list lst pos new-val)
  (for/list ([i (in-range (length lst))])
    (if (= i pos) new-val (list-ref lst i))))

;; Example usage:
;; (min_operations [5 1 3] [9 4 2 3 4]) ; Output: 2
;; (min_operations [6 4 8 1 3 2] [4 7 6 2 3 8 6 1]) ; Output: 3
(require rackunit)

(define (test-humaneval) 

  (let (( candidate min_operations))
    (check-within (candidate (list 1) (list 1)) 0 0.001)
    (check-within (candidate (list 1 2 3 4 5 6 7 8 9 10) (list 2 4 6 8 10)) 5 0.001)
    (check-within (candidate (list ) (list 1 2 3 4 5)) 0 0.001)
    (check-within (candidate (list 5 1 3) (list 9 4 2 3 4)) 2 0.001)
    (check-within (candidate (list 5 1 3) (list 9 4 2 3 4)) 2 0.001)
    (check-within (candidate (list 6 4 8 1 3 2) (list 4 7 6 2 3 8 6 1)) 3 0.001)
    (check-within (candidate (list ) (list )) 0 0.001)
    (check-within (candidate (list 6 4 8 1 3 2) (list 4 7 6 2 3 8 6 1)) 3 0.001)
    (check-within (candidate (list 1 2 3 4 5) (list 5 4 3 2 1 6 7 8 9 10)) 4 0.001)
    (check-within (candidate (list 5 1 3) (list 4 2 3 4)) 2 0.001)
    (check-within (candidate (list 1 2 3 4 5) (list 1 2 3 4 5)) 0 0.001)
    (check-within (candidate (list 6 4 8 1 3 2) (list 4 7 6 2 3 8 6 1)) 3 0.001)
    (check-within (candidate (list 5 1 3) (list 9 4 2 3 4)) 2 0.001)
    (check-within (candidate (list 5 1 3) (list 9 4 2 3 4 2 1 6 7 8 9 10)) 2 0.001)
    (check-within (candidate (list 1 3 5 7 9) (list 2 4 6 8 10)) 5 0.001)
    (check-within (candidate (list 1 2 3 4 5) (list 5 4 3 2 1)) 4 0.001)
    (check-within (candidate (list 1 2 3 4 5) (list )) 5 0.001)
    (check-within (candidate (list 5 1 3) (list 9 4 2 3 4)) 2 0.001)
    (check-within (candidate (list 1 2 3 4 5 6 7 8 9 10) (list 1 3 5 7 9)) 5 0.001)
    (check-within (candidate (list 5 1 3) (list 9 4 2 3 4)) 2 0.001)
    (check-within (candidate (list 2 2 2) (list 2 2)) 2 0.001)
    (check-within (candidate (list 1 2 3 4 5 6 7 8 9 10) (list 10 9 8 7 6 5 4 3 2 1)) 9 0.001)
    (check-within (candidate (list 1 2 3 4 5) (list 1 2 3 4)) 1 0.001)
    (check-within (candidate (list 1 2 3 4 5 6) (list 1 4 4 4 5 6)) 2 0.001)
    (check-within (candidate (list 6 4 8 1 3 2) (list 4 7 6 2 3 8 6 1)) 3 0.001)
))

(test-humaneval)