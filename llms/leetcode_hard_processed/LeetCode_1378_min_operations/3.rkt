#lang racket

;; Function to find the minimum number of operations needed to make target a subsequence of arr
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
  ;; Create a hash map of target values to their indices for fast lookup
  (define index-map (make-hash (for/list ([num (in-list target)] [i (in-naturals)]) (cons num i))))
  ;; Initialize an empty list to keep track of the Longest Increasing Subsequence (LIS)
  (define lis '())
  ;; Function to find the position to insert an element in the LIS
  (define (find-position lst elem)
    (let loop ((lst lst) (index 0))
      (cond
       [(null? lst) index] ;; Base case: if the list is empty, return the current index
       [(>= (car lst) elem) index] ;; If the current element in lis is >= elem, return the index
       [else (loop (cdr lst) (add1 index))]))) ;; Otherwise, continue searching in the rest of the list
  ;; Iterate over each number in arr
  (for ([num (in-list arr)])
    ;; Check if the number is in the target using the hash map
    (when (hash-has-key? index-map num)
      ;; Find the appropriate position for this number in the LIS
      (let* ([pos (find-position lis (hash-ref index-map num))]
             ;; If the position is equal to the length of lis, append the number, else replace the element at pos
             [new-lis (if (= pos (length lis))
                          (append lis (list (hash-ref index-map num)))
                          (for/list ([i (in-range (length lis))])
                            (if (= i pos) (hash-ref index-map num) (list-ref lis i))))])
        (set! lis new-lis))))
  ;; The minimum number of operations is the difference between the length of target and the length of the LIS found
  (- (length target) (length lis)))

;; Example usage:
;; (min_operations [5 1 3] [9 4 2 3 4]) ;; Expected output: 2
;; (min_operations [6 4 8 1 3 2] [4 7 6 2 3 8 6 1]) ;; Expected output: 3
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