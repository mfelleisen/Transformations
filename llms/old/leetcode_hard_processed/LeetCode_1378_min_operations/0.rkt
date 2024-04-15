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
(define (min_operations target arr)
  ;; Creates a hash table from target list with value as index for efficient lookup and order comparison
  (define target-map (for/hash ([num (in-list target)] [i (in-naturals)])
                        (values num i)))
  ;; Helper function to find the position to insert the element in the "lis" to maintain increasing order
  (define (binary-search lis value)
    (let loop ([low 0] [high (length lis)])
      (if (= low high)
          low
          (let ([mid (quotient (+ low high) 2)])
            (if (< (list-ref lis mid) value)
                (loop (add1 mid) high)
                (loop low mid))))))
  ;; Constructs a Longest Increasing Subsequence (LIS) based only on the elements found in both target and arr, respecting the order in target
  (define lis
    (foldl (lambda (num lis)
             (let ([mapped-index (hash-ref target-map num #f)])
               (if mapped-index
                   (let ([pos (binary-search lis mapped-index)])
                     (if (= pos (length lis))
                         (append lis (list mapped-index))
                         (append (take lis pos) (list mapped-index) (drop lis (add1 pos)))))
                   lis)))
           '()
           arr))
  ;; The minimum operations needed are the length of target minus the length of the constructed LIS
  (- (length target) (length lis)))

;; Examples provided in the description:
(display (min_operations '(5 1 3) '(9 4 2 3 4))) ; Output: 2
(display (min_operations '(6 4 8 1 3 2) '(4 7 6 2 3 8 6 1))) ; Output: 3
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