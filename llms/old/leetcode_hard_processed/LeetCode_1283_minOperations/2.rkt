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
  ;; Creating a hash map (pos) to map each element in 'target' to its index for quick lookup.
  (define pos (for/hash ([i (in-range (length target))]
                         [val (in-list target)])
                (values val i)))
  ;; Using dynamic programming list (dp) to keep track of the longest subsequence found.
  (define dp '())
  ;; Iterating over 'arr' to check if the current element is in 'target'.
  (for ([a (in-list arr)])
    (when (hash-has-key? pos a)
      ;; If 'a' is in 'target', find the correct position in 'dp' where it should be inserted.
      (let* ([idx (bisect-left dp (hash-ref pos a))]
             [len (length dp)])
        (if (= idx len)
            ;; If the index is equal to the length of 'dp', append the position of 'a'.
            (set! dp (append dp (list (hash-ref pos a))))
            ;; Otherwise, replace the element at 'idx' in 'dp' with the position of 'a'.
            (set! dp (update-list dp idx (hash-ref pos a)))))))
  ;; The minimum number of operations is the difference between the length of 'target' and 'dp'.
  (- (length target) (length dp)))

;; Helper function to find the leftmost insertion point for a value in a sorted list.
(define (bisect-left lst val)
  (let loop ([low 0] [high (length lst)])
    (if (< low high)
        (let* ([mid (quotient (+ low high) 2)]
               [mid-val (list-ref lst mid)])
          (if (>= mid-val val)
              (loop low mid)
              (loop (+ mid 1) high)))
        low)))

;; Helper function to update an element at a specific index in a list.
(define (update-list lst idx new-val)
  (append (take lst idx)
          (list new-val)
          (drop lst (+ idx 1))))

;; Example usage:
(minOperations '(5 1 3) '(9 4 2 3 4))  ; Output: 2
(minOperations '(6 4 8 1 3 2) '(4 7 6 2 3 8 6 1))  ; Output: 3
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