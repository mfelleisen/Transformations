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
  ;; Create a hash table to store the positions of each element in the target list for quick lookup.
  (define pos (for/hash ([i (in-range (length target))])
                (values (list-ref target i) i)))
  
  ;; Initialize an empty list to act as a dynamic programming (DP) table.
  (define dp '())
  
  ;; Iterate over each element in arr.
  (for ([a arr])
    (when (hash-has-key? pos a) ;; Check if the current element is in target.
      ;; Calculate the insertion index in dp where pos[a] should be placed to maintain increasing order.
      (define idx (for/fold ([i 0]) ([dp-val (in-list dp)] #:break (> dp-val (hash-ref pos a)))
                    (if (> dp-val (hash-ref pos a)) i (add1 i))))
      ;; If idx is equal to the length of dp, it means pos[a] is larger than all elements in dp, so append it.
      ;; Otherwise, replace the element at idx with pos[a] to maintain the smallest increasing subsequence.
      (set! dp (if (= idx (length dp))
                   (append dp (list (hash-ref pos a)))
                   (append (take dp idx) (list (hash-ref pos a)) (drop dp (add1 idx)))))))
  
  ;; The length of the longest increasing subsequence in target that matches the order in arr is len(dp).
  ;; Subtract this from the length of target to get the minimum number of operations needed.
  (- (length target) (length dp)))

;; Example usage
(minOperations '(5 1 3) '(9 4 2 3 4)) ;; Output: 2
(minOperations '(6 4 8 1 3 2) '(4 7 6 2 3 8 6 1)) ;; Output: 3
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