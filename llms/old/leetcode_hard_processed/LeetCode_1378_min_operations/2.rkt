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
  ;; Create an index map from target values to their indices
  (define index-map (make-hash (map cons target (range (length target)))))
  ;; Function to find the position to insert a value in the list maintaining sorted order
  (define (find-pos lis val)
    (let loop ([i 0])
      (cond
        [(>= i (length lis)) i]
        [(>= (list-ref lis i) val) i]
        [else (loop (+ i 1))])))
  ;; Build the longest increasing subsequence (LIS) using the index map for elements found in arr
  (define (build-lis arr)
    (foldl (lambda (num lis)
             (let ((idx (hash-ref index-map num #f)))
               (if idx
                   (let ((pos (find-pos lis idx)))
                     (if (= pos (length lis))
                         (append lis (list idx))
                         (append (take lis pos) (list idx) (drop lis (+ pos 1)))))
                   lis)))
           '()
           arr))
  ;; Calculate the minimum number of operations as the difference between target length and LIS length
  (- (length target) (length (build-lis arr))))

;; Examples
(min_operations '(5 1 3) '(9 4 2 3 4)) ; Output: 2
(min_operations '(6 4 8 1 3 2) '(4 7 6 2 3 8 6 1)) ; Output: 3
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