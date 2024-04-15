#lang racket

;; Given an integer list nums, return an integer list counts where counts[i] is the number of smaller elements to the right of nums[i].
;; Given an integer list nums, return an integer list counts where counts[i] is the number of smaller elements to the right of nums[i].
;; Example 1:
;; Input: nums = [5,2,6,1]
;; Output: [2,1,1,0]
;; Explanation:
;; To the right of 5 there are 2 smaller elements (2 and 1).
;; To the right of 2 there is only 1 smaller element (1).
;; To the right of 6 there is 1 smaller element (1).
;; To the right of 1 there is 0 smaller element.
;; Example 2:
;; Input: nums = [-1]
;; Output: [0]
;; Example 3:
;; Input: nums = [-1,-1]
;; Output: [0,0]
;; Constraints:
;; 1 <= nums.length <= 105
;; -104 <= nums[i] <= 104
(define (countSmaller nums)
  ;; Merge sort function that sorts indices based on nums values, while counting smaller elements to the right.
  (define (merge-sort indices)
    (if (<= (length indices) 1)
        indices
        (let* ((mid (quotient (length indices) 2))
               (left (merge-sort (take indices mid)))
               (right (merge-sort (drop indices mid))))
          (merge left right))))
  
  ;; Merge function that merges two sorted lists of indices, while updating counts for the left elements.
  (define (merge left right)
    (let loop ((left left) (right right) (merged '()) (count 0))
      (cond
       ((null? left) (append merged right)) ; If left is empty, append the rest of right
       ((null? right) ; If right is empty, update counts for the rest of left and append them
        (begin
          (for-each (lambda (i) (vector-set! counts i (+ (vector-ref counts i) count))) left)
          (append merged left)))
       ((<= (list-ref nums (car left)) (list-ref nums (car right))) ; Compare current elements of left and right
        (begin
          (vector-set! counts (car left) (+ (vector-ref counts (car left)) count)) ; Update count for left element
          (loop (cdr left) right (append merged (list (car left))) count))) ; Move to next left element
       (else
        (loop left (cdr right) (append merged (list (car right))) (+ count (length left))))))) ; Move to next right element, increase count
  
  ;; Initialize counts vector with 0s
  (define counts (make-vector (length nums) 0))
  ;; Call merge-sort with a list of indices
  (merge-sort (build-list (length nums) values))
  ;; Convert counts vector to a list and return
  (vector->list counts))
(require rackunit)

(define (test-humaneval) 

  (let (( candidate countSmaller))
    (check-within (candidate (list 2 1 3 1 2)) (list 2 0 2 0 0) 0.001)
    (check-within (candidate (list 2 2 2 2 2 2)) (list 0 0 0 0 0 0) 0.001)
    (check-within (candidate (list 1 2 3 2 1)) (list 0 1 2 1 0) 0.001)
    (check-within (candidate (list -1)) (list 0) 0.001)
    (check-within (candidate (list -1 -1)) (list 0 0) 0.001)
    (check-within (candidate (list 1)) (list 0) 0.001)
    (check-within (candidate (list 1 2 3 4 5)) (list 0 0 0 0 0) 0.001)
    (check-within (candidate (list 2 2 2 2 2)) (list 0 0 0 0 0) 0.001)
))

(test-humaneval)