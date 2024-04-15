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
  ;; Auxiliary function to perform merge sort and simultaneously count smaller elements.
  (define (merge-sort indices)
    (if (<= (length indices) 1)
        indices
        (let* ((mid (quotient (length indices) 2))
               (left (merge-sort (take indices mid)))
               (right (merge-sort (drop indices mid))))
          (merge left right))))
  
  ;; Merge step of the merge sort algorithm, modified to count smaller elements.
  (define (merge left right)
    (let loop ((left left) (right right) (merged '()) (count 0))
      (cond
        ;; If left is non-empty and its first element is less or equal to the first element of right,
        ;; increment the count for that index and continue merging.
        ((and (not (null? left)) (not (null? right)) (<= (list-ref nums (car left)) (list-ref nums (car right))))
         (begin
           (vector-set! counts (car left) (+ (vector-ref counts (car left)) count))
           (loop (cdr left) right (append merged (list (car left))) count)))
        ;; If the first element of right is smaller, increase the count of smaller elements seen and continue merging.
        ((and (not (null? left)) (not (null? right)))
         (loop left (cdr right) (append merged (list (car right))) (+ count (length left))))
        ;; If only left or right is non-empty, append the rest to merged and add count to remaining left elements.
        (left
         (begin
           (for-each (lambda (i) (vector-set! counts i (+ (vector-ref counts i) count))) left)
           (append merged left right)))
        ;; If only right is non-empty, append it to merged.
        (right
         (append merged right))
        ;; If both are empty, just return merged.
        (else merged))))
  
  ;; Initialize the counts vector with zeros.
  (define counts (make-vector (length nums) 0))
  ;; Perform the modified merge sort and return the counts as a list.
  (merge-sort (build-list (length nums) values))
  (vector->list counts))

;; Example usage:
(countSmaller (list 5 2 6 1)) ;; Expected output: '(2 1 1 0)
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