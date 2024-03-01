#lang racket

;; Function to count smaller elements to the right of each element in a list
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
  ;; Merge sort function that sorts indices based on their values in nums, also updates counts
  (define (merge-sort indices)
    (if (<= (length indices) 1)
        indices
        (let* ((mid (quotient (length indices) 2))
               (left (merge-sort (take indices mid)))
               (right (merge-sort (drop indices mid))))
          (merge left right))))
  
  ;; Merge function that merges two lists of indices, comparing their values in nums
  (define (merge left right)
    (let loop ((left left) (right right) (merged '()) (count 0))
      (cond
        ((and (null? left) (null? right)) (reverse merged))
        ((null? right) 
         (begin
           (set! counts (update-counts left count counts))
           (loop (cdr left) right (cons (car left) merged) count)))
        ((null? left) (loop left (cdr right) (cons (car right) merged) count))
        ((<= (list-ref nums (car left)) (list-ref nums (car right)))
         (begin
           (set! counts (update-counts (list (car left)) count counts))
           (loop (cdr left) right (cons (car left) merged) count)))
        (else
         (loop left (cdr right) (cons (car right) merged) (+ count (length left)))))))
  
  ;; Updates the counts list based on the number of elements to the right that are smaller
  (define (update-counts indices count counts)
    (foldl (lambda (idx acc)
             (list-set acc idx (+ (list-ref acc idx) count)))
           counts
           indices))
  
  ;; Initialize counts list with zeros
  (define counts (make-list (length nums) 0))
  ;; Sort the indices of nums, which indirectly sorts nums and updates counts
  (merge-sort (range (length nums)))
  ;; Return the counts list
  counts)

;; Example usage:
(countSmaller '(5 2 6 1))  ;; Output: '(2 1 1 0)
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