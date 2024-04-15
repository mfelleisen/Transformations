#lang racket

;; This function calculates the number of smaller elements to the right of each element in a list
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
  ;; merge-sort function that sorts indices based on their corresponding values in nums
  ;; while also updating the counts of smaller numbers to the right
  (define (merge-sort indices)
    (if (<= (length indices) 1)
        indices
        (let* ((mid (quotient (length indices) 2))
               (left (merge-sort (take indices mid)))
               (right (merge-sort (drop indices mid))))
          (merge left right))))
  
  ;; merge function that merges two sorted lists (of indices) into one
  ;; while updating counts based on the merge process
  (define (merge left right)
    (let loop ((left left) (right right) (merged '()) (count 0))
      (cond
       ((null? left) (append merged right)) ;; If left is empty, append right to merged
       ((null? right) ;; If right is empty, add count to each remaining element in left and append to merged
        (begin
          (for-each (lambda (i) (vector-set! counts i (+ (vector-ref counts i) count)))
                    left)
          (append merged left)))
       ((<= (list-ref nums (first left)) (list-ref nums (first right))) ;; Compare elements and merge accordingly
        (begin
          (vector-set! counts (first left) (+ (vector-ref counts (first left)) count))
          (loop (rest left) right (append merged (list (first left))) count)))
       (else
        (loop left (rest right) (append merged (list (first right))) (+ count (length left)))))))
  
  ;; Initialize counts vector with zeros, one for each element in nums
  (define counts (make-vector (length nums) 0))
  ;; Calls merge-sort function on a list of indices (0 to length of nums - 1)
  (merge-sort (range 0 (length nums)))
  ;; Converts counts vector to a list before returning
  (vector->list counts))

;; Example usage:
(countSmaller '(5 2 6 1))
;; Output: '(2 1 1 0)
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