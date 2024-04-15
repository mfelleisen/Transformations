#lang racket

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
  ;; Define a helper function for merge sort that sorts the indices based on their values in nums
  (define (merge-sort indices)
    (if (<= (length indices) 1)
        indices
        (let* ((mid (quotient (length indices) 2))
               (left (merge-sort (take indices mid)))
               (right (merge-sort (drop indices mid))))
          (merge left right))))
  
  ;; Merge two sorted lists, updating counts based on the number of smaller elements to the right
  (define (merge left right)
    (let loop ((left left) (right right) (merged '()) (count 0))
      (cond
       ;; When both sides have elements
       ((and (not (null? left)) (not (null? right)))
        (if (<= (list-ref nums (car left)) (list-ref nums (car right)))
            (begin
              (vector-set! counts (car left) (+ (vector-ref counts (car left)) count))
              (loop (cdr left) right (cons (car left) merged) count))
            (loop left (cdr right) (cons (car right) merged) (+ count (length left)))))
       ;; When only the left side has elements
       ((not (null? left))
        (begin
          (vector-set! counts (car left) (+ (vector-ref counts (car left)) count))
          (loop (cdr left) right (cons (car left) merged) count)))
       ;; When only the right side has elements
       ((not (null? right))
        (loop left (cdr right) (cons (car right) merged) count))
       ;; When both sides are empty
       (else (reverse merged)))))

  ;; Initialize counts as a vector of zeros
  (define counts (make-vector (length nums) 0))
  ;; Call merge-sort with a list of indices
  (merge-sort (build-list (length nums) values))
  ;; Convert the counts vector back to a list
  (vector->list counts))

;; Example usage
(countSmaller '(5 2 6 1))
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