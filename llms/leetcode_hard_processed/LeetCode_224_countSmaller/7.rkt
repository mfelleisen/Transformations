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
  ;; Internal function for merge sort that operates on indices rather than values,
  ;; allowing us to track the original position of elements.
  (define (merge-sort indices)
    (if (<= (length indices) 1)
        indices
        (let* ((mid (quotient (length indices) 2))
               (left (merge-sort (take indices mid)))
               (right (merge-sort (drop indices mid))))
          (merge left right))))

  ;; Merge step of merge sort, modified to count smaller elements to the right.
  (define (merge left right)
    (let loop ((left left) (right right) (merged '()) (count 0))
      (cond
        ((and (null? left) (null? right)) (reverse merged))
        ((null? left)
         (loop left (cdr right) (cons (car right) merged) count))
        ((null? right)
         (begin
           (vector-set! counts (car left) (+ (vector-ref counts (car left)) count))
           (loop (cdr left) right (cons (car left) merged) count)))
        ((<= (list-ref nums (car left)) (list-ref nums (car right)))
         (begin
           (vector-set! counts (car left) (+ (vector-ref counts (car left)) count))
           (loop (cdr left) right (cons (car left) merged) count)))
        (else
         (loop left (cdr right) (cons (car right) merged) (+ count (length left)))))))

  ;; Initialize a vector to store the count of smaller elements to the right for each element.
  (define counts (make-vector (length nums) 0))
  
  ;; Call merge-sort on a list of indices [0, 1, ..., n-1].
  (merge-sort (range 0 (length nums)))
  
  ;; Convert the counts vector to a list before returning.
  (vector->list counts))

;; Example calls
(countSmaller '(5 2 6 1)) ;; => '(2 1 1 0)
(countSmaller '(-1)) ;; => '(0)
(countSmaller '(-1 -1)) ;; => '(0 0)
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