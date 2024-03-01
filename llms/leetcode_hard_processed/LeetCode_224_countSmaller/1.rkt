#lang racket

;; Function to count the number of smaller elements to the right for each element in a list
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
  ;; Nested function to perform merge sort on the indices, while updating the counts
  (define (merge-sort indices)
    (if (<= (length indices) 1)
        indices
        (let ([mid (quotient (length indices) 2)])
          (merge (merge-sort (take indices mid))
                 (merge-sort (drop indices mid))))))

  ;; Nested function to merge two lists of indices, updating counts based on the merge process
  (define (merge left right)
    (let loop ([left left] [right right] [merged '()] [count 0])
      (cond [(and (null? left) (null? right))
             (reverse merged)]
            [(null? right)
             (begin
               (set! counts (update-counts counts left count))
               (loop (cdr left) right (cons (car left) merged) count))]
            [(null? left)
             (loop left (cdr right) (cons (car right) merged) count)]
            [(<= (list-ref nums (car left)) (list-ref nums (car right)))
             (begin
               (set! counts (update-in-list counts (car left) (+ (list-ref counts (car left)) count)))
               (loop (cdr left) right (cons (car left) merged) count))]
            [else
             (loop left (cdr right) (cons (car right) merged) (+ count (length left)))])))
  
  ;; Helper function to update a list at a specific index
  (define (update-in-list lst idx val)
    (map (lambda (i x) (if (= i idx) val x)) (range (length lst)) lst))

  ;; Helper function to update counts for remaining elements in left list after merging
  (define (update-counts lst left count)
    (foldl (lambda (idx acc) (update-in-list acc idx (+ (list-ref acc idx) count))) lst left))
  
  ;; Initialize counts list with zeros
  (define counts (make-list (length nums) 0))
  
  ;; Trigger the sorting and counting process
  (merge-sort (range (length nums)))
  
  ;; Return the counts
  counts)
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