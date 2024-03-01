#lang racket

;; An list is squareful if the sum of every pair of adjacent elements is a perfect square.
;; Given an integer list nums, return the number of permutations of nums that are squareful.
;; Two permutations perm1 and perm2 are different if there is some index i such that perm1[i] != perm2[i].
;; Example 1:
;; Input: nums = [1,17,8]
;; Output: 2
;; Explanation: [1,8,17] and [17,8,1] are the valid permutations.
;; Example 2:
;; Input: nums = [2,2,2]
;; Output: 1
;; Constraints:
;; 1 <= nums.length <= 12
;; 0 <= nums[i] <= 109
(define (numSquarefulPerms nums)
  ;; Check if a number is a perfect square
  (define (is-square? n)
    (equal? (* (floor (sqrt n)) (floor (sqrt n))) n))
  
  ;; Depth-first search algorithm to count squareful permutations
  (define (dfs idx nums counter)
    (if (= idx (length nums))
        1
        (let loop ((keys (hash-keys counter)) (count 0))
          (if (null? keys)
              count
              (let ((key (car keys)))
                (if (and (> (hash-ref counter key) 0)
                         (or (= idx 0)
                             (is-square? (+ (list-ref nums (- idx 1)) key))))
                    (let ((new-counter (hash-update counter key sub1)))
                      (loop (cdr keys)
                            (+ count
                               (dfs (+ idx 1)
                                   (list-set nums idx key)
                                   new-counter))))
                    (loop (cdr keys) count)))))))
  
  ;; Remove duplicates and create a counter (hash) for unique elements and their counts
  (define (nums-counter lst)
    (foldl (lambda (x acc)
             (hash-update acc x (lambda (v) (if v (+ v 1) 1)) 0))
           #hash()
           lst))
  
  ;; Initiate DFS with the counter
  (dfs 0 nums (nums-counter nums)))

;; Testing the function with given examples
(displayln (numSquarefulPerms '(1 17 8))) ; Output: 2
(displayln (numSquarefulPerms '(2 2 2)))  ; Output: 1
(require rackunit)

(define (test-humaneval) 

  (let (( candidate numSquarefulPerms))
    (check-within (candidate (list 1 2 2 3 3 3)) 0 0.001)
    (check-within (candidate (list 2 2 2 2 2 2 2 2 2 2 2 2)) 1 0.001)
    (check-within (candidate (list 2 2 2)) 1 0.001)
    (check-within (candidate (list 2 2 2 2 2 2)) 1 0.001)
    (check-within (candidate (list 1 1 1 1 1 1 1 1 1 1 1 8)) 0 0.001)
    (check-within (candidate (list 1 1 1 1 1 1 1 1 1 1 1 17)) 0 0.001)
    (check-within (candidate (list 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)) 1 0.001)
    (check-within (candidate (list 1 2 3 4 5 6 7 8 9)) 0 0.001)
    (check-within (candidate (list 1 1 1 1 1 1 1 1 1 1 1 17 17 17 17 17 17 17 17 17 17 17 17 17 17 17 17 17 17 17 17)) 0 0.001)
    (check-within (candidate (list 1 17 8)) 2 0.001)
    (check-within (candidate (list 1 1 1 1 1 1 1 1 1 1 1 17 17 17 17 17 17 17 17 17 17 17 17 17 17 17 17 17)) 0 0.001)
))

(test-humaneval)