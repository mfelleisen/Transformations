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
  ;; Function to check if a number is a perfect square
  (define (is-square? n)
    (equal? (* (floor (sqrt n)) (floor (sqrt n))) n))

  ;; Depth-first search function for generating permutations
  (define (dfs idx nums counter)
    ;; Base case: if idx is equal to the length of nums, a valid permutation is found
    (if (= idx (length nums))
        1
        (let loop ((keys (hash-keys counter)) (count 0))
          (if (null? keys)
              count
              (let* ((key (car keys))
                     (val (hash-ref counter key))
                     (next-nums (if (= idx 0) nums (list-set nums idx key))))
                (if (and (> val 0)
                         (or (= idx 0)
                             (is-square? (+ (list-ref nums (- idx 1)) key))))
                    (begin
                      (hash-set! counter key (- val 1))
                      (let ((new-count (+ count (dfs (+ idx 1) next-nums counter))))
                        (hash-set! counter key val)
                        (loop (cdr keys) new-count)))
                    (loop (cdr keys) count)))))))
  
  ;; Main logic: initializing the counter (frequency table) and calling dfs
  (let ((counter (make-hash)))
    (for-each (lambda (num) (hash-update! counter num add1 0)) nums)
    (dfs 0 nums counter)))

;; Example usage:
(numSquarefulPerms '(1 17 8))
(numSquarefulPerms '(2 2 2))
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