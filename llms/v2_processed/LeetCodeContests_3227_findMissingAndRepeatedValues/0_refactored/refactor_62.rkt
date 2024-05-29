#lang racket

;; Function to find the repeated and missing numbers in a n*n grid.
;; You are given a 0-indexed 2D integer matrix grid of size n * n with values in the range [1, n2]. Each integer appears exactly once except a which appears twice and b which is missing. The task is to find the repeating and missing numbers a and b.
;; Return a 0-indexed integer array ans of size 2 where ans[0] equals to a and ans[1] equals to b.
;; Example 1:
;; Input: grid = [[1,3],[2,2]]
;; Output: [2,4]
;; Explanation: Number 2 is repeated and number 4 is missing so the answer is [2,4].
;; Example 2:
;; Input: grid = [[9,1,7],[8,9,2],[3,4,6]]
;; Output: [9,5]
;; Explanation: Number 9 is repeated and number 5 is missing so the answer is [9,5].
;; Constraints:
;; 2 <= n == grid.length == grid[i].length <= 50
;; 1 <= grid[i][j] <= n * n
;; For all x that 1 <= x <= n * n there is exactly one x that is not equal to any of the grid members.
;; For all x that 1 <= x <= n * n there is exactly one x that is equal to exactly two of the grid members.
;; For all x that 1 <= x <= n * n except two of them there is exatly one pair of i, j that 0 <= i, j <= n - 1 and grid[i][j] == x.
(define (findMissingAndRepeatedValues grid)
  (define n (length grid))
  (define max-val (* n n))
  (define flat-list (apply append grid))
  (define occurrences (make-hash))

  ;; Populate the hash table with counts
  (for-each (λ (num) (hash-update! occurrences num add1 0)) flat-list)

  ;; Find the repeated and missing numbers
  (define-values (repeated missing)
    (for/fold ([repeated #f] [missing #f]) ([i (in-range 1 (add1 max-val))])
      (match (hash-ref occurrences i 0)
        [0 (values repeated i)]
        [2 (values i missing)]
        [_ (values repeated missing)])))

  (list repeated missing))

;; Example usage:
(findMissingAndRepeatedValues '((1 3) (2 2)))  ; Output: '(2 4)
(findMissingAndRepeatedValues '((9 1 7) (8 9 2) (3 4 6)))  ; Output: '(9 5)

(require rackunit)


(define (test-humaneval) 

  (let (( candidate findMissingAndRepeatedValues))
    (check-within (candidate (list (list 1 3) (list 2 2))) (list 2 4) 0.001)
    (check-within (candidate (list (list 9 1 7) (list 8 9 2) (list 3 4 6))) (list 9 5) 0.001)
    (check-within (candidate (list (list 1 1) (list 3 2))) (list 1 4) 0.001)
    (check-within (candidate (list (list 1 1) (list 3 4))) (list 1 2) 0.001)
    (check-within (candidate (list (list 1 2) (list 1 3))) (list 1 4) 0.001)
    (check-within (candidate (list (list 1 2) (list 1 4))) (list 1 3) 0.001)
    (check-within (candidate (list (list 1 2) (list 3 3))) (list 3 4) 0.001)
    (check-within (candidate (list (list 1 2) (list 4 1))) (list 1 3) 0.001)
    (check-within (candidate (list (list 1 2) (list 4 2))) (list 2 3) 0.001)
    (check-within (candidate (list (list 1 2) (list 4 4))) (list 4 3) 0.001)
    (check-within (candidate (list (list 1 4) (list 1 3))) (list 1 2) 0.001)
    (check-within (candidate (list (list 1 4) (list 2 1))) (list 1 3) 0.001)
    (check-within (candidate (list (list 1 4) (list 3 1))) (list 1 2) 0.001)
    (check-within (candidate (list (list 1 4) (list 3 4))) (list 4 2) 0.001)
    (check-within (candidate (list (list 1 4) (list 4 2))) (list 4 3) 0.001)
    (check-within (candidate (list (list 2 1) (list 4 2))) (list 2 3) 0.001)
    (check-within (candidate (list (list 2 1) (list 4 4))) (list 4 3) 0.001)
    (check-within (candidate (list (list 2 2) (list 3 4))) (list 2 1) 0.001)
    (check-within (candidate (list (list 2 2) (list 4 1))) (list 2 3) 0.001)
    (check-within (candidate (list (list 2 3) (list 2 1))) (list 2 4) 0.001)
    (check-within (candidate (list (list 2 3) (list 4 3))) (list 3 1) 0.001)
    (check-within (candidate (list (list 2 4) (list 3 2))) (list 2 1) 0.001)
    (check-within (candidate (list (list 2 4) (list 3 4))) (list 4 1) 0.001)
    (check-within (candidate (list (list 2 4) (list 4 1))) (list 4 3) 0.001)
    (check-within (candidate (list (list 3 1) (list 3 2))) (list 3 4) 0.001)
    (check-within (candidate (list (list 3 1) (list 3 4))) (list 3 2) 0.001)
    (check-within (candidate (list (list 3 1) (list 4 4))) (list 4 2) 0.001)
    (check-within (candidate (list (list 3 3) (list 1 4))) (list 3 2) 0.001)
    (check-within (candidate (list (list 3 4) (list 2 2))) (list 2 1) 0.001)
    (check-within (candidate (list (list 3 4) (list 2 4))) (list 4 1) 0.001)
    (check-within (candidate (list (list 3 4) (list 3 1))) (list 3 2) 0.001)
    (check-within (candidate (list (list 3 4) (list 4 1))) (list 4 2) 0.001)
    (check-within (candidate (list (list 4 1) (list 1 2))) (list 1 3) 0.001)
    (check-within (candidate (list (list 4 1) (list 2 2))) (list 2 3) 0.001)
    (check-within (candidate (list (list 4 1) (list 2 4))) (list 4 3) 0.001)
    (check-within (candidate (list (list 4 1) (list 3 1))) (list 1 2) 0.001)
    (check-within (candidate (list (list 4 1) (list 3 3))) (list 3 2) 0.001)
    (check-within (candidate (list (list 4 1) (list 4 2))) (list 4 3) 0.001)
    (check-within (candidate (list (list 4 2) (list 2 3))) (list 2 1) 0.001)
    (check-within (candidate (list (list 4 2) (list 4 1))) (list 4 3) 0.001)
    (check-within (candidate (list (list 4 3) (list 1 1))) (list 1 2) 0.001)
    (check-within (candidate (list (list 4 3) (list 2 2))) (list 2 1) 0.001)
    (check-within (candidate (list (list 4 3) (list 2 4))) (list 4 1) 0.001)
    (check-within (candidate (list (list 4 3) (list 3 1))) (list 3 2) 0.001)
    (check-within (candidate (list (list 4 4) (list 2 3))) (list 4 1) 0.001)
    (check-within (candidate (list (list 1 3 4) (list 9 7 5) (list 8 2 3))) (list 3 6) 0.001)
    (check-within (candidate (list (list 1 5 2) (list 8 4 3) (list 7 8 6))) (list 8 9) 0.001)
    (check-within (candidate (list (list 1 5 8) (list 2 7 3) (list 6 1 4))) (list 1 9) 0.001)
    (check-within (candidate (list (list 1 6 1) (list 4 3 7) (list 5 2 8))) (list 1 9) 0.001)
    (check-within (candidate (list (list 1 6 4) (list 9 7 5) (list 7 8 2))) (list 7 3) 0.001)
    (check-within (candidate (list (list 1 6 7) (list 3 6 8) (list 9 5 4))) (list 6 2) 0.001)
    (check-within (candidate (list (list 1 7 4) (list 8 6 2) (list 8 3 5))) (list 8 9) 0.001)
    (check-within (candidate (list (list 1 7 8) (list 4 5 6) (list 3 9 9))) (list 9 2) 0.001)
    (check-within (candidate (list (list 1 8 4) (list 9 2 7) (list 6 3 8))) (list 8 5) 0.001)
    (check-within (candidate (list (list 1 8 5) (list 4 3 2) (list 7 9 4))) (list 4 6) 0.001)
    (check-within (candidate (list (list 1 9 3) (list 2 7 8) (list 2 4 5))) (list 2 6) 0.001)
    (check-within (candidate (list (list 1 9 7) (list 8 4 2) (list 6 3 9))) (list 9 5) 0.001)
    (check-within (candidate (list (list 2 1 3) (list 2 9 4) (list 6 8 5))) (list 2 7) 0.001)
    (check-within (candidate (list (list 2 2 4) (list 7 5 3) (list 1 6 8))) (list 2 9) 0.001)
    (check-within (candidate (list (list 2 3 9) (list 5 6 4) (list 2 8 7))) (list 2 1) 0.001)
    (check-within (candidate (list (list 2 4 6) (list 4 8 9) (list 7 3 5))) (list 4 1) 0.001)
    (check-within (candidate (list (list 2 5 5) (list 4 8 7) (list 9 3 6))) (list 5 1) 0.001)
    (check-within (candidate (list (list 2 6 4) (list 6 9 5) (list 3 7 8))) (list 6 1) 0.001)
    (check-within (candidate (list (list 2 6 9) (list 1 7 9) (list 4 8 5))) (list 9 3) 0.001)
    (check-within (candidate (list (list 2 7 1) (list 8 6 2) (list 9 3 4))) (list 2 5) 0.001)
    (check-within (candidate (list (list 2 7 5) (list 7 6 4) (list 1 3 9))) (list 7 8) 0.001)
    (check-within (candidate (list (list 2 7 9) (list 6 8 1) (list 4 1 5))) (list 1 3) 0.001)
    (check-within (candidate (list (list 2 9 7) (list 8 5 1) (list 6 7 4))) (list 7 3) 0.001)
    (check-within (candidate (list (list 3 4 5) (list 8 2 4) (list 6 1 7))) (list 4 9) 0.001)
    (check-within (candidate (list (list 3 5 7) (list 8 6 9) (list 1 5 2))) (list 5 4) 0.001)
    (check-within (candidate (list (list 3 6 1) (list 5 9 2) (list 1 7 8))) (list 1 4) 0.001)
    (check-within (candidate (list (list 3 9 4) (list 3 6 1) (list 5 7 2))) (list 3 8) 0.001)
    (check-within (candidate (list (list 4 2 6) (list 3 5 8) (list 3 1 9))) (list 3 7) 0.001)
    (check-within (candidate (list (list 4 3 2) (list 6 9 9) (list 8 7 5))) (list 9 1) 0.001)
    (check-within (candidate (list (list 4 6 5) (list 3 5 7) (list 2 8 9))) (list 5 1) 0.001)
    (check-within (candidate (list (list 4 8 7) (list 4 6 9) (list 3 2 1))) (list 4 5) 0.001)
    (check-within (candidate (list (list 4 9 6) (list 2 5 8) (list 3 7 7))) (list 7 1) 0.001)
    (check-within (candidate (list (list 5 3 6) (list 1 4 2) (list 9 8 2))) (list 2 7) 0.001)
    (check-within (candidate (list (list 5 6 9) (list 3 7 8) (list 2 2 4))) (list 2 1) 0.001)
    (check-within (candidate (list (list 5 7 8) (list 1 3 2) (list 7 6 9))) (list 7 4) 0.001)
    (check-within (candidate (list (list 6 1 3) (list 2 4 2) (list 8 9 7))) (list 2 5) 0.001)
    (check-within (candidate (list (list 6 4 2) (list 3 7 8) (list 5 6 9))) (list 6 1) 0.001)
    (check-within (candidate (list (list 6 4 5) (list 7 9 3) (list 1 2 9))) (list 9 8) 0.001)
    (check-within (candidate (list (list 6 4 8) (list 8 1 2) (list 9 3 7))) (list 8 5) 0.001)
    (check-within (candidate (list (list 6 9 3) (list 8 9 7) (list 5 4 2))) (list 9 1) 0.001)
    (check-within (candidate (list (list 7 2 1) (list 6 5 3) (list 2 9 4))) (list 2 8) 0.001)
    (check-within (candidate (list (list 7 2 4) (list 5 8 7) (list 9 3 1))) (list 7 6) 0.001)
    (check-within (candidate (list (list 7 3 1) (list 8 9 2) (list 4 5 2))) (list 2 6) 0.001)
    (check-within (candidate (list (list 7 4 2) (list 9 1 9) (list 8 3 5))) (list 9 6) 0.001)
    (check-within (candidate (list (list 7 4 8) (list 1 1 3) (list 2 6 9))) (list 1 5) 0.001)
    (check-within (candidate (list (list 7 5 3) (list 4 6 3) (list 9 2 8))) (list 3 1) 0.001)
    (check-within (candidate (list (list 7 5 7) (list 3 1 6) (list 8 9 4))) (list 7 2) 0.001)
    (check-within (candidate (list (list 8 2 6) (list 1 8 9) (list 4 5 3))) (list 8 7) 0.001)
    (check-within (candidate (list (list 8 2 7) (list 3 5 1) (list 9 6 3))) (list 3 4) 0.001)
    (check-within (candidate (list (list 8 6 3) (list 1 9 5) (list 5 4 7))) (list 5 2) 0.001)
    (check-within (candidate (list (list 8 6 5) (list 3 9 1) (list 8 7 4))) (list 8 2) 0.001)
    (check-within (candidate (list (list 8 9 6) (list 6 1 3) (list 2 7 5))) (list 6 4) 0.001)
    (check-within (candidate (list (list 8 9 6) (list 7 4 2) (list 7 1 5))) (list 7 3) 0.001)
    (check-within (candidate (list (list 9 2 3) (list 7 6 4) (list 5 8 8))) (list 8 1) 0.001)
    (check-within (candidate (list (list 9 2 7) (list 3 8 7) (list 1 5 4))) (list 7 6) 0.001)
))

(test-humaneval)