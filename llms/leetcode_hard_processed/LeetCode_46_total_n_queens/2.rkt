#lang racket

;; Function to count the number of distinct solutions to the n-queens puzzle.
;; The n-queens puzzle is the problem of placing n queens on an n x n chessboard such that no two queens attack each other.
;; Given an integer n, return the number of distinct solutions to the n-queens puzzle.
;; Example 1:
;; Input: n = 4
;; Output: 2
;; Explanation: There are two distinct solutions to the 4-queens puzzle as shown.
;; Example 2:
;; Input: n = 1
;; Output: 1
;; Constraints:
;; 1 <= n <= 9
(define (total_n_queens n)
  ;; Helper function to backtrack and try different queen placements.
  (define (backtrack n cols)
    ;; If we've placed n queens, we've found a solution.
    (if (= (length cols) n)
        1  ;; Return 1 to count this solution.
        ;; Otherwise, try placing a queen in each row of the next column.
        (let loop ((i 0) (count 0))
          (if (< i n)
              (if (is-safe cols i)
                  ;; If it's safe to place a queen, do it and continue backtracking.
                  (loop (add1 i) (+ count (backtrack n (append cols (list i)))))
                  ;; If not safe, just move to the next row.
                  (loop (add1 i) count))
              ;; Once all rows in the column are tried, return the count of solutions.
              count))))
  
  ;; Helper function to check if placing a queen at the given row is safe.
  (define (is-safe cols row)
    (let ((col (length cols)))
      (let loop ((i 0))
        (if (< i col)
            (let ((r (list-ref cols i)))
              (if (or (= r row) (= (abs (- i col)) (abs (- r row))))
                  #f  ;; Not safe if on the same row or diagonal.
                  (loop (add1 i))))
            #t))))  ;; Safe if none of the conditions are met.

  ;; Start the backtracking process with an empty list of columns.
  (backtrack n '()))

;; Example usage:
(total_n_queens 4)  ;; => 2
(total_n_queens 1)  ;; => 1
(require rackunit)

(define (test-humaneval) 

  (let (( candidate total_n_queens))
    (check-within (candidate 6) 4 0.001)
    (check-within (candidate 1) 1 0.001)
    (check-within (candidate 5) 10 0.001)
    (check-within (candidate 8) 92 0.001)
    (check-within (candidate 4) 2 0.001)
    (check-within (candidate 9) 352 0.001)
    (check-within (candidate 7) 40 0.001)
    (check-within (candidate 11) 2680 0.001)
    (check-within (candidate 3) 0 0.001)
    (check-within (candidate 2) 0 0.001)
    (check-within (candidate 10) 724 0.001)
))

(test-humaneval)