#lang racket

;; This function calculates the number of distinct solutions to the n-queens puzzle
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
  ;; Helper function to backtrack and count solutions
  (define (backtrack cols)
    (if (= (length cols) n)
        1  ;; If all queens are placed, count as a solution
        ;; Otherwise, try placing a queen in each row of the next column
        (let loop ((i 0) (count 0))
          (if (< i n)
              (if (is-safe cols i)
                  ;; If it's safe to place a queen, do so and continue backtracking
                  (loop (add1 i) (+ count (backtrack (append cols (list i)))))
                  ;; If not safe, just move to the next row
                  (loop (add1 i) count))
              count)))) ;; Return the total count after trying all rows
  
  ;; Helper function to check if it's safe to place a queen at the given row
  (define (is-safe cols row)
    (let ((col (length cols)))
      (let loop ((i 0))
        (if (< i col)
            (let ((r (list-ref cols i)))
              (if (or (= r row) (= (abs (- i col)) (abs (- r row))))
                  #f  ;; Conflict found, not safe
                  (loop (add1 i))))
            #t)))) ;; No conflicts, safe to place
  
  ;; Start the backtracking process with an empty list of column placements
  (backtrack '()))

;; Example usage:
;; (total_n_queens 4) ;; => 2
;; (total_n_queens 1) ;; => 1
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