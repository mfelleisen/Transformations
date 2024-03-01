#lang racket
;; The n-queens puzzle solution in Racket. The goal is to place n queens on an n x n chessboard 
;; such that no two queens attack each other. This function returns the number of distinct solutions.

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
  ;; A helper function to check if the placement of the current queen is safe.
  ;; It checks against all previously placed queens for any conflicts.
  (define (is-safe cols row)
    (define col (length cols))
    (define (check-conflict prev-cols step)
      (cond
        [(null? prev-cols) #t] ; No more columns to check, placement is safe
        [(or (= (car prev-cols) row) ; Check for same row or diagonal conflict
             (= (abs (- (car prev-cols) row)) step))
         #f] ; Conflict found
        [else (check-conflict (cdr prev-cols) (add1 step))])) ; Check next column
    (check-conflict (reverse cols) 1))
  
  ;; A helper function for backtracking and exploring all possible placements of queens.
  ;; It returns the count of valid solutions found from the current state.
  (define (backtrack n cols)
    (if (= (length cols) n)
        1 ; Found a valid solution
        (let loop ((i 0) (count 0))
          (if (= i n)
              count ; Explored all rows for the current column, return count
              (if (is-safe cols i)
                  (loop (add1 i) (+ count (backtrack n (append cols (list i))))) ; Explore next row with current row as part of the solution
                  (loop (add1 i) count))))) ; Explore next row without including the current row
    )
  ;; Start the backtracking process with an empty list representing no queens placed.
  (backtrack n '()))

;; Example usage:
;; (total_n_queens 4) ; Should return 2
;; (total_n_queens 1) ; Should return 1
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