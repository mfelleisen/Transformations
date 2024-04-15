#lang racket

;; Function to calculate the total number of distinct solutions to the n-queens puzzle.
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
  ;; Helper function to backtrack and place queens on the board.
  (define (backtrack n cols)
    ;; If the length of cols is equal to n, a solution is found.
    (if (= (length cols) n)
        1  ;; Return 1 as a count for one solution.
        ;; Otherwise, try placing a queen in each row of the next column.
        (let loop ((i 0) (count 0))
          (if (< i n)
              (if (is-safe cols i)
                  ;; If it is safe to place a queen, do so and continue the search.
                  (let ((new-cols (append cols (list i))))  ;; Place queen and generate new cols.
                    (loop (+ i 1) (+ count (backtrack n new-cols))))  ;; Recur with the new placement and increment count if a solution is found.
                  ;; If not safe, just move to the next row.
                  (loop (+ i 1) count))
              ;; Return the total count of solutions found for this column.
              count))))
  
  ;; Helper function to check if the current placement is safe.
  (define (is-safe cols row)
    (let ((col (length cols)))
      ;; Check against all previously placed queens.
      (let loop ((i 0))
        (if (< i col)
            (let ((r (list-ref cols i)))
              (if (or (= r row) (= (abs (- i col)) (abs (- r row))))
                  #f  ;; Not safe if on the same row or diagonal.
                  (loop (+ i 1))))
            #t))))  ;; Safe if none of the conditions are met.

  ;; Start the search with an empty list of column placements.
  (backtrack n '()))

;; Example Usage:
(total_n_queens 4)  ;; Should return 2.
(total_n_queens 1)  ;; Should return 1.
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