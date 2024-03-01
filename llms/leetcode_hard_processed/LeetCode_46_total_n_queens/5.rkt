#lang racket

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
  ;; Helper function to check if the position is safe for the queen.
  (define (is-safe cols row)
    (define col (length cols))
    (define (safe? idx)
      (if (= idx col)
          #t
          (let ((r (list-ref cols idx)))
            (and (not (= r row))
                 (not (= (abs (- idx col)) (abs (- r row))))
                 (safe? (add1 idx))))))
    (safe? 0))

  ;; Backtrack function to explore all possible positions.
  (define (backtrack n cols)
    (if (= (length cols) n)
        1
        (let loop ((i 0) (count 0))
          (if (= i n)
              count
              (if (is-safe cols i)
                  (loop (add1 i) (+ count (backtrack n (append cols (list i)))))
                  (loop (add1 i) count))))))

  ;; Start the backtrack function with an empty list for columns.
  (backtrack n '()))

;; Example usage:
(total_n_queens 4)
;; Returns 2
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