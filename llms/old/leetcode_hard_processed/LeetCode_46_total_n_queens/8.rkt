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
  ;; This function calculates the number of distinct solutions to the n-queens puzzle.
  (define (backtrack n cols)
    ;; A helper function for backtracking.
    ;; It tries to place a queen in each column and checks if it's safe.
    (if (= (length cols) n)
        1  ;; If all queens are placed, return 1 as a valid solution.
        (let loop ((i 0) (count 0))
          (if (< i n)
              (if (is_safe cols i)
                  (loop (+ i 1) (+ count (backtrack n (append cols (list i)))))
                  (loop (+ i 1) count))  ;; If not safe, try next position.
              count))))  ;; Return the count of solutions.
  (define (is_safe cols row)
    ;; Checks if the position is safe for a queen by checking all previous queens.
    (let ((col (length cols)))
      (let loop ((i 0))
        (if (< i col)
            (let ((r (list-ref cols i)))
              (if (or (= r row) (= (abs (- i col)) (abs (- r row))))
                  #f  ;; If attack is possible, return false.
                  (loop (+ i 1))))
            #t))))  ;; If all checks passed, return true.
  (backtrack n '()))  ;; Start the backtracking with an empty list for columns.

;; Example usage:
;; (total_n_queens 4)  ;; Should return 2
;; (total_n_queens 1)  ;; Should return 1
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