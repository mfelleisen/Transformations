#lang racket

;; The n-queens puzzle is the problem of placing n queens on an n x n chessboard such that no two queens attack each other.
;; Given an integer n, return the number of distinct solutions to the n-queens puzzle.
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
  ;; Helper function to check if it's safe to place a queen.
  ;; Checks for conflicts with existing queens.
  (define (is-safe cols row)
    (let ([col (length cols)])
      (let loop ((i 0))
        (if (= i col)
            #t
            (let ([r (list-ref cols i)])
              (if (or (= r row) (= (abs (- i col)) (abs (- r row))))
                  #f
                  (loop (add1 i))))))))
  
  ;; Backtracking function to explore all valid arrangements.
  ;; Adds a queen and checks for validity recursively.
  (define (backtrack n cols)
    (if (= (length cols) n)
        1  ; Found a valid arrangement.
        (let loop ((i 0) (count 0))
          (if (= i n)
              count  ; Explored all rows for the current column.
              (if (is-safe cols i)
                  (let ([new-cols (append cols (list i))])  ; Append the new row position.
                    (loop (add1 i) (+ count (backtrack n new-cols))))
                  (loop (add1 i) count))))))  ; Move to next row if not safe.

  ;; Initial call to backtrack with an empty list representing the column positions of the queens.
  (backtrack n '()))

;; Example usage:
;; (total_n_queens 4) -> 2
;; (total_n_queens 1) -> 1
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