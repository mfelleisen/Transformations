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
  ;; Helper function to check if a queen placement is safe.
  ;; It checks if the current row placement conflicts with any existing queen.
  (define (is-safe cols row)
    (let ((col (length cols)))
      (let loop ((i 0))
        (if (< i col)
            (let ((r (list-ref cols i)))
              (if (or (= r row) (= (abs (- i col)) (abs (- r row))))
                  #f
                  (loop (+ i 1))))
            #t))))
  
  ;; Backtracking function to explore all possible queen placements
  ;; and count valid solutions.
  (define (backtrack n cols)
    (if (= (length cols) n)
        1
        (let loop ((i 0) (count 0))
          (if (< i n)
              (if (is-safe cols i)
                  (let ((new-cols (append cols (list i))))
                    (loop (+ i 1) (+ count (backtrack n new-cols))))
                  (loop (+ i 1) count))
              count))))
  
  ;; Initial call to the backtracking function with an empty list for columns.
  (backtrack n '()))

;; Example calls
(total_n_queens 4) ;; Returns: 2
(total_n_queens 1) ;; Returns: 1
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