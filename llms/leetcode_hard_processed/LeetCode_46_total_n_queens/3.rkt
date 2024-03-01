#lang racket

;; The n-queens puzzle is the problem of placing n queens on an n x n chessboard such that no two queens attack each other.
;; Given an integer n, return the number of distinct solutions to the n-queens puzzle.
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
  ;; Helper function to check if a position is safe. It takes the current state of the board (columns where queens are placed)
  ;; and the row position for the new queen to be placed. It checks for column and diagonal conflicts.
  (define (is-safe cols row)
    (let ((col (length cols)))
      (let loop ((i 0))
        (if (< i col)
            (let ((r (list-ref cols i)))
              (if (or (= r row) (= (abs (- i col)) (abs (- r row))))
                  #f
                  (loop (+ i 1))))
            #t))))
  ;; Backtrack function tries to place a queen in each row and recursively checks for safe placements for all queens.
  ;; Once a valid placement for all queens is found, it increments the count.
  (define (backtrack n cols)
    (if (= (length cols) n)
        1
        (let loop ((i 0)
                   (count 0))
          (if (< i n)
              (if (is-safe cols i)
                  (let ((new-count (+ count (backtrack n (append cols (list i))))))
                    (loop (+ i 1) new-count))
                  (loop (+ i 1) count))
              count))))
  ;; The main function initializes the problem with an empty list representing no queens placed, and starts the backtrack search.
  (backtrack n '()))

;; Test the function with a given n
(total_n_queens 4)  ; Output: 2
(total_n_queens 1)  ; Output: 1
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