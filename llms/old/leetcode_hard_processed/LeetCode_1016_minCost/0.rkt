#lang racket

;; Given an m x n grid. Each cell of the grid has a sign pointing to the next cell you should visit if you are currently in this cell. The sign of grid[i][j] can be:
;; 1 which means go to the cell to the right. (i.e go from grid[i][j] to grid[i][j + 1])
;; 2 which means go to the cell to the left. (i.e go from grid[i][j] to grid[i][j - 1])
;; 3 which means go to the lower cell. (i.e go from grid[i][j] to grid[i + 1][j])
;; 4 which means go to the upper cell. (i.e go from grid[i][j] to grid[i - 1][j])
;; Notice that there could be some signs on the cells of the grid that point outside the grid.
;; You will initially start at the upper left cell (0, 0). A valid path in the grid is a path that starts from the upper left cell (0, 0) and ends at the bottom-right cell (m - 1, n - 1) following the signs on the grid. The valid path does not have to be the shortest.
;; You can modify the sign on a cell with cost = 1. You can modify the sign on a cell one time only.
;; Return the minimum cost to make the grid have at least one valid path.
;; Example 1:
;; Input: grid = [[1,1,1,1],[2,2,2,2],[1,1,1,1],[2,2,2,2]]
;; Output: 3
;; Explanation: You will start at point (0, 0).
;; The path to (3, 3) is as follows. (0, 0) --> (0, 1) --> (0, 2) --> (0, 3) change the arrow to down with cost = 1 --> (1, 3) --> (1, 2) --> (1, 1) --> (1, 0) change the arrow to down with cost = 1 --> (2, 0) --> (2, 1) --> (2, 2) --> (2, 3) change the arrow to down with cost = 1 --> (3, 3)
;; The total cost = 3.
;; Example 2:
;; Input: grid = [[1,1,3],[3,2,2],[1,1,4]]
;; Output: 0
;; Explanation: You can follow the path from (0, 0) to (2, 2).
;; Example 3:
;; Input: grid = [[1,2],[4,3]]
;; Output: 1
;; Constraints:
;; m == grid.length
;; n == grid[i].length
;; 1 <= m, n <= 100
;; 1 <= grid[i][j] <= 4
(define (minCost grid)
  ;; Extract the dimensions of the grid
  (define m (length grid))
  (define n (length (first grid)))
  
  ;; Initialize visited matrix with high values
  (define visited (make-matrix m n 1000000))
  
  ;; Queue for BFS, starting from (0, 0)
  (define q (list (cons 0 0)))
  
  ;; Set the starting point cost to 0
  (matrix-set! visited 0 0 0)
  
  ;; Directions for right, down, left, up
  (define dx '(1 0 -1 0))
  (define dy '(0 1 0 -1))
  
  ;; While there are positions to visit
  (let loop ((q q))
    (unless (null? q)
      (let* ((current (car q))
             (y (car current))
             (x (cdr current))
             (cost (matrix-ref visited y x)))
        (when (and (= y (- m 1)) (= x (- n 1)))
          (set! cost (matrix-ref visited y x))) ; Found the end, update cost
        
        ;; For each direction
        (for ([i (in-range 4)])
          (let* ((newY (+ y (list-ref dy i)))
                 (newX (+ x (list-ref dx i)))
                 ;; Check if moving in the current grid direction incurs a cost
                 (newCost (+ cost (if (= (+ i 1) (list-ref (list-ref grid y) x)) 0 1))))
            ;; If within bounds and cost is lower, update and add to queue
            (when (and (>= newY 0) (< newY m) (>= newX 0) (< newX n))
              (when (< newCost (matrix-ref visited newY newX))
                (matrix-set! visited newY newX newCost)
                (set! q (append q (list (cons newY newX))))))))
        ;; Recur without the current element
        (loop (cdr q)))))
  
  ;; Return the cost to reach the bottom-right corner
  (matrix-ref visited (- m 1) (- n 1)))

;; Helper functions for matrix manipulation
(define (make-matrix rows cols fill)
  (for/list ([i (in-range rows)])
    (make-vector cols fill)))

(define (matrix-set! matrix row col val)
  (vector-set! (list-ref matrix row) col val))

(define (matrix-ref matrix row col)
  (vector-ref (list-ref matrix row) col))
(require rackunit)

(define (test-humaneval) 

  (let (( candidate minCost))
    (check-within (candidate (list (list 1))) 0 0.001)
    (check-within (candidate (list (list 1 1 1 1 1) (list 1 1 1 1 1) (list 1 1 1 1 1) (list 1 1 1 1 1) (list 1 1 1 1 1))) 4 0.001)
    (check-within (candidate (list (list 3 3 3 3) (list 2 2 2 2) (list 3 3 3 3) (list 1 1 1 1))) 2 0.001)
    (check-within (candidate (list (list 1 1 2 2 2) (list 2 2 2 2 2) (list 2 2 2 2 2) (list 4 3 3 3 3) (list 4 4 3 3 2))) 3 0.001)
    (check-within (candidate (list (list 1 1 1 1) (list 2 2 2 2) (list 3 3 3 3) (list 4 4 4 4))) 2 0.001)
    (check-within (candidate (list (list 1 2 2 2 2) (list 2 2 2 2 2) (list 2 2 2 2 2) (list 4 3 3 3 3) (list 4 4 3 3 1))) 4 0.001)
))

(test-humaneval)