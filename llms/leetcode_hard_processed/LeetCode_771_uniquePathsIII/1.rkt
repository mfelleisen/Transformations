#lang racket

;; You are given an m x n integer list grid where grid[i][j] could be:
;; 1 representing the starting square. There is exactly one starting square.
;; 2 representing the ending square. There is exactly one ending square.
;; 0 representing empty squares we can walk over.
;; -1 representing obstacles that we cannot walk over.
;; Return the number of 4-directional walks from the starting square to the ending square, that walk over every non-obstacle square exactly once.
;; Example 1:
;; Input: grid = [[1,0,0,0],[0,0,0,0],[0,0,2,-1]]
;; Output: 2
;; Explanation: We have the following two paths: 
;; 1. (0,0),(0,1),(0,2),(0,3),(1,3),(1,2),(1,1),(1,0),(2,0),(2,1),(2,2)
;; 2. (0,0),(1,0),(2,0),(2,1),(1,1),(0,1),(0,2),(0,3),(1,3),(1,2),(2,2)
;; Example 2:
;; Input: grid = [[1,0,0,0],[0,0,0,0],[0,0,0,2]]
;; Output: 4
;; Explanation: We have the following four paths: 
;; 1. (0,0),(0,1),(0,2),(0,3),(1,3),(1,2),(1,1),(1,0),(2,0),(2,1),(2,2),(2,3)
;; 2. (0,0),(0,1),(1,1),(1,0),(2,0),(2,1),(2,2),(1,2),(0,2),(0,3),(1,3),(2,3)
;; 3. (0,0),(1,0),(2,0),(2,1),(2,2),(1,2),(1,1),(0,1),(0,2),(0,3),(1,3),(2,3)
;; 4. (0,0),(1,0),(2,0),(2,1),(1,1),(0,1),(0,2),(0,3),(1,3),(1,2),(2,2),(2,3)
;; Example 3:
;; Input: grid = [[0,1],[2,0]]
;; Output: 0
;; Explanation: There is no path that walks over every empty square exactly once.
;; Note that the starting and ending square can be anywhere in the grid.
;; Constraints:
;; m == grid.length
;; n == grid[i].length
;; 1 <= m, n <= 20
;; 1 <= m * n <= 20
;; -1 <= grid[i][j] <= 2
;; There is exactly one starting cell and one ending cell.
(define (uniquePathsIII grid)
  ;; Depth-first search function to traverse the grid
  (define (dfs grid x y empty)
    (cond
      ;; Check for out-of-bounds or hitting an obstacle
      [(or (< x 0) (>= x (length grid))
            (< y 0) (>= y (length (first grid)))
            (= (list-ref (list-ref grid x) y) -1))
       0]
      ;; If the ending square is reached and all non-obstacle squares are walked over
      [(= (list-ref (list-ref grid x) y) 2)
       (if (= empty -1) 1 0)]
      ;; Recursive DFS calls for 4-directional walks
      [else
       (begin
         (set! grid (update-grid grid x y -1)) ;; Mark current square as visited
         (define paths (+ (dfs grid (+ x 1) y (- empty 1))
                          (dfs grid (- x 1) y (- empty 1))
                          (dfs grid x (+ y 1) (- empty 1))
                          (dfs grid x (- y 1) (- empty 1))))
         (set! grid (update-grid grid x y 0)) ;; Unmark current square for other paths
         paths)]))
  
  ;; Helper function to update the value at a specific grid position
  (define (update-grid grid x y val)
    (map (lambda (row i)
           (if (= i x)
               (map (lambda (cell j) (if (= j y) val cell)) row (range (length row)))
               row))
         grid
         (range (length grid))))
  
  ;; Find the starting square and count the number of empty squares
  (define-values (start-x start-y empty) 
    (let loop ((i 0) (j 0) (x 0) (y 0) (empty 1))
      (cond
        [(= i (length grid)) (values x y empty)]
        [(= j (length (first grid))) (loop (+ i 1) 0 x y empty)]
        [else
         (cond
           [(= (list-ref (list-ref grid i) j) 1) (loop i (+ j 1) i j empty)]
           [(= (list-ref (list-ref grid i) j) 0) (loop i (+ j 1) x y (+ empty 1))]
           [else (loop i (+ j 1) x y empty)])])))
  
  ;; Call the DFS function from the starting square
  (dfs grid start-x start-y empty))

;; Example usage:
(uniquePathsIII '((1 0 0 0) (0 0 0 0) (0 0 2 -1))) ;; => 2
(uniquePathsIII '((1 0 0 0) (0 0 0 0) (0 0 0 2))) ;; => 4
(uniquePathsIII '((0 1) (2 0))) ;; => 0
(require rackunit)

(define (test-humaneval) 

  (let (( candidate uniquePathsIII))
    (check-within (candidate (list (list 0))) 0 0.001)
    (check-within (candidate (list (list 1 0 0 0) (list 0 0 -1 0) (list 0 0 0 2))) 0 0.001)
    (check-within (candidate (list (list 0 1) (list 2 0))) 0 0.001)
    (check-within (candidate (list (list 1 0 0 0 0) (list 0 0 0 0 0) (list 0 0 0 0 -1) (list 0 0 0 0 2))) 0 0.001)
    (check-within (candidate (list (list 0) (list 0) (list 0))) 0 0.001)
    (check-within (candidate (list (list 1 -1 0 0) (list 0 0 0 0) (list 0 0 0 2))) 0 0.001)
    (check-within (candidate (list (list 1 0 0 0) (list 0 0 0 0) (list 0 0 0 2) (list 0 0 0 -1))) 0 0.001)
    (check-within (candidate (list (list 0) (list 0))) 0 0.001)
))

(test-humaneval)