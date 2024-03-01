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
  ;; Helper function to perform depth-first search
  (define (dfs grid x y empty)
    (cond
      ;; If out of bounds or on an obstacle, return 0
      [(or (< x 0) (< y 0) (>= x (length grid)) (>= y (length (first grid))) (= (list-ref (list-ref grid x) y) -1))
       0]
      ;; If on the ending square and all squares are visited, return 1
      [(= (list-ref (list-ref grid x) y) 2)
       (if (= empty -1) 1 0)]
      ;; Else, mark the current square as visited and explore the 4 directions
      [else
       (let ([orig (list-ref (list-ref grid x) y)])
         (set! grid (update-grid grid x y -1)) ; Mark as visited by setting to -1
         (define paths 
           (+ (dfs grid (+ x 1) y (- empty 1))
              (dfs grid (- x 1) y (- empty 1))
              (dfs grid x (+ y 1) (- empty 1))
              (dfs grid x (- y 1) (- empty 1))))
         (set! grid (update-grid grid x y orig)) ; Reset to original state
         paths)]))
  
  ;; Helper function to update the grid
  (define (update-grid grid x y val)
    (map (lambda (row i) (if (= i x) (update-row row y val) row))
         grid
         (range (length grid))))
  
  ;; Helper function to update a row
  (define (update-row row y val)
    (map (lambda (elem j) (if (= j y) val elem))
         row
         (range (length row))))
  
  ;; Find the starting position and count empty squares
  (define-values (start-x start-y empty) 
    (let loop ([i 0] [j 0] [start-x 0] [start-y 0] [empty 1])
      (cond
        [(= i (length grid)) (values start-x start-y empty)]
        [(= j (length (first grid))) (loop (+ i 1) 0 start-x start-y empty)]
        [else
         (case (list-ref (list-ref grid i) j)
           [(1) (loop i (+ j 1) i j empty)]
           [(0) (loop i (+ j 1) start-x start-y (+ empty 1))]
           [else (loop i (+ j 1) start-x start-y empty)])])))
  
  ;; Start the depth-first search
  (dfs grid start-x start-y empty))

;; The code defines a function `uniquePathsIII` that computes the number of unique paths from the start square to the end square in a grid, where each non-obstacle square is visited exactly once.
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