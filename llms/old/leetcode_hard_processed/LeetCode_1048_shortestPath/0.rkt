#lang racket

;; You are given an m x n integer matrix grid where each cell is either 0 (empty) or 1 (obstacle). You can move up, down, left, or right from and to an empty cell in one step.
;; Return the minimum number of steps to walk from the upper left corner (0, 0) to the lower right corner (m - 1, n - 1) given that you can eliminate at most k obstacles. If it is not possible to find such walk return -1.
;; Example 1:
;; Input: grid = [[0,0,0],[1,1,0],[0,0,0],[0,1,1],[0,0,0]], k = 1
;; Output: 6
;; Explanation: 
;; The shortest path without eliminating any obstacle is 10.
;; The shortest path with one obstacle elimination at position (3,2) is 6. Such path is (0,0) -> (0,1) -> (0,2) -> (1,2) -> (2,2) -> (3,2) -> (4,2).
;; Example 2:
;; Input: grid = [[0,1,1],[1,1,1],[1,0,0]], k = 1
;; Output: -1
;; Explanation: We need to eliminate at least two obstacles to find such a walk.
;; Constraints:
;; m == grid.length
;; n == grid[i].length
;; 1 <= m, n <= 40
;; 1 <= k <= m * n
;; grid[i][j] is either 0 or 1.
;; grid[0][0] == grid[m - 1][n - 1] == 0
(define (shortestPath grid k)
  ;; Define the initial variables: dimensions of the grid, starting point, and visited matrix with k layers
  (let* ([m (length grid)]
         [n (length (first grid))]
         [queue (list (list 0 0 0 k))]  ; queue of (x, y, steps, remaining_k)
         [visited (make-vector m)]  ; Visited states: m x n x (k+1)
         [directions (list (list -1 0) (list 1 0) (list 0 -1) (list 0 1))])  ; Movement directions
    ;; Initialize the visited vector
    (for ([i (in-range m)])
      (vector-set! visited i (make-vector n)))
    (for ([i (in-range m)])
      (for ([j (in-range n)])
        (vector-set! (vector-ref visited i) j (make-vector (+ k 1) #f))))
    ;; BFS algorithm
    (let loop ([queue queue])
      (if (null? queue)  ; If the queue is empty, return -1
          -1
          (let* ([current (car queue)]
                 [x (car current)]
                 [y (cadr current)]
                 [steps (caddr current)]
                 [remaining_k (cadddr current)]
                 [current-visited (vector-ref (vector-ref visited x) y)])
            (cond
             ;; Check if the target is reached
             ((and (= x (- m 1)) (= y (- n 1))) steps)
             ;; Check if this state is already visited
             ((vector-ref current-visited remaining_k) (loop (cdr queue)))
             ;; Mark as visited and explore neighbors
             (else
               (vector-set! current-visited remaining_k #t)
               (define next-queue
                 (foldl (lambda (dir acc)
                          (let* ([dx (car dir)]
                                 [dy (cadr dir)]
                                 [nx (+ x dx)]
                                 [ny (+ y dy)])
                            (if (and (>= nx 0) (< nx m) (>= ny 0) (< ny n))  ; Check boundaries
                                (let ([next-cell (list-ref (list-ref grid nx) ny)])
                                  (cond
                                   ((and (= next-cell 1) (> remaining_k 0))
                                    (append acc (list (list nx ny (+ steps 1) (- remaining_k 1)))))
                                   ((= next-cell 0)
                                    (append acc (list (list nx ny (+ steps 1) remaining_k))))
                                   (else acc)))
                                acc)))
                        (cdr queue) directions))
               (loop next-queue))))))))

;; Example usage
(shortestPath '((0 0 0) (1 1 0) (0 0 0) (0 1 1) (0 0 0)) 1)  ; Output: 6
(require rackunit)

(define (test-humaneval) 

  (let (( candidate shortestPath))
    (check-within (candidate (list (list 0 1 1) (list 1 1 1) (list 1 0 0)) 1) -1 0.001)
    (check-within (candidate (list (list 0 0 0) (list 1 1 0) (list 1 1 0)) 1) 4 0.001)
    (check-within (candidate (list (list 0 0 0) (list 1 1 1) (list 0 0 0)) 2) 4 0.001)
    (check-within (candidate (list (list 0 0 0) (list 1 0 1) (list 1 1 0)) 1) 4 0.001)
    (check-within (candidate (list (list 0 0 0) (list 1 1 1) (list 0 0 0) (list 1 1 1) (list 0 0 0)) 1) -1 0.001)
    (check-within (candidate (list (list 0 1 1 1 1) (list 0 0 0 0 0) (list 1 1 1 1 0)) 2) 6 0.001)
    (check-within (candidate (list (list 0 1 1 1 0) (list 0 0 0 1 0) (list 1 1 0 0 0)) 2) 6 0.001)
))

(test-humaneval)