#lang racket

;; Helper function to build a graph from the edges
(define (build-graph edges)
  ;; Using a hash table to represent the adjacency list of the graph
  (let ([graph (make-hash)])
    ;; Iterate over each edge to add both directions since it's an undirected graph
    (for ([edge edges])
      (let ([from (first edge)]
            [to (second edge)])
        ;; Adding the edge from -> to
        (hash-update! graph from (lambda (x) (cons to x)) '())
        ;; Adding the edge to -> from
        (hash-update! graph to (lambda (x) (cons from x)) '())))
    graph))

;; Depth-First Search (DFS) function to calculate the probability
(define (dfs node parent t target graph)
  ;; Base case: if time is 0, check if node is the target
  (if (= t 0)
      (if (= node target) 1.0 0.0)  ;; Return 1.0 if node is target, else 0.0
      (let* ([neighbors (hash-ref graph node '())]  ;; Get neighbors of the current node
             [valid-neighbors (filter (lambda (x) (not (= x parent))) neighbors)]  ;; Filter out the parent node
             [num-valid-neighbors (length valid-neighbors)])  ;; Count of valid neighbors
        (if (= num-valid-neighbors 0)
            (if (= node target) 1.0 0.0)  ;; If no valid neighbors and node is target, probability is 1, else 0
            (let ([probability (apply + (map (lambda (neighbor) (dfs neighbor node (- t 1) target graph))
                                              valid-neighbors))])  ;; Sum probabilities from valid neighbors
              (/ probability num-valid-neighbors))))))  ;; Divide by count of valid neighbors for equal distribution

;; Main function to calculate frog's position probability
;; Given an undirected tree consisting of n vertices numbered from 1 to n. A frog starts jumping from vertex 1. In one second, the frog jumps from its current vertex to another unvisited vertex if they are directly connected. The frog can not jump back to a visited vertex. In case the frog can jump to several vertices, it jumps randomly to one of them with the same probability. Otherwise, when the frog can not jump to any unvisited vertex, it jumps forever on the same vertex.
;; The edges of the undirected tree are given in the list edges, where edges[i] = [ai, bi] means that exists an edge connecting the vertices ai and bi.
;; Return the probability that after t seconds the frog is on the vertex target. Answers within 10-5 of the actual answer will be accepted.
;; Example 1:
;; Input: n = 7, edges = [[1,2],[1,3],[1,7],[2,4],[2,6],[3,5]], t = 2, target = 4
;; Output: 0.16666666666666666 
;; Explanation: The figure above shows the given graph. The frog starts at vertex 1, jumping with 1/3 probability to the vertex 2 after second 1 and then jumping with 1/2 probability to vertex 4 after second 2. Thus the probability for the frog is on the vertex 4 after 2 seconds is 1/3 * 1/2 = 1/6 = 0.16666666666666666. 
;; Example 2:
;; Input: n = 7, edges = [[1,2],[1,3],[1,7],[2,4],[2,6],[3,5]], t = 1, target = 7
;; Output: 0.3333333333333333
;; Explanation: The figure above shows the given graph. The frog starts at vertex 1, jumping with 1/3 = 0.3333333333333333 probability to the vertex 7 after second 1. 
;; Constraints:
;; 1 <= n <= 100
;; edges.length == n - 1
;; edges[i].length == 2
;; 1 <= ai, bi <= n
;; 1 <= t <= 50
;; 1 <= target <= n
(define (frogPosition n edges t target)
  (let ([graph (build-graph edges)])  ;; Build the graph from edges
    (dfs 1 -1 t target graph)))  ;; Start DFS from node 1 with parent -1

;; Example usage:
(frogPosition 7 '([1 2] [1 3] [1 7] [2 4] [2 6] [3 5]) 2 4)
;; Output: 0.16666666666666666

(frogPosition 7 '([1 2] [1 3] [1 7] [2 4] [2 6] [3 5]) 1 7)
;; Output: 0.3333333333333333
(require rackunit)

(define (test-humaneval) 

  (let (( candidate frogPosition))
    (check-within (candidate 3 (list (list 2 1) (list 3 2)) 2 3) 1.0 0.001)
    (check-within (candidate 7 (list (list 1 2) (list 1 3) (list 1 7) (list 2 4) (list 2 6) (list 3 5)) 1 7) 0.3333333333333333 0.001)
    (check-within (candidate 3 (list (list 2 1) (list 3 2)) 2 1) 0.0 0.001)
    (check-within (candidate 3 (list (list 2 1) (list 3 2)) 1 2) 1.0 0.001)
    (check-within (candidate 12 (list (list 9 11) (list 4 5) (list 1 2) (list 10 12) (list 3 8) (list 6 9) (list 3 1) (list 3 10) (list 8 11) (list 7 9) (list 7 4) (list 8 6) (list 10 11)) 0 1) 1.0 0.001)
    (check-within (candidate 7 (list (list 1 2) (list 1 3) (list 1 7) (list 2 4) (list 2 6) (list 3 5)) 0 1) 1.0 0.001)
    (check-within (candidate 3 (list (list 2 1) (list 3 2)) 1 3) 0.0 0.001)
))

(test-humaneval)