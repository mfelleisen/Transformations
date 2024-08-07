#lang racket

;; Define the function to calculate the probability of the frog being on a certain vertex after t seconds.
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
  ;; Convert the edge list into a graph represented as an adjacency list.
  ;; Using a mutable hash table for easy updates and lookups.
  (define graph (make-hash))
  (for ([edge edges])
    (for ([i (range 2)])
      (hash-update! graph (list-ref edge i)
                    (lambda (v) (cons (list-ref edge (- 1 i)) v))
                    '())))
  
  ;; Define a helper function for depth-first search in the graph.
  ;; node: current vertex, parent: vertex from which the current vertex was reached
  ;; t: remaining time, target: target vertex, graph: graph representation
  (define (dfs node parent t target graph)
    (if (= t 0)
        ;; If time is up, check if the current node is the target.
        (if (= node target) 1.0 0.0)
        ;; Otherwise, explore the neighbors.
        (let ([neighbors (filter (lambda (n) (not (= n parent))) (hash-ref graph node '()))])
          (if (null? neighbors)
              ;; If the frog can't move, it stays in place only if it's at the target.
              (if (= node target) 1.0 0.0)
              ;; Calculate the probability by exploring all neighbors.
              (let ([prob (/ 1 (length neighbors))])
                (foldl (lambda (neighbor acc)
                         (+ acc (* prob (dfs neighbor node (- t 1) target graph))))
                       0
                       neighbors))))))
  
  ;; Start DFS from vertex 1, with no parent (-1), given time, and target.
  (dfs 1 -1 t target graph))

;; Example usage:
(frogPosition 7 '((1 2) (1 3) (1 7) (2 4) (2 6) (3 5)) 2 4)
(frogPosition 7 '((1 2) (1 3) (1 7) (2 4) (2 6) (3 5)) 1 7)
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