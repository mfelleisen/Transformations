#lang racket

;; In this problem, a rooted tree is a directed graph such that, there is exactly one node (the root) for which all other nodes are descendants of this node, plus every node has exactly one parent, except for the root node which has no parents.
;; The given input is a directed graph that started as a rooted tree with n nodes (with distinct values from 1 to n), with one additional directed edge added. The added edge has two different vertices chosen from 1 to n, and was not an edge that already existed.
;; The resulting graph is given as a 2D-list of edges. Each element of edges is a pair [ui, vi] that represents a directed edge connecting nodes ui and vi, where ui is a parent of child vi.
;; Return an edge that can be removed so that the resulting graph is a rooted tree of n nodes. If there are multiple answers, return the answer that occurs last in the given 2D-list.
;; Example 1:
;; Input: edges = [[1,2],[1,3],[2,3]]
;; Output: [2,3]
;; Example 2:
;; Input: edges = [[1,2],[2,3],[3,4],[4,1],[1,5]]
;; Output: [4,1]
;; Constraints:
;; n == edges.length
;; 3 <= n <= 1000
;; edges[i].length == 2
;; 1 <= ui, vi <= n
;; ui != vi
(define (findRedundantDirectedConnection edges)
  ;; Create a parent list initialized with 0, with length of (edges + 1)
  (define parent (make-vector (+ 1 (length edges)) 0))
  (define candidateA '())
  (define candidateB '())

  ;; Loop through each edge to find candidates A and B for the edge to be removed
  (for-each (lambda (edge)
              (define u (first edge))
              (define v (second edge))
              (if (> (vector-ref parent v) 0)
                  (begin
                    (set! candidateA (list (vector-ref parent v) v))
                    (set! candidateB edge))
                  (vector-set! parent v u)))
            edges)

  ;; Check for a cycle in the graph
  (define (find-cycle edge)
    (define cycle (first edge))
    (define steps (length edges))
    (let loop ((cycle cycle) (steps steps))
      (if (and (> (vector-ref parent cycle) 0) (> steps 0))
          (loop (vector-ref parent cycle) (- steps 1))
          steps)))

  ;; Determine if a cycle exists and decide which candidate to return
  (define (check-cycles)
    (for/or ((i (in-range 1 (+ 1 (length edges)))))
            (if (= 0 (find-cycle (list i (vector-ref parent i))))
                (if (null? candidateA)
                    (list (vector-ref parent i) i)
                    candidateA)
                #f)))
  
  ;; Return the result based on cycle checking, or candidate B if no cycles were found
  (or (check-cycles) candidateB))

;; Example usage
(findRedundantDirectedConnection '((1 2) (1 3) (2 3))) ;; Expected output: '(2 3)
(findRedundantDirectedConnection '((1 2) (2 3) (3 4) (4 1) (1 5))) ;; Expected output: '(4 1)
(require rackunit)

(define (test-humaneval) 

  (let (( candidate findRedundantDirectedConnection))
    (check-within (candidate (list (list 1 2) (list 2 3) (list 3 4) (list 4 5) (list 5 6) (list 6 7) (list 7 1))) (list 7 1) 0.001)
    (check-within (candidate (list (list 1 2) (list 1 3) (list 2 3))) (list 2 3) 0.001)
    (check-within (candidate (list (list 1 2) (list 1 3) (list 3 2))) (list 3 2) 0.001)
    (check-within (candidate (list (list 1 2) (list 2 3) (list 3 1) (list 4 1))) (list 3 1) 0.001)
    (check-within (candidate (list (list 1 2) (list 2 3) (list 3 4) (list 4 1) (list 1 5))) (list 4 1) 0.001)
    (check-within (candidate (list (list 1 2) (list 2 3) (list 3 4) (list 4 5) (list 5 6) (list 6 2))) (list 6 2) 0.001)
    (check-within (candidate (list (list 1 2) (list 2 3) (list 3 4) (list 2 4) (list 3 5))) (list 2 4) 0.001)
    (check-within (candidate (list (list 1 2) (list 2 3) (list 3 1))) (list 3 1) 0.001)
    (check-within (candidate (list (list 1 2) (list 2 3) (list 3 4) (list 4 5) (list 5 6) (list 6 4))) (list 6 4) 0.001)
    (check-within (candidate (list (list 1 2) (list 2 3) (list 3 4) (list 4 5) (list 2 5))) (list 2 5) 0.001)
    (check-within (candidate (list (list 1 2) (list 1 3) (list 2 3))) (list 2 3) 0.001)
    (check-within (candidate (list (list 1 2) (list 2 3) (list 3 1))) (list 3 1) 0.001)
    (check-within (candidate (list (list 1 2) (list 2 3) (list 3 4) (list 4 5) (list 1 5))) (list 1 5) 0.001)
    (check-within (candidate (list (list 1 2) (list 2 3) (list 3 4) (list 1 4))) (list 1 4) 0.001)
    (check-within (candidate (list (list 1 2) (list 1 3) (list 3 4) (list 4 2))) (list 4 2) 0.001)
    (check-within (candidate (list (list 1 2) (list 1 3) (list 3 4) (list 4 5) (list 5 6) (list 6 2))) (list 6 2) 0.001)
    (check-within (candidate (list (list 1 2) (list 2 3) (list 3 4) (list 2 1))) (list 2 1) 0.001)
    (check-within (candidate (list (list 1 2) (list 2 3) (list 3 4) (list 4 5) (list 5 1))) (list 5 1) 0.001)
    (check-within (candidate (list (list 1 2) (list 2 3) (list 3 4) (list 4 5) (list 5 6) (list 6 1) (list 1 7))) (list 6 1) 0.001)
    (check-within (candidate (list (list 1 2) (list 2 3) (list 3 4) (list 4 1) (list 1 5))) (list 4 1) 0.001)
))

(test-humaneval)