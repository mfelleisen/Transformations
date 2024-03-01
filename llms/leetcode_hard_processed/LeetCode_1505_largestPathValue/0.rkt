#lang racket

;; Helper function to perform depth-first search (DFS) on the graph
(define (dfs node graph colors cache)
  (let ((color-index (- (char->integer (string-ref colors node)) (char->integer #\a))))
    ;; Check if we haven't computed the value for this node and color
    (when (zero? (vector-ref (vector-ref cache node) color-index))
      ;; Initialize the cache for this node and color
      (vector-set! (vector-ref cache node) color-index 1)
      ;; Recursively visit all neighbors
      (for-each (lambda (neighbor)
                  (let ((neighbor-color-count (+ 1 (dfs neighbor graph colors cache))))
                    (vector-set! (vector-ref cache node) color-index
                                 (max (vector-ref (vector-ref cache node) color-index)
                                      neighbor-color-count))))
                (hash-ref graph node '())))
    ;; Return the computed value for this node and color
    (vector-ref (vector-ref cache node) color-index)))

;; There is a directed graph of n colored nodes and m edges. The nodes are numbered from 0 to n - 1.
;; You are given a string colors where colors[i] is a lowercase English letter representing the color of the ith node in this graph (0-indexed). You are also given a 2D list edges where edges[j] = [aj, bj] indicates that there is a directed edge from node aj to node bj.
;; A valid path in the graph is a sequence of nodes x1 -> x2 -> x3 -> ... -> xk such that there is a directed edge from xi to xi+1 for every 1 <= i < k. The color value of the path is the number of nodes that are colored the most frequently occurring color along that path.
;; Return the largest color value of any valid path in the given graph, or -1 if the graph contains a cycle.
;; Example 1:
;; Input: colors = "abaca", edges = [[0,1],[0,2],[2,3],[3,4]]
;; Output: 3
;; Explanation: The path 0 -> 2 -> 3 -> 4 contains 3 nodes that are colored "a" (red in the above image).
;; Example 2:
;; Input: colors = "a", edges = [[0,0]]
;; Output: -1
;; Explanation: There is a cycle from 0 to 0.
;; Constraints:
;; n == colors.length
;; m == edges.length
;; 1 <= n <= 105
;; 0 <= m <= 105
;; colors consists of lowercase English letters.
;; 0 <= aj, bj < n
(define (largestPathValue colors edges)
  ;; Initial setup
  (define n (string-length colors))
  (define graph (make-hash))
  (define cache (make-vector n))
  (define in-degree (make-vector n 0))
  (define res -1)
  
  ;; Initialize graph and cache
  (for ([i (in-range n)])
    (hash-set! graph i '())
    (vector-set! cache i (make-vector 26 0)))
  
  ;; Build the graph and calculate in-degrees
  (for-each (lambda (edge)
              (define a (first edge))
              (define b (second edge))
              (hash-set! graph a (cons b (hash-ref graph a '())))
              (vector-set! in-degree b (+ 1 (vector-ref in-degree b))))
            edges)
  
  ;; Perform DFS from each node with in-degree 0 to compute the largest color value
  (for ([i (in-range n)])
    (when (zero? (vector-ref in-degree i))
      (set! res (max res (dfs i graph colors cache))))
    )
  
  ;; Return the result
  res)
(require rackunit)

(define (test-humaneval) 

  (let (( candidate largestPathValue))
    (check-within (candidate "aababba" (list (list 4 0) (list 1 0) (list 4 3) (list 1 4) (list 4 2))) 3 0.001)
    (check-within (candidate "aaa" (list (list 1 2) (list 0 2) (list 2 1) (list 2 0) (list 1 0))) -1 0.001)
    (check-within (candidate "aaa" (list (list 0 1) (list 1 2) (list 2 0))) -1 0.001)
    (check-within (candidate "a" (list (list 0 0))) -1 0.001)
    (check-within (candidate "aaaa" (list (list 3 0) (list 0 1) (list 2 3) (list 2 1))) 4 0.001)
    (check-within (candidate "aababbaa" (list (list 4 0) (list 1 0) (list 4 3) (list 1 4) (list 4 2) (list 0 2) (list 1 2))) 4 0.001)
))

(test-humaneval)