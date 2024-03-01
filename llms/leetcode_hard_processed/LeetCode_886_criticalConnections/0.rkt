#lang racket

;; There are n servers numbered from 0 to n - 1 connected by undirected server-to-server connections forming a network where connections[i] = [ai, bi] represents a connection between servers ai and bi. Any server can reach other servers directly or indirectly through the network.
;; A critical connection is a connection that, if removed, will make some servers unable to reach some other server.
;; Return all critical connections in the network in any order.
;; Example 1:
;; Input: n = 4, connections = [[0,1],[1,2],[2,0],[1,3]]
;; Output: [[1,3]]
;; Explanation: [[3,1]] is also accepted.
;; Example 2:
;; Input: n = 2, connections = [[0,1]]
;; Output: [[0,1]]
;; Constraints:
;; 2 <= n <= 105
;; n - 1 <= connections.length <= 105
;; 0 <= ai, bi <= n - 1
;; ai != bi
;; There are no repeated connections.
(define (criticalConnections n connections)
  ;; Build a graph from the connections
  (define graph (make-hash))
  (for ([conn connections])
    (for ([i 2])
      (hash-update! graph (list-ref conn i) (lambda (l) (cons (list-ref conn (- 1 i)) l)) '())))
  
  ;; Initialize rank as a list of -1s to denote unvisited nodes
  (define rank (make-vector n -1))
  ;; Result list to store critical connections
  (define result '())

  ;; Depth-first search function to find critical connections
  (define (dfs node parent depth)
    (vector-set! rank node depth)
    (for ([neighbor (hash-ref graph node '())])
      (when (not (= neighbor parent))
        ;; Visit unvisited neighbors
        (when (= (vector-ref rank neighbor) -1)
          (dfs neighbor node (+ depth 1)))
        ;; Update rank of the current node based on the exploration of the neighbor
        (vector-set! rank node (min (vector-ref rank node) (vector-ref rank neighbor)))
        ;; If the only way to reach a neighbor is through the current edge, it's a critical connection
        (when (= (vector-ref rank neighbor) (+ depth 1))
          (set! result (cons (list node neighbor) result)))))
    )

  ;; Start DFS from the first node with an initial depth of 0
  (dfs 0 -1 0)
  result)

;; Example usage:
(criticalConnections 4 '((0 1) (1 2) (2 0) (1 3)))
(require rackunit)

(define (test-humaneval) 

  (let (( candidate criticalConnections))
    (check-within (candidate 4 (list (list 0 1) (list 1 2) (list 2 3) (list 3 0))) (list ) 0.001)
    (check-within (candidate 2 (list (list 0 1))) (list (list 0 1)) 0.001)
    (check-within (candidate 3 (list (list 0 1) (list 1 2) (list 2 0))) (list ) 0.001)
    (check-within (candidate 6 (list (list 0 1) (list 1 2) (list 2 3) (list 3 4) (list 4 5) (list 5 0))) (list ) 0.001)
    (check-within (candidate 6 (list (list 0 1) (list 1 2) (list 2 3) (list 3 4) (list 4 5) (list 5 0) (list 0 3) (list 1 4) (list 3 2))) (list ) 0.001)
    (check-within (candidate 4 (list (list 0 1) (list 1 2) (list 2 0) (list 1 3))) (list (list 1 3)) 0.001)
    (check-within (candidate 1 (list )) (list ) 0.001)
))

(test-humaneval)