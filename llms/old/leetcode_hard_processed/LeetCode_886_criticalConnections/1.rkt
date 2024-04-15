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
  ;; Convert the list of connections into a graph represented as an adjacency list.
  (define graph (make-hash))
  (for-each (lambda (conn)
              (for-each (lambda (i j)
                          (hash-update! graph i (lambda (l) (cons j l)) '()))
                        conn
                        (reverse conn)))
            connections)

  ;; Initialize ranks of all nodes as -1, which will be used to check if a node is visited.
  (define ranks (make-vector n -1))

  ;; Store the result in a list.
  (define result '())

  ;; Define the DFS function to traverse the graph.
  (define (dfs node parent depth)
    (vector-set! ranks node depth)
    (for-each (lambda (neighbor)
                (when (not (= neighbor parent))
                  (when (= (vector-ref ranks neighbor) -1)
                    (dfs neighbor node (+ depth 1)))
                  ;; Update the rank of the node based on the traversal.
                  (vector-set! ranks node (min (vector-ref ranks node) (vector-ref ranks neighbor)))
                  ;; If the condition for a critical connection is met, add it to the result.
                  (when (= (vector-ref ranks neighbor) (+ depth 1))
                    (set! result (cons (list node neighbor) result)))))
              (hash-ref graph node '())))

  ;; Start DFS from the first node with depth 0.
  (dfs 0 -1 0)

  ;; Return the computed list of critical connections.
  result)

;; Example usage:
(criticalConnections 4 '((0 1) (1 2) (2 0) (1 3)))
(criticalConnections 2 '((0 1)))
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